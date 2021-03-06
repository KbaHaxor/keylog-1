/*
 * keylog.c
 * Copyright (c) 2013 Ray Lehtiniemi
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <unistd.h>
#include <string.h>
#include <getopt.h>
#include <ctype.h>
#include <sys/mman.h>
#include <sys/types.h>
#include <sys/select.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <fcntl.h>
#include <linux/input.h>


#define MAXSYM 0x117

static const char *sym_shift = "S-";
static const char *sym_caps = "<caps_lock>";
static const char *sym_undef = "<UNDEF>";

static const unsigned short sym_mouse4 = 275;
static const unsigned short sym_mouse5 = 276;

struct parms
{
	const char *keyboard;
	const char *mouse;
	const char *symbols;
	int timestamps;
};

struct state
{
	int kfd;
	int mfd;
	const char *normal[MAXSYM];
	const char *shifted[MAXSYM];
	int ismod[MAXSYM];
	int isdown[MAXSYM];
	int repeat;
	int lastkey;
	int lastcount;
	int shiftcount;
	int capslock;
	int disable_output;
	int timestamps;
};

static struct parms parms =
{
	"event2",
	"event3",
	"keymap.txt",
	0
};

static struct state state =
{
	0,
	0,
	{ NULL },
	{ NULL },
	{ 0 },
	{ 0 },
	0,
	0,
	0,
	0,
	0,
	0,
	0
};

#define die(m) die_(__LINE__,(m))

static void die_ (int line, const char *msg)
{
	fprintf(stderr, "At line %d: ", line);
	perror(msg);
	exit(-1);
}

static void usage (void)
{
	fprintf(stderr, "\nUsage: sudo keylog [-k eventX] [-m eventY] [-s keymap.txt] [-t]\n\n");
	exit(-1);
}

static uid_t ruid, euid;
static gid_t rgid, egid;

static int sudo_env (const char *e)
{
	int n;

	if ((e = getenv(e)) == NULL)
		die("getenv");

	if (sscanf(e, "%d", &n) != 1)
		die("sscanf");

	return n;
}

static void init_priv (void)
{
	ruid = (uid_t) sudo_env("SUDO_UID");
	euid = geteuid();
	rgid = (gid_t) sudo_env("SUDO_GID");
	egid = getegid();
}

static void show_priv (const char *x)
{
	if (0)
	{
		fprintf(stderr, "%s:\n", x);
		fprintf(stderr, "  U: e:%5d  <r:%5d>\n", geteuid(), getuid());
		fprintf(stderr, "  G: e:%5d  <r:%5d>\n", getegid(), getgid());
	}
}

static void take_priv (void)
{
	if (setreuid(ruid,euid) < 0)
		die("setreuid");

	if (setregid(rgid,egid) < 0)
		die("setregid");

	show_priv("take");
}

static void drop_priv (void)
{
	if (setregid(egid,rgid) < 0)
		die("setregid");

	if (setreuid(euid,ruid) < 0)
		die("setreuid");

	show_priv("drop");
}

static void kill_priv (void)
{
	if (setregid(rgid,rgid) < 0)
		die("setregid");

	if (setreuid(ruid,ruid) < 0)
		die("setreuid");

	show_priv("kill");
}

// make sure p is of the form "eventN" for integer N
static const char * check (const char * p)
{
	const char *q;

	if (strncmp(p,"event",5) != 0)
		die("strncmp");

	for (q=p+5; *q; q++)
		if (!isdigit(*q))
			die("isdigit");

	return p;
}

static void parse_cmdline (int argc, char **argv)
{
	static struct option opts[] =
	{
	{ "keyboard",       required_argument,   0,   'k' },
	{ "mouse",          required_argument,   0,   'm' },
	{ "symbols",        required_argument,   0,   's' },
	{ "timestamps",     required_argument,   0,   't' },
	};

	char c;

	while ((c = getopt_long(argc, argv, "k:m:s:t", opts, NULL)) != -1)
	{
		switch (c)
		{
		case 'k':
			parms.keyboard = check(optarg);
			break;

		case 'm':
			parms.mouse = check(optarg);
			break;

		case 's':
			parms.symbols = optarg;
			break;

		case 't':
			parms.timestamps = 1;
			break;

		default:
			usage();
		}
	}

	if (optind < argc)
		die("excess cmdline args");
}

static void flush (struct state *s)
{
	if (!s->disable_output)
	{
		printf("\n");

		if (fflush(stdout) != 0)
			die("fflush");
	}
}

static void key_up (struct state *s, unsigned short c)
{
	s->isdown[c] = 0;

	if (c != s->lastkey)
	{
		s->lastkey = 0;
		s->lastcount = 0;
	}

	if (s->shiftcount)
		if (!strcmp(s->normal[c], sym_shift))
			s->shiftcount--;
}

static void key_down (struct state *s, unsigned short c)
{
	s->isdown[c] = 1;

	if (s->ismod[c])
	{
		s->lastkey = 0;
		s->lastcount = 0;
	}
	else if (c == s->lastkey)
	{
		s->lastcount++;
	}
	else
	{
		s->lastkey = c;
		s->lastcount = 0;
	}

	if (!strcmp(s->normal[c], sym_shift))
	{
		s->shiftcount++;
		if (s->shiftcount == 2)
		{
			printf(s->disable_output ? "RESUME\n" : "SUSPEND\n");

			if (fflush(stdout) != 0)
				die("fflush");

			s->disable_output ^= 1;
		}
	}

	if (!strcmp(s->normal[c], sym_caps))
		s->capslock ^= 1;
}

static int monitor (const char *type, const char *dev)
{
	char name[1024];
	int fd;

	if (snprintf(name, sizeof(name), "/dev/input/%s", dev) < 0)
		die("snprintf");

	take_priv();

	if ((fd = open(name, O_RDONLY)) < 0)
		die("open");

	drop_priv();

	memset(name, 0, sizeof name);

	if (ioctl (fd, EVIOCGNAME(sizeof name - 1), &name) < 0)
		die("ioctl");

	fprintf(stderr, "Monitoring %s \"%s\"", type, name);

	memset(name, 0, sizeof name);

	if (ioctl (fd, EVIOCGPHYS(sizeof name - 1), &name) < 0)
		die("ioctl");

	fprintf(stderr, " on %s\n", name);

	return fd;
}

static void load_symbols (const struct parms *p, struct state *s)
{
	struct stat sb;
	char *map;
	char *tok;
	int fd;
	int i;

	if ((fd = open(p->symbols, O_RDWR)) < 0)
		die("open");

	if (fstat(fd, &sb) < 0)
		die("fstat");

	if ((map = (char *)mmap(NULL,sb.st_size,PROT_READ|PROT_WRITE,MAP_PRIVATE,fd,0)) == MAP_FAILED)
		die("mmap");

	if (close(fd) < 0)
		die("close");

	map[sb.st_size-1] = '\0';

	for (i=0; i<MAXSYM; i++)
	{
		s->normal[i] = sym_undef;
		s->shifted[i] = sym_undef;
	}

	if ((tok = strtok((char*)map, "\t")) == NULL)
		die("strtok");

	while (tok != NULL)
	{
		unsigned short code;

		if (sscanf(tok, "%hd", &code) != 1)
			die("sscanf");

		if (code > MAXSYM-1)
			die("code");

		if ((tok = strtok(NULL,"\t")) == NULL)
			die("strtok");

		s->normal[code] = tok;

		if ((tok = strtok(NULL,"\t")) == NULL)
			die("strtok");

		s->shifted[code] = tok;

		if ((tok = strtok(NULL,"\n")) == NULL)
			die("strtok");

		switch (*tok)
		{
		case 'Y':
			s->ismod[code] = 1;
			break;

		case 'N':
			s->ismod[code] = 0;
			break;
		default:
			fprintf(stderr,"%d\n", code);
			die("ismod");
		}

		tok = strtok(NULL,"\t");
	}
}

static void scan_keyboard (struct state *s)
{
	unsigned long leds;
	uint8_t keys[MAXSYM / 8 + 1];
	int i,j;

	if (ioctl (s->kfd, EVIOCGKEY(sizeof keys), &keys) < 0)
		die("ioctl");

	for (i = 0; i < sizeof keys; i++)
		for (j = 0; j < 8; j++)
			if (keys[i] & (1 << j))
				key_down(s,(i*8) + j);

	if (ioctl (s->kfd, EVIOCGLED(sizeof leds), &leds) < 0)
		die("ioctl");

	if (leds & (1 << LED_CAPSL))
		s->capslock = 1;
}

static void prepare_system (const struct parms *p, struct state *s)
{
	s->kfd = monitor("keyboard", p->keyboard);
	s->mfd = monitor("mouse", p->mouse);
	kill_priv();
	load_symbols(p,s);
	scan_keyboard(s);
	s->timestamps = p->timestamps;
}

static const char * event_name (struct state *s, unsigned short c)
{
	int shift = 0;

	if (s->capslock)
		if (isalpha(*s->normal[c]))
			shift |= 1;

	if (s->shiftcount)
		shift ^= 1;

	return (shift ? s->shifted : s->normal)[c];
}

static void show_key (struct state *s, unsigned short c)
{
	if (!s->disable_output)
		printf("%s", event_name(s,c));
}

static void show_repeat (struct state *s)
{
	if (!s->disable_output)
		printf("+%d", s->repeat);
}

// filter out S- if event_name() will use other symbols
static int want_to_see (struct state *s, unsigned short c, int m)
{
	if (!strcmp(s->normal[c], s->shifted[c]))
		return 1;

	if (!strcmp(s->normal[m], sym_shift))
		return 0;

	return 1;
}

static void show_modifiers (struct state *s, unsigned short c)
{
	int m;

	for (m=1; m<MAXSYM; m++)
		if (s->isdown[m])
			if (s->ismod[m])
				if (want_to_see(s,c,m))
					show_key(s,m);
}

static void show_last (struct state *s)
{
	if (!s->disable_output)
		printf("*%d", s->lastcount);
}

static void show_timestamp (struct state *s)
{
	struct timeval tv;

	if (!s->timestamps)
		return;

	if (gettimeofday(&tv,NULL) < 0)
		die("gettimeofday");

	printf("\t%ld.%06ld", tv.tv_sec, tv.tv_usec);
}

static void show_press (struct state *s, unsigned short c)
{
	if (s->lastcount > 1)
	{
		show_last(s);
	}
	else
	{
		show_modifiers(s,c);
		show_key(s,c);
	}
	show_timestamp(s);
	flush(s);
}


static void do_keyboard (struct state *s)
{
	struct input_event e;

	if (read(s->kfd, &e, sizeof(e)) != sizeof(e))
		die("read keyboard");

	if (e.type != EV_KEY)
		return;

	if (e.code > 255)
	{
		fprintf(stderr,"IGNORING keyboard code %d\n", e.code);
		return;
	}

	switch (e.value)
	{
	case 0:   // key up
		key_up(s,e.code);
		break;

	case 1:   // key down
		key_down(s,e.code);
		if (s->ismod[e.code])
		{
			s->repeat = -1;
		}
		else
		{
			s->repeat = 1;
			show_press(s,e.code);
		}
		break;

	case 2:   // key repeat
		if (s->repeat > 0)
		{
			if (s->lastkey != 0)
			{
				s->lastkey = 0;
				s->lastcount = 0;
			}
			show_repeat(s);
			show_timestamp(s);
			flush(s);
			s->repeat++;
		}
		break;

	default:
		fprintf(stderr, "Bad EV_KEY value %d\n", e.value);
		die("value");
	}
}

static void show_rel (struct state *s, unsigned short c, int n)
{
	if (n > 3)
	{
		fprintf(stderr, "%s: reported %d, limiting to 3...\n", event_name(s,c), n);
		n = 3;
	}

	while (n--)
	{
		key_down(s,c);
		show_press(s,c);
		key_up(s,c);
	}
}

static void do_mouse (struct state *s)
{
	struct input_event e;

	if (read(s->mfd, &e, sizeof(e)) != sizeof(e))
		die("read mouse");

	//fprintf(stderr, "Got mouse %04x %04x %08x\n", e.type, e.code, e.value);

	switch (e.type)
	{
	case EV_KEY:

		if ((e.code < 0x110) || (e.code > 0x117))
		{
			fprintf(stderr,"IGNORING mouse key %d\n", e.code);
			return;
		}

		switch (e.value)
		{
		case 0:   // key up
			key_up(s,e.code);
			break;

		case 1:   // key down
			key_down(s,e.code);
			show_press(s,e.code);
			break;

		default:
			fprintf(stderr, "Bad EV_KEY value %d\n", e.value);
			die("value");
		}
		break;

	case EV_REL:
		if (e.code != REL_WHEEL)
			return;

		if (e.value > 0)
			show_rel(s, sym_mouse4, e.value);
		else if (e.value < 0)
			show_rel(s, sym_mouse5, -e.value);
		else
			fprintf(stderr, "EV_REL zero, ignoring...\n");

		break;

	default:
		return;
	}
}

static void process_events (struct state *s)
{
	int nfds = 1 + ((s->kfd > s->mfd) ? s->kfd : s->mfd);

	while (1)
	{
		fd_set rfds;

		FD_ZERO(&rfds);

		FD_SET(s->kfd, &rfds);
		FD_SET(s->mfd, &rfds);

		if (select(nfds, &rfds, NULL, NULL, NULL) < 0)
			die("select");

		if (FD_ISSET(s->kfd, &rfds))
			do_keyboard(s);

		if (FD_ISSET(s->mfd, &rfds))
			do_mouse(s);
	}
}

int main (int argc, char **argv)
{
	init_priv();
	drop_priv();
	parse_cmdline(argc, argv);
	prepare_system(&parms, &state);
	process_events(&state);
	return 0;
}
