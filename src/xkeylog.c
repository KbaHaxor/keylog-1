/*
 * xkeylog.c
 * Copyright (c) 2014 Ray Lehtiniemi
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
#include <unistd.h>
#include <string.h>
#include <getopt.h>
#include <ctype.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <fcntl.h>
#include <X11/Xlib.h>
#include <X11/Xproto.h>
#include <X11/extensions/record.h>


#define MAXSYM 262

static const char *sym_shift = "S-";
static const char *sym_caps = "<caps_lock>";
static const char *sym_undef = "<UNDEF>";

struct parms
{
	const char *display;
	const char *symbols;
	int timestamps;
};

struct state
{
	Display * ctl;
	Display * dat;
	XRecordContext ctx;
	const char *normal[MAXSYM];
	const char *shifted[MAXSYM];
	int ismod[MAXSYM];
	int isdown[MAXSYM];
	int lastkey;
	int lastcount;
	int shiftcount;
	int capslock;
	int disable_output;
	int timestamps;
};

static struct parms parms =
{
	":0",
	"xkeymap.txt",
	0
};

static struct state state =
{
	NULL,
	NULL,
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
	fprintf(stderr, "\nUsage: xkeylog [-d display] [-s keymap.txt] [-t]\n\n");
	exit(-1);
}

static void parse_cmdline (int argc, char **argv)
{
	static struct option opts[] =
	{
	{ "display",        required_argument,   0,   'd' },
	{ "symbols",        required_argument,   0,   's' },
	{ "timestamps",     required_argument,   0,   't' },
	};

	char c;

	while ((c = getopt_long(argc, argv, "d:s:t", opts, NULL)) != -1)
	{
		switch (c)
		{
		case 'd':
			parms.display = optarg;
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

#if 0

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

#endif

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

static void x_record_cb (XPointer p, XRecordInterceptData* i)
{
	if (i->category == XRecordFromServer)
	{
		struct state * s = (struct state *) p;
		xEvent * e = (xEvent *) i->data;
		BYTE t = e->u.u.type;
		BYTE d = e->u.u.detail;
		// KeyButMask m = e->u.keyButtonPointer.state;
		switch (t)
		{
		case KeyPress:
			key_down(s,d);
			if (!s->ismod[d])
				show_press(s,d);
			break;
		case KeyRelease:
			key_up(s,d);
			break;
		case ButtonPress:
			key_down(s,d+256);
			show_press(s,d+256);
			break;
		case ButtonRelease:
			key_up(s,d+256);
			break;
		default:
			break;
		}
	}

	XRecordFreeData(i);
}

static void x_record_setup (struct state * s)
{
	XRecordClientSpec c = XRecordAllClients;
        int f = XRecordFromServerTime;
	XRecordRange* x;
	int v;
	int r;

	XSynchronize(s->ctl, True);

	if (!XRecordQueryVersion(s->ctl, &v, &r))
		die("XRecordQueryVersion");

	fprintf(stderr,"XRecord %d.%d\n", v, r);

	if ((x = XRecordAllocRange()) == NULL)
		die("XRecordAllocRange");

	x->device_events.first = KeyPress;
	x->device_events.last = ButtonRelease;

	if ((s->ctx = XRecordCreateContext(s->ctl, f, &c, 1, &x, 1)) == 0)
		die("XRecordCreateContext");
}

static Display * x_connect (const char * display)
{
	Display * d = XOpenDisplay(display);

	if (d == NULL)
		die("XOpenDisplay");

	return d;
}

static void prepare_system (const struct parms *p, struct state *s)
{
	s->ctl = x_connect(p->display);
	s->dat = x_connect(p->display);
	x_record_setup(s);
	load_symbols(p,s);
	//scan_keyboard(s);
	s->timestamps = p->timestamps;
}

static void process_events (struct state *s)
{
	if (!XRecordEnableContext(s->dat, s->ctx, &x_record_cb, (XPointer) s))
		die("XRecordEnableContext");
}

int main (int argc, char **argv)
{
	parse_cmdline(argc, argv);
	prepare_system(&parms, &state);
	process_events(&state);
	return 0;
}
