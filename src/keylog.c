
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
#include <fcntl.h>
#include <linux/input.h>


#define MAXSYM 0x117

static const char *sym_shift = "S-";
static const char *sym_caps = "<caps_lock>";
static const char *sym_undef = "<UNDEF>";
static const char *sym_mouse4 = "<mouse-4>";
static const char *sym_mouse5 = "<mouse-5>";

struct parms
{
	const char *keyboard;
	const char *mouse;
	const char *symbols;
};

struct state
{
	int kfd;
	int mfd;
	const char *normal[MAXSYM];
	const char *shifted[MAXSYM];
	int ismod[MAXSYM];
	int isdown[MAXSYM];
	int shiftcount;
	int capslock;
	int disable_output;
};

static struct parms parms =
{
	"/dev/input/event2",
	"/dev/input/event3",
	"keymap.txt"
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
	0
};

static void die (const char *msg)
{
	perror(msg);
	exit(-1);
}

static void usage (void)
{
	fprintf(stderr, "\nUsage: keylog [-k keyboard] [-m mouse] [-s symbols]\n\n");
	exit(-1);
}

static void parse_cmdline (int argc, char **argv)
{
	static struct option opts[] =
	{
	{ "keyboard",       required_argument,   0,   'k' },
	{ "mouse",          required_argument,   0,   'm' },
	{ "symbols",        required_argument,   0,   's' },
	};

	char c;

	while ((c = getopt_long(argc, argv, "k:m:s:", opts, NULL)) != -1)
	{
		switch (c)
		{
		case 'k':
			parms.keyboard = optarg;
			break;

		case 'm':
			parms.mouse = optarg;
			break;

		case 's':
			parms.symbols = optarg;
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
	if (s->disable_output)
		return;

	printf("\n");
	fflush(stdout);
}

static void key_up (struct state *s, unsigned short c)
{
	s->isdown[c] = 0;

	if (s->shiftcount)
		if (!strcmp(s->normal[c], sym_shift))
			s->shiftcount--;
}

static void key_down (struct state *s, unsigned short c)
{
	s->isdown[c] = 1;

	if (!strcmp(s->normal[c], sym_shift))
	{
		s->shiftcount++;
		if (s->shiftcount == 2)
		{
			printf(s->disable_output ? "RESUME\n" : "SUSPEND\n");
			fflush(stdout);
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

	if ((fd = open(dev, O_RDONLY)) < 0)
		die("open");

	if (ioctl (fd, EVIOCGNAME(sizeof name), &name) < 0)
		die("ioctl");

	fprintf(stderr, "Monitoring %s \"%s\"", type, name);

	if (ioctl (fd, EVIOCGPHYS(sizeof name), &name) < 0)
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

	if (fstat(fd, &sb) == -1)
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
	uint8_t keys[16];
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
	load_symbols(p,s);
	scan_keyboard(s);
	seteuid(65534);
	setegid(65534);
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

// filter out S- if event_name() will use other symbols
static int want_to_see (struct state *s, unsigned short c, int m)
{
	if (!strcmp(s->normal[c], s->shifted[c])) return 1;
	if (!strcmp(s->normal[m], sym_shift)) return 0;
	return 1;
}

static void show_modifiers (struct state *s, unsigned short c)
{
	int m;
	for (m=1; m<MAXSYM-1; m++)
		if (s->isdown[m])
			if (s->ismod[m])
				if (want_to_see(s,c,m))
					show_key(s,m);
}

static void do_keyboard (struct state *s)
{
	static int repeat = 0;
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
		repeat = -1;
		if (!s->ismod[e.code])
		{
			repeat = 1;
			show_modifiers(s,e.code);
			show_key(s,e.code);
			flush(s);
		}
		break;

	case 2:   // key repeat
		if (repeat > 0)
		{
			repeat++;
			if (!s->disable_output)
			{
				printf("+ %d", repeat);
				flush(s);
			}
		}
		break;

	default:
		die("bad keyboard value");
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
			if (!s->ismod[e.code])
			{
				show_modifiers(s,e.code);
				show_key(s,e.code);
				flush(s);
			}
			break;

		default:
			die("bad mouse key value");
		}
		break;

	case EV_REL:
		if (e.code != REL_WHEEL)
			return;

		switch (e.value)
		{
		case 1:   // wheel up
			if (!s->disable_output)
			{
				show_modifiers(s,1); // hack
				printf("%s",sym_mouse4);
				flush(s);
			}
			break;

		case -1:   // wheel down
			if (!s->disable_output)
			{
				show_modifiers(s,1); // hack
				printf("%s",sym_mouse5);
				flush(s);
			}
			break;

		default:
			die("bad mouse rel value");
		}
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
		int rc;

		FD_ZERO(&rfds);

		FD_SET(s->kfd, &rfds);
		FD_SET(s->mfd, &rfds);

		if ((rc = select(nfds, &rfds, NULL, NULL, NULL)) < 0)
			die("select");

		if (FD_ISSET(s->kfd, &rfds))
			do_keyboard(s);

		if (FD_ISSET(s->mfd, &rfds))
			do_mouse(s);
	}
}

int main (int argc, char **argv)
{
	parse_cmdline(argc, argv);
	prepare_system(&parms, &state);
	process_events(&state);
	return 0;
}
