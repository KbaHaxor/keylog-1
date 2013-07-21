
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <getopt.h>
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
	const char *ismod[256];
	int isdown[256];
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
	{ NULL },
	{ 0 }
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

static void prepare_system (const struct parms *p, struct state *s)
{
	struct stat sb;
	void *map;
	int fd;

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

	char *tok = strtok((char*)map, "\t");

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
		case 'N':
			break;
		default:
			die("ismod");
		}

		s->ismod[code] = tok;

		tok = strtok(NULL,"\t");
	}
}

static const char * event_name (unsigned short c)
{
	return state.normal[c];
}

static void show_modifiers (struct state *s)
{
	int i;
	for (i=1; i<255; i++)
		if (s->isdown[i])
			if (*s->ismod[i] == 'Y')
				printf("%s", event_name(i));
}

static void show_key (unsigned short c)
{
	printf("%s ", event_name(c));
}

static void process_events (const struct parms *p, struct state *s)
{
	struct input_event e;

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
			s->isdown[e.code] = 0;
			break;

		case 1:   // key down
			s->isdown[e.code] = 1;
			if (*s->ismod[e.code] == 'N')
			{
				show_modifiers(s);
				show_key(e.code);
				fflush(stdout);
			}
			break;

		case 2:   // key repeat
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
