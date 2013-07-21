
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <getopt.h>
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
};

static struct parms parms =
{
	NULL,
	NULL
};

static struct state state =
{
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
	{
		die("excess cmdline args");
	}
}

static void open_keyboard (const struct parms *p, struct state *s)
{
	int fd;

	if ((fd = open(p->device, O_RDONLY)) < 0)
		die("open");

	s->fd = fd;
}

static const char * event_kind (unsigned int v)
{
	switch (v)
	{
	case 0:  return "U";
	case 1:  return "D";
	case 2:  return "R";
	default: return "?";
	}
}

static void process_events (const struct parms *p, struct state *s)
{
	struct input_event e;

	while (read(s->fd, &e, sizeof(e)) == sizeof(e))
	{
		if (e.type != EV_KEY)
		{
			continue;
		}

		printf("%04x-%s ", e.code, event_kind(e.value));
		fflush(stdout);
	}
}

int main (int argc, char **argv)
{
	parse_cmdline(argc, argv);
	// parse keymap
	open_keyboard(&parms, &state);
	process_events(&parms, &state);
	return 0;
}
