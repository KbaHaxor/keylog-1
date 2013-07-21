include Makefile.am.inc

bin_PROGRAMS = keylog

keylog_SOURCES = keylog.c

keymap.txt:
	sudo dumpkeys -f | ./make_keymap.pl > keymap.txt

