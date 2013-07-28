include compile_opts.inc

bin_PROGRAMS = keylog

keylog_SOURCES = keylog.c

keymap.txt:
	sudo dumpkeys -f | ./make_keymap.pl > keymap.txt

x:
	sudo chown root keylog
	sudo chmod 4755 keylog
