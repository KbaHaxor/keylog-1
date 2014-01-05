include compile_opts.inc

bin_PROGRAMS = keylog xkeylog

keylog_SOURCES = keylog.c

xkeylog_SOURCES = xkeylog.c
xkeylog_LDADD = -lX11 -lXtst

keymap.txt:
	sudo dumpkeys -f | ./make_keymap.pl > keymap.txt

xkeymap.txt:
	xmodmap -pke | ./make_xkeymap.pl > xkeymap.txt

