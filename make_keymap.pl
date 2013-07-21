#!/usr/bin/perl

# expect output of 'dumpkeys -f' on stdin

# discard keymaps line
die unless <> =~ m/^keymaps \d+-\d+$/;

# extract normal and shifted columns
while (my $line = <>)
{
	my @line = split(/\s+/, $line);
	exit unless $line[0] eq "keycode";
	print sprintf("%04x\t%s\t%s\n", $line[1], $line[3], $line[4]);
}

