#!/usr/bin/perl

# make_keymap.pl
# Copyright (c) 2013 Ray Lehtiniemi
# 
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.


# expect output of 'dumpkeys -f' on stdin

my %replace =
(
	'Delete' => 'del',
	'Escape' => 'esc',
	'Insert' => 'ins',
	'Return' => 'cr',
	'VoidSymbol' => 'void',
	'ampersand' => '&',
	'apostrophe' => '\'',
	'asciicircum' => '^',
	'asciitilde' => '~',
	'asterisk' => '*',
	'at' => '@',
	'backslash' => '\\',
	'bar' => '|',
	'braceleft' => '{',
	'braceright' => '}',
	'bracketleft' => '[',
	'bracketright' => ']',
	'colon' => ':',
	'comma' => ',',
	'dollar' => '$',
	'eight' => '8',
	'equal' => '=',
	'exclam' => '!',
	'five' => '5',
	'four' => '4',
	'grave' => '`',
	'greater' => '>',
	'less' => '<',
	'minus' => '-',
	'nine' => '9',
	'numbersign' => '#',
	'one' => '1',
	'parenleft' => '(',
	'parenright' => ')',
	'percent' => '%',
	'period' => '.',
	'plus' => '+',
	'question' => '?',
	'quotedbl' => '"',
	'semicolon' => ';',
	'seven' => '7',
	'six' => '6',
	'slash' => '/',
	'space' => 'spc',
	'three' => '3',
	'two' => '2',
	'underscore' => '_',
	'zero' => '0',
);

my %mark = map { $_ => 1 }
qw(
	Break
	cr
	del
	Do
	Down
	esc
	Find
	Help
	ins
	KP_0
	KP_1
	KP_2
	KP_3
	KP_4
	KP_5
	KP_6
	KP_7
	KP_8
	KP_9
	KP_Add
	KP_Divide
	KP_Enter
	KP_MinPlus
	KP_Multiply
	KP_Period
	KP_Subtract
	Last_Console
	Left
	Macro
	Next
	Num_Lock
	Pause
	Prior
	Remove
	Right
	Scroll_Backward
	Scroll_Forward
	Scroll_Lock
	Select
	Show_Memory
	spc
	Tab
	Up
	void
);

my %mod = map { $_ => uc substr($_,0,1) . "-"}
qw(
	Control
	Alt
	Shift
);

sub clean
{
	my ($sym) = @_;
	$sym =~ s/^\+//;
	$sym = $replace{$sym} if exists $replace{$sym};
	$sym = "<" . lc $sym . ">" if exists $mark{$sym};
	$sym = $mod{$sym} if exists $mod{$sym};
	$sym;
}

sub ismod
{
	my ($sym) = @_;
	(exists $mod{$sym}) ? 'Y' : 'N';
}

# discard keymaps line
die unless <> =~ m/^keymaps \d+-\d+$/;

# extract normal and shifted columns
while (my $line = <>)
{
	my @line = split(/\s+/, $line);
	exit unless $line[0] eq "keycode";
	print sprintf("%d\t%s\t%s\t%s\n", $line[1], clean($line[3]), clean($line[4]), ismod($line[3]));
}

