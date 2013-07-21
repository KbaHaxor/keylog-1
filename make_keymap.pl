#!/usr/bin/perl

# expect output of 'dumpkeys -f' on stdin

my %replace =
(
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
	'three' => '3',
	'two' => '2',
	'underscore' => '_',
	'zero' => '0',
);

sub clean
{
	my ($sym) = @_;
	$sym =~ s/^\+//;
	$sym = $replace{$sym} if exists $replace{$sym};
	$sym;
}

sub ismod
{
	my ($sym) = @_;
	my %mods = map { $_ => 1 }
	(
		'Control',
		'Shift',
		'Alt',
	);
	(exists $mods{$sym}) ? 'Y' : 'N';
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

