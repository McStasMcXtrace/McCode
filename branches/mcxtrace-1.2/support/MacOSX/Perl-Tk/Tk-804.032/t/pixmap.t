#!/usr/bin/perl -w
use strict;
BEGIN {
    if (!eval q{
	use Test::More;
	1;
    }) {
	print "1..0 # skip: no Test::More module\n";
	exit;
    }
}
plan tests => 2;

my($icon)=<<'END';
/* XPM */
static char * junk_xpm[] = {
"10 10 6 1",
"       c #000000",
".      c #FFFFFF",
"X      c #B129F8",
"o      c #F869A6",
"O      c #00FF00",
"+      c #1429F8",
" X..oo.+. ",
" .X....+. ",
" ..X...+. ",
" o..X.... ",
" oo..X... ",
" .oo..X.. ",
" ......X. ",
" .+.....X ",
" .+..oo.. ",
" .+...oo. "};
END
use Tk;
my $mw = tkinit;
$mw->geometry("+20+20");
my $label = $mw->Label(-image=>$mw->Pixmap(-data=>$icon))->pack;
pass("Loaded and displayed pixmap");
eval { $mw->Pixmap(-file => "__nonexistingpixmap__") };
like($@, qr{(\QCannot open '__nonexistingpixmap__' in mode 'r'\E
	    |\Qcouldn't read file "__nonexistingpixmap__": No such file or directory\E
	   )}x, "Non-existing xpm error");
$mw->after(500,[destroy => $mw]);
MainLoop;
