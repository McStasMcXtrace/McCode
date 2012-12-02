#!/usr/bin/perl -w
# -*- perl -*-

use strict;

use Tk;

BEGIN {
    if (!eval q{
	use Test::More;
	1;
    }) {
	print "1..0 # skip: no Test::More module\n";
	exit;
    }
}

plan tests => 8;

use_ok("Tk::ColorEditor");

my $mw = tkinit;
$mw->geometry("+10+10");

for (1..2) {
    my $c = $mw->ColorSelect->pack;
    isa_ok($c, "Tk::ColorSelect");
    my $lb = $c->Subwidget("Names");
 SKIP: {
	if (!$lb) {
	    my @rgbTxtPath = $c->_rgbTxtPath;
	    my $found = grep { -e $_ } @rgbTxtPath;
	    # This is expexted on MSWin32 systems, and even not all
	    # X11 systems install rgb.txt (seen on FreeBSD, where the
	    # file is part of the x11/rgb package, which is not
	    # mandatory for xorg)
	    skip("No rgb.txt found on this system", 2)
		if !$found;
	}

	isa_ok($lb, "Tk::Listbox");
	# This used to fail until Tk804.027_501:
	cmp_ok(scalar @{ $lb->get(0,"end") }, ">=", 10, "Some colors found in listbox");
    }
    $c->destroy;
}

$mw->after(500, sub { $mw->destroy });
$mw->chooseColor;
pass("chooseColor destroyed");


__END__
