#!/usr/bin/perl -w
# -*- perl -*-

#
# $Id: $
# Author: Slaven Rezic
#

use strict;

use Tk;
use Tk::PNG;
use FindBin;

BEGIN {
    if (!eval q{
	use Test::More;
	1;
    }) {
	print "1..0 # skip: no Test::More module\n";
	exit;
    }
}

plan tests => 1;

my $mw = tkinit;
$mw->geometry('+10+10');

$mw->packPropagate(0);
my $file = "$FindBin::RealBin/../pngtest.png";
my $img = $mw->Photo(-file => $file);
$mw->Label(-image => $img)->pack;
$mw->afterIdle
    (sub {
	 $mw->GeometryRequest($img->width, $img->height);
	 $mw->after(100,
		    sub {
			$mw->GeometryRequest($img->width-20, $img->height-20);
			$mw->after(100,
				   sub {
				       $mw->destroy;
				   }
				  );
		    }
		   );
     });
MainLoop;

pass("No crash while resizing a window with PNG image");

__END__
