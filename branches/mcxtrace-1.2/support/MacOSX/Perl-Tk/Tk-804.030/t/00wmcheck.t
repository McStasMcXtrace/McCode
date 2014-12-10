#!/usr/bin/perl -w
# -*- perl -*-

#
# $Id: $
# Author: Slaven Rezic
#

use strict;
use FindBin;
use lib $FindBin::RealBin;

use Tk;
use Tk::Config ();

use TkTest qw(wm_info);

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

my $mw = MainWindow->new;
$mw->withdraw;

my @diag = ("",
	    "Tk platform:    $Tk::platform",
	   );

my $server = eval { $mw->server }; # not sure if this works on non-X11 systems
if ($server) {
    push @diag, "server info:    $server";
}

SKIP: {
    skip("window manager check only on X11", 1)
	if $Tk::platform ne "unix";

    my %wm_info = wm_info($mw);

    push @diag, ("window manager: $wm_info{name}",
		 "       version: $wm_info{version}",
		);

    pass("window manager check done");
}

my $Xft = $Tk::Config::xlib =~ /-lXft\b/ ? "yes" : "no";
push @diag, ("XFT:            $Xft");

diag join("\n", @diag);

__END__
