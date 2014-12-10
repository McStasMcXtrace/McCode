#!/usr/bin/perl -w
# -*- perl -*-

#
# $Id: $
# Author: Slaven Rezic
#

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

plan tests => 4;

if (!defined $ENV{BATCH}) { $ENV{BATCH} = 1 }

use_ok("Tk::Adjuster");

my $mw = tkinit;
$mw->geometry("+10+10");

{
    my $f = $mw->Frame->pack(qw(-fill both -expand 1));

    my $adj = $f->Adjuster;
    isa_ok($adj, "Tk::Adjuster");

    $f->Label(-text => "top", -bg => "red")->pack;
    $adj->pack(-fill => "x");
    $f->Label(-text => "bottom", -bg => "blue")->pack;

    pass("Adjuster packed");
}

{
    my $f = $mw->Frame->pack(qw(-fill both -expand 1));
    $f->Label(-text => "top")->packAdjust;
    $f->Label(-text => "bottom")->pack;

    pass("packAdjust");
}

if ($ENV{BATCH}) {
    $mw->after(300, sub { $mw->destroy });
}
MainLoop;

__END__
