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

plan tests => 8;

use_ok("Tk::ROText");

my $mw = tkinit;

{
    my $ro = $mw->ROText;
    isa_ok($ro, "Tk::ROText");
    isa_ok($ro, "Tk::Text");
    $ro->destroy;
}

{
    my $ro  = $mw->ROText;
    my $txt = $mw->Text;
    is($ro->cget(-background), $txt->cget(-background), "Text and ROText have same background color");
    is($ro->cget(-foreground), $txt->cget(-foreground), "Text and ROText have same foreground color");
}

{
    my $ro = $mw->ROText(-background => "red", -foreground => "green");
    is($ro->cget(-background), "red", "Setting -background worked");
    is($ro->cget(-foreground), "green", "Setting -foreground worked");
}

$mw->optionAdd("*background", "yellow");

{
    my $ro = $mw->ROText;
    is($ro->cget(-background), "yellow", "Setting -background via optionDB worked");
}

__END__
