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

plan tests => 3;

my $mw = tkinit;
$mw->geometry("+10+10");

my $b = $mw->Button->pack;
like(join("\n", $b->bindDump), qr{Binding tag.*Tk::Button.*has these bindings}, "bindDump with Button");

my $c = $mw->Canvas->pack;
like(join("\n", $c->bindDump), qr{Binding tag.*Tk::Canvas.*has these bindings}, "bindDump with Canvas");

my $l = $mw->Label->pack;
like(join("\n", $l->bindDump), qr{Binding tag.*Tk::Label.*has no bindings}, "bindDump with Label");

__END__
