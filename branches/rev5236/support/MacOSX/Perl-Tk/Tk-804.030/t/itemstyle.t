#!/usr/bin/perl -w
# -*- perl -*-

#
# Author: Slaven Rezic
#

use strict;

BEGIN {
    if (!eval q{
	use Test::More;
	1;
    }) {
	print "1..0 # skip no Test::More module\n";
	exit;
    }
}

plan tests => 3;

use Tk;
use Tk::HList;
use Tk::ItemStyle;

my $mw = Tk::MainWindow->new;
eval { $mw->geometry('+10+10'); };  # This works for mwm and interactivePlacement

my $hl = $mw->HList(-columns => 2);
my $is = $mw->ItemStyle('text', -anchor => 'nw');
isa_ok $is, 'Tk::ItemStyle';

$hl->headerCreate(0, -style => $is);
pass 'Setting itemstyle worked';

eval { $hl->headerCreate(1, -style => 'invalid_style') };
like $@, qr{Display style "invalid_style" not found}, 'Invalid ItemStyle used';

__END__
