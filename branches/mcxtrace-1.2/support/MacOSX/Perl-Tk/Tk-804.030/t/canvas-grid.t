#!/usr/bin/perl -w
# -*- perl -*-

#
# Author: Slaven Rezic
#

use strict;

use Getopt::Long;
use Tk;

BEGIN {
    if (!eval q{
	use Test::More;
	1;
    }) {
	print "1..0 # skip no Test::More module\n";
	exit;
    }
}

my $mw = eval { tkinit };
if (!$mw) {
    plan skip_all => 'Cannot create MainWindow';
    CORE::exit(0);
}

plan tests => 1;

my $show;
GetOptions("show!" => \$show)
    or die "usage: $0 [-show]";

$mw->geometry('+0+0');
my $c = $mw->Canvas->pack;
$c->createGrid(0,0,20,20, -color=>'blue'); # as documented
$c->createGrid(10,10,30,30, -fill=>'red'); # as implemented for a long time
pass 'createGrid done';

if (!$show) {
    $mw->after(200, sub { $mw->destroy });
}
MainLoop;

__END__
