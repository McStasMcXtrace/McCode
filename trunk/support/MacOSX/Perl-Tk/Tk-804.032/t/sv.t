#!/usr/bin/perl -w
# -*- cperl -*-

#
# Author: Slaven Rezic
#

use strict;
use Test::More 'no_plan';
use Tk;

my $mw = tkinit;
$mw->geometry('+0+0');

{
    # a couple of test cases related to RT #90077
    {
	my $val = 0.20;
	{ no warnings 'void'; $val * 2 }
	is $val * 1, 0.20;

	{
	    my $e = $mw->Entry(-textvariable => \$val)->pack;
	    $e->destroy;
	}
	# This fails with perl < 5.18
	is $val * 1, 0.20 or
	    do { require Devel::Peek; Devel::Peek::Dump($val) }; # pv_dump says: FLAGS = (PADMY,IOK,NOK,POK,pIOK,pNOK,pPOK,UTF8)
    }

    {
	my $val = 0.20;
	{ no warnings 'void'; $val * 2 }
	is $val * 1, 0.20;

	{
	    my $e = $mw->Entry(-textvariable => \$val)->pack;
	    { no warnings 'void'; $val * 2 } # set the pIOK flag again
	    $e->destroy;
	}
	# This fails also with perl 5.18
	is $val * 1, 0.20 or
	    do { require Devel::Peek; Devel::Peek::Dump($val) };
    }
}

__END__
