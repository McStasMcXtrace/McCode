#!/usr/local/bin/perl -w

#
# Figure 15.5, p153
#

use Tk;

my $mw = MainWindow->new;
my $top = $mw->Button(-text => 'Top button');
$top->pack;
my $bottom = $mw->Button(-text => 'Bottom button');
$bottom->pack;

MainLoop;


