#!/usr/local/bin/perl -w

#
# Figure 16.2, p161
#

use Tk;

my $bitmaps = Tk->findINC("demos/images");
my $mw = MainWindow->new;
$mw->Label(-bitmap => "\@$ {bitmaps}/flagdown")->pack;
$mw->Label(-text => 'No new mail')->pack;

MainLoop;
