#!/usr/bin/perl -w
# -*- perl -*-


use strict;
use FindBin;
use lib "$FindBin::RealBin";

use Tk;
use Test::More;

use TkTest qw(wm_info);

# Win32 gets one <visibility> event on toplevel and one on content (as expected)
# UNIX/X is more complex, as windows overlap (deliberately)
our $tests = 6;
our $expect = 0;
plan 'no_plan'; # $tests for fast connections, $tests-1 for slow connections

my $mw = new MainWindow;

my %wm_info = wm_info($mw);
my $wm_name = $wm_info{name};

my $initial_ok_delay = 0.4;
# GNOME Shell is sometimes slow
my $ok_delay = $wm_name eq 'GNOME Shell' ? 1.0 : 0.5;

my $event = '<Map>';
my $why;
my $start;
my $skip_slow_connection;

sub begin
{
 $start = Tk::timeofday();
 $why = shift;
 $expect = shift;
 diag "Start $why $expect";
}

# First setup timers to kill the script in case of timeouts
$mw->after(5*1000, sub { diag "This test script takes longer than usual... it will maybe be killed in some seconds." });
$mw->after(30*1000, sub { diag "Killing main window."; $mw->destroy });

my $l = $mw->Label(-text => 'Content')->pack;
#$l->bind($event,[\&mapped,"update"]);
$mw->bind($event,[\&mapped,"initial"]);
$mw->geometry("+0+0");
begin('update',2);
$mw->update;

local $TODO = "Ignore test results because of slow connection" if $skip_slow_connection;

my $t = $mw->Toplevel(-width => 100, -height => 100);
$t->geometry("-0+0");
my $l2 = $t->Label(-text => 'Content')->pack;
$t->bind($event,[\&mapped,"Popup"]);
#$l2->bind($event,[\&mapped,"Popup"]);
begin('Popup',2);
$t->Popup(-popover => $mw);
$t->update;
begin('withdraw',0);
$t->withdraw;
begin('Popup Again',2);
$t->Popup(-popover => $mw);

$mw->after(500, sub { begin('destroy',0); $mw->destroy });

MainLoop;


sub mapped
{
 my ($w, $state) = @_;
 my $now = Tk::timeofday();
 my $delay = $now - $start;
 diag sprintf "%s $why %.3g $expect\n",$w->PathName,$delay;
 if ($state eq 'initial' && $delay > $initial_ok_delay)
  {
   $skip_slow_connection = 1;
   return;
  }
 if ($expect-- > 0)
  {
   cmp_ok($delay, "<", $ok_delay, $why);
  }
}





