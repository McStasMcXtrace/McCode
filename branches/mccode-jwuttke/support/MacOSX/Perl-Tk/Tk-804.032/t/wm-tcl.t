#!/usr/bin/perl -w
# -*- perl -*-

# This file is a Tcl script to test out Tk's interactions with
# the window manager, including the "wm" command.  It is organized
# in the standard fashion for Tcl tests.
#
# Copyright (c) 1992-1994 The Regents of the University of California.
# Copyright (c) 1994-1997 Sun Microsystems, Inc.
# Copyright (c) 1998-1999 by Scriptics Corporation.
# All rights reserved.
#
# RCS: @(#) $Id$

# This file tests window manager interactions that work across
# platforms. Window manager tests that only work on a specific
# platform should be placed in unixWm.test or winWm.test.

#
# Translated by Slaven Rezic (2006-11, from CVS version 1.36)
#

# Some tests are marked as TODO because they fail with
# some window managers.
#
# Window managers passing all tests: 
# * fvwm 2.4.19
# * twm
# * windowmaker 0.92.0
#
# Window managers with test failures
# * metacity 2.16.3
# * metacity 2.10.3 (even more failures)
# * fvwm 2.5.18
# * fvwm 2.6.5
# * blackbox 0.70.1
# * KWin: 3.0
# * Xfwm4: 4.2.3.2
# * X11 on Mac OS X 10.5.1
# * fluxbox 1.0.0

use strict;
use FindBin;
use lib $FindBin::RealBin;

use Tk;
use Getopt::Long;

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

plan tests => 315;

my $mw = MainWindow->new;
my %wm_info = wm_info($mw);
my $wm_name = $wm_info{name};

my $wm_problems = $Tk::platform eq 'unix';
my $kwin_problems = defined $wm_name && $wm_name eq 'KWin';
my $xfwm4_problems = defined $wm_name && $wm_name eq 'Xfwm4';
my $macosx_x11_problems = $Tk::platform eq 'unix' && $^O eq 'darwin';
my $fluxbox_problems = defined $wm_name && $wm_name eq 'Fluxbox';
my $fvwm_problems = defined $wm_name && $wm_name eq 'FVWM';

my $poswin = 1;
my $netwm = 0;
GetOptions("poswin!" => \$poswin,
	   "trace!"  => sub { $mw->wmTracing(1) },
	   "netwm!"  => \$netwm,
	   "nowmproblems" => sub { $wm_problems = 0 },
	  )
    or die "usage: $0 [-poswin] [-trace] [-netwm]
-noposwin: turn off fixed geometry setting (fixed geometry setting is needed
           for some window managers like twm with default manual positioning)
-trace:    turns wmTracing on
-netwm:    set this is using modern opendesktop compliant X11 window manager
-nowmproblems: set this if you believe your X11 window manager implements
               all ICCCM specifications correctly
";


$mw->geometry("+10+10");

# Create entries in the option database to be sure that geometry options
# like border width have predictable values.
$mw->optionAdd('*Toplevel.borderWidth', 0);

$mw->deiconify;
if (!$mw->ismapped) {
    $mw->waitVisibility;
}

# Weak guess whether a window manager is running at all (only done on
# X11)
my $wm_running = 1;
if ($Tk::platform eq 'unix') {
    $mw->iconify;
    $wm_running = $mw->state eq 'iconic';
    $mw->deiconify;
}

my $t;

sub stdWindow () {
    $t->destroy if Tk::Exists($t);
    $t = $mw->Toplevel(qw(-width 100 -height 50));
    $t->geometry("+0+0");
    $t->update
}

sub deleteWindows () {
    eval { $_->destroy } for $mw->children;
}

# [raise] and [lower] may return before the window manager
# has completed the operation.  The raiseDelay procedure
# idles for a while to give the operation a chance to complete.
#

sub raiseDelay () {
    $mw->after(100);
    $mw->update;
}

sub raiseDelayLonger () {
    $mw->after(2000);
    $mw->update;
}

sub poswin ($;@) {
    if ($poswin) {
	for (@_) {
	    $_->geometry("+0+0");
	}
    }
}

deleteWindows;
stdWindow;

{
    my $b = $mw->Button(-text => "hello");
    eval { Tk::Wm::geometry($b) }; # one shouldn't do this anyway
    like($@, qr{window ".button" isn't a top-level window},
	 q{Tk_WmObjCmd procedure, miscellaneous errors});
    $b->destroy;
}

### wm aspect ###
{
    eval { $mw->aspect("_") };
    like($@, qr{\Qwrong # args: should be "wm aspect window ?minNumer minDenom maxNumer maxDenom?"\E},
	 "wm aspect usage");

    eval { $mw->aspect("_", "_", "_") };
    like($@, qr{\Qwrong # args: should be "wm aspect window ?minNumer minDenom maxNumer maxDenom?"\E});

    eval { $mw->aspect("_", "_", "_", "_", "_") };
    like($@, qr{\Qwrong # args: should be "wm aspect window ?minNumer minDenom maxNumer maxDenom?"\E});

    eval { $mw->aspect(qw(bad 14 15 16)) };
    like($@, qr{'bad' isn't numeric});

    eval { $mw->aspect(qw(13 foo 15 16)) };
    like($@, qr{'foo' isn't numeric});

    eval { $mw->aspect(qw(13 14 bar 16)) };
    like($@, qr{'bar' isn't numeric});

    eval { $mw->aspect(qw(13 14 15 baz)) };
    like($@, qr{'baz' isn't numeric});

    eval { $mw->aspect(qw(0 14 15 16)) };
    like($@, qr{\Qaspect number can't be <= 0});

    eval { $mw->aspect(qw(13 0 15 16)) };
    like($@, qr{\Qaspect number can't be <= 0});

    eval { $mw->aspect(qw(13 14 0 16)) };
    like($@, qr{\Qaspect number can't be <= 0});

    eval { $mw->aspect(qw(13 14 15 0)) };
    like($@, qr{\Qaspect number can't be <= 0});
}

{
    is_deeply([$mw->aspect], [], "setting and reading aspect values");
    $mw->aspect(qw(3 4 10 2));
    is_deeply([$mw->aspect], [qw(3 4 10 2)]);
    $mw->aspect(undef,undef,undef,undef);
    is_deeply([$mw->aspect],[]);
}

### wm attributes ###
{
    eval { $mw->attributes(-alpha => 1.0, '-disabled') };
    if ($Tk::platform eq 'MSWin32') {
	local $TODO = "-alpha and -fullscreen not yet implemented";
	like($@, qr{\Qwrong # args: should be "wm attributes window ?-alpha ?double?? ?-disabled ?bool?? ?-fullscreen ?bool?? ?-toolwindow ?bool?? ?-topmost ?bool??"});
    } else {
	like($@, qr{\Qwrong # args: should be "wm attributes window ?-attribute ?value ...??});
    }

 SKIP: {
	skip("works only on windows", 1)
	    if $Tk::platform ne 'MSWin32';
	local $TODO = "Still fails...";
	eval { $mw->attributes('-to') };
	like($@, qr{\Qwrong # args: should be "wm attributes window ?-alpha ?double?? ?-disabled ?bool?? ?-fullscreen ?bool?? ?-toolwindow ?bool?? ?-topmost ?bool??"});
    }
    
 SKIP: {
	skip("works only on unix", 1)
	    if $Tk::platform ne 'unix';
	eval { $mw->attributes("_") };
	like($@, qr{\Qbad attribute "_": must be -alpha, -topmost, -zoomed, or -fullscreen},
	     "wm attributes usage");
    }

 SKIP: {
	skip("works only on aqua", 1)
	    if $Tk::platform ne 'aqua';
	die <<EOF;
not yet translated:
test wm-attributes-1.2.5 {usage} aqua {
    list [catch {wm attributes . _} err] \$err
} {1 {bad attribute "_": must be -alpha, -modified, -notify, or -titlepath}}
EOF
    }
}

{
    ### wm client ###
    is($t->client, undef, "wm client, setting and reading values");
    $t->client('Miffo');
    is($t->client, 'Miffo');
    $t->client(undef);
    is($t->client, undef);
}

SKIP: {
    skip("fullscreen tests only on windows", 38)
 	if $Tk::platform ne 'MSWin32';
    skip("fullscreen tests NYI on windows", 38);
## Getting fullscreen attribute is not yet implemented for X11
#     skip("fullscreen tests only on windows or on X11 with option -netwm", 38)
# 	if !($Tk::platform eq 'MSWin32' || ($Tk::platform eq 'unix' && $netwm));

    {
	deleteWindows;
	my $t = $mw->Toplevel;
	is($t->attributes(-fullscreen), 0,
	   "default -fullscreen value");
    }

    {
	deleteWindows;
	my $t = $mw->Toplevel;
	$t->attributes(-fullscreen => 1);
	is($t->attributes(-fullscreen), 1,
	   q{change -fullscreen before map});
    }

    {
	deleteWindows;
	my $t = $mw->Toplevel;
	poswin $t;
	$t->attributes(-fullscreen => 1);
	$mw->update;
	is($t->attributes(-fullscreen), 1,
	   q{change -fullscreen before map});
    }

    {
	deleteWindows;
	my $t = $mw->Toplevel;
	poswin $t;
	$mw->update;
	$t->attributes(-fullscreen => 1);
	is($t->attributes(-fullscreen), 1,
	   q{change -fullscreen after map});
    }

    {
	deleteWindows;
	my $t = $mw->Toplevel;
	poswin $t;
	$mw->update;
	is($t->attributes(-fullscreen), 0,
	   q{change -fullscreen after map});
	$t->attributes(-fullscreen => 1);
	is($t->attributes(-fullscreen), 1);
	# Query above should not clear fullscreen state
	is($t->attributes(-fullscreen), 1);
	$t->attributes(-fullscreen => 0);
	is($t->attributes(-fullscreen), 0);
    }

    {
	deleteWindows;
	my $t = $mw->Toplevel;
	my $normal_geom = "301x302+101+102";
	my $fullscreen_geom = $mw->screenwidth . "x" . $mw->screenheight . "+0+0";
	$t->geometry($normal_geom);
	$mw->update;
	is($t->geometry, $normal_geom, q{change -fullscreen after map});
	$t->attributes(-fullscreen => 1);
	is($t->geometry, $fullscreen_geom);
	$t->attributes(-fullscreen => 0);
	is($t->geometry, $normal_geom);
    }

    {
	deleteWindows;
	my $t = $mw->Toplevel;
	poswin $t;
	$mw->update;
	$t->attributes(-fullscreen => 1);
	$t->withdraw;
	$t->deiconify;
	is($t->attributes(-fullscreen), 1,
	   q{state change does not change -fullscreen});
    }

    {
	deleteWindows;
	my $t = $mw->Toplevel;
	$mw->update;
	$t->attributes(-fullscreen => 1);
	$t->iconify;
	$t->deiconify;
	is($t->attributes(-fullscreen), 1,
	   q{state change (iconify) does not change -fullscreen});
    }

    {
	deleteWindows;
	my $t = $mw->Toplevel(Name => "t");
	poswin $t;
	$mw->update;
	$t->overrideredirect(1);
	eval { $t->attributes(-fullscreen => 1) };
	like($@, qr{\Qcan't set fullscreen attribute for ".t": override-redirect flag is set},
	     q{override-redirect not compatible with fullscreen attribute});
    }

    {
	deleteWindows;
	my $t = $mw->Toplevel(Name => "t");
	poswin $t;
	$mw->update;
	$t->maxsize(5000, 450);
	eval { $t->attributes(-fullscreen => 1) };
	like($@, qr{\Qcan't set fullscreen attribute for ".t": max width/height is too small},
	     q{max height too small});
    }

    {
	deleteWindows;
	my $t = $mw->Toplevel(Name => "t");
	poswin $t;
	$mw->update;
	$t->maxsize(450, 5000);
	eval { $t->attributes(-fullscreen => 1) };
	like($@, qr{\Qcan't set fullscreen attribute for ".t": max width/height is too small},
	     q{max width too small});
    }

    {
	deleteWindows;
	my $t = $mw->Toplevel;
	poswin $t;
	$mw->update;
	$t->attributes(-alpha => 1.0, -fullscreen => 1);
	is($t->attributes(-fullscreen), 1,
	   q{another attribute, then -fullscreen});
    }

    {
	deleteWindows;
	my $t = $mw->Toplevel;
	poswin $t;
	$mw->update;
	# This was originally -toolwindow instead of -alpha; changed
	# this to make the test runnable under X11
	$t->attributes(-alpha => 0.1, -fullscreen => 1, -topmost => 0);
	is($t->attributes(-fullscreen), 1,
	   q{another attribute, then -fullscreen, then another});
    }

    {
	deleteWindows;
	$mw->focusForce;
	my $t = $mw->Toplevel;
	poswin $t;
	$t->lower;
	$mw->update;
	is($mw->focus, $mw,
	   q{setting/unsetting fullscreen does not change the focus});

	my $done;
	$t->attributes(-fullscreen => 1);
	$mw->after(200, sub { $done = 1 });
	$mw->waitVariable(\$done);
	is($mw->focus, $mw);

	$done = 0;
	$t->attributes(-fullscreen => 0);
	$mw->after(200, sub { $done = 1 });
	$mw->waitVariable(\$done);
	is($mw->focus, $mw);
    }

    {
	deleteWindows;
	my(@focusin, $done);
	$mw->focusForce;
	my $t = $mw->Toplevel(Name => "t");
	poswin $t;
	my $te = $t->Entry(Name => "e")->pack;
	$t->lower;
	$t->bind("<FocusIn>", [sub {push @focusin, $_[0]}, Ev('W')]);
	$mw->after(200, sub { $done = 1 });
	$mw->waitVariable(\$done);

	push @focusin, 1;
	$te->focusForce;
	$mw->after(200, sub { $done = 2 });
	$mw->waitVariable(\$done);

	push @focusin, 2;
	$t->attributes(-fullscreen => 1);
	$mw->after(200, sub { $done = 3 });
	$mw->waitVariable(\$done);

	push @focusin, 3;
	$t->attributes(-fullscreen => 0);
	$mw->after(200, sub { $done = 4 });
	$mw->waitVariable(\$done);

	push @focusin, "final";

	$mw->bind("<FocusIn>" => '');
	$t->bind("<FocusIn>" => '');
	
	is_deeply(\@focusin, [1, $t, $te, 2, 3, "final", $te],
		  q{setting fullscreen does not generate FocusIn on wrapper create});
    }
	
    {
	deleteWindows;
	my $t = $mw->Toplevel(Name => "t");
	is_deeply([$mw->stackorder], ["."],
		  "fullscreen stackorder");
	my $done;
	$mw->after(200, sub { $done = 1 });
	$mw->waitVariable(\$done);
	is_deeply([$mw->stackorder], [".", ".t"]);

	# Default stacking is on top of other windows
	# on the display. Setting the fullscreen attribute
	# does not change this.
	$t->attributes(qw(-fullscreen 1));
	$mw->after(200, sub { $done = 2 });
	$mw->waitVariable(\$done);
	is_deeply([$mw->stackorder], [".", ".t"]);
    }

    {
	deleteWindows;
	my $t = $mw->Toplevel(Name => "t");
	poswin $t;
	$t->lower;
	my $done;
	$mw->after(200, sub { $done = 1 });
	$mw->waitVariable(\$done);
	is_deeply([$mw->stackorder], [".t", "."],
		  q{fullscreen stackorder});
    
	# If stacking order is explicitly set, then
	# setting the fullscreen attribute should
	# not change it.
	$t->attributes(-fullscreen => 1);
	$mw->after(200, sub { $done = 2 });
	$mw->waitVariable(\$done);
	is_deeply([$mw->stackorder], [".t", "."]);
    }

    {
	deleteWindows;
	my $t = $mw->Toplevel(Name => "t");
	poswin $t;
	# lower forces the window to be mapped, it would not be otherwise
	$t->lower;
	is_deeply([$mw->stackorder], [".t", "."]);

	# If stacking order is explicitly set
	# for an unmapped window, then setting
	# the fullscreen attribute should
	# not change it.
	$t->attributes(qw(-fullscreen 1));
	my $done;
	$mw->after(200 => sub { $done = 1 });
	$mw->waitVariable(\$done);
	is_deeply([$mw->stackorder], [".t", "."]);
    }

    {
	deleteWindows;
	my $t = $mw->Toplevel(Name => "t");
	my $done;
	$mw->after(200, sub { $done = 1});
	$mw->waitVariable(\$done);
	is_deeply([$mw->stackorder], [".", ".t"],
		  q{fullscreen stackorder});

	$t->attributes(qw(-fullscreen 1));
	$mw->after(200, sub { $done = 2});
	$mw->waitVariable(\$done);
	is_deeply([$mw->stackorder], [".", ".t"]);

	# Unsetting the fullscreen attribute
	# should not change the stackorder.
	$t->attributes(qw(-fullscreen 0));
	$mw->after(200, sub { $done = 3 });
	$mw->waitVariable(\$done);
	is_deeply([$mw->stackorder], [".", ".t"]);
    }

    {
	deleteWindows;
	my $t = $mw->Toplevel(Name => "t");
	poswin $t;
	$t->lower;
	my $done;
	$mw->after(200, sub { $done = 1 });
	$mw->waitVariable(\$done);
	is_deeply([$mw->stackorder], [".t", "."]);

	$t->attributes(qw(-fullscreen 1));
	$mw->after(200, sub { $done = 2 });
	$mw->waitVariable(\$done);
	is_deeply([$mw->stackorder], [".t", "."]);

	# Unsetting the fullscreen attribute
	# should not change the stackorder.
	$t->attributes(qw(-fullscreen 0));
	$mw->after(200, sub { $done = 3});
	$mw->waitVariable(\$done);
	is_deeply([$mw->stackorder], [".t", "."]);
    }

    {
	deleteWindows;
	my $a = $mw->Toplevel(Name => "a");
	my $b = $mw->Toplevel(Name => "b");
	my $c = $mw->Toplevel(Name => "c");
	poswin $a, $b, $c;
	$a->raise;
	$b->raise;
	$c->raise;
	my $done;
	$mw->after(200, sub { $done = 1});
	$mw->waitVariable(\$done);
	is_deeply([$mw->stackorder], [".", ".a", ".b", ".c"]);

	$b->attributes(-fullscreen => 1);
	$mw->after(200, sub { $done = 2});
	$mw->waitVariable(\$done);
	is_deeply([$mw->stackorder], [".", ".a", ".b", ".c"]);

	# Unsetting the fullscreen attribute
	# should not change the stackorder.
	$b->attributes(qw(-fullscreen 0));
	$mw->after(200, sub { $done = 3});
	$mw->waitVariable(\$done);
	is_deeply([$mw->stackorder], [".", ".a", ".b", ".c"]);
    }
}

deleteWindows;
stdWindow;

{
    ### wm colormapwindows ###
    eval { $mw->colormapwindows("_","_") };
    like($@, qr{\Qwrong # args: should be "wm colormapwindows window ?windowList?"},
	 "wm colormapwindows usage");

    eval { $mw->colormapwindows("foo") };
    like($@, qr{bad window path name "foo"});
}

{
    my $t1 = $mw->Toplevel(qw(Name toplevel1 -width 200 -height 200 -colormap new));
    poswin $t1;
    my $t1a = $t1->Frame(qw(-width 100 -height 30));
    my $t1b = $t1->Frame(qw(-width 100 -height 30 -colormap new));
    Tk::pack($t1a, $t1b, qw(-side top));
    $mw->update;

    is_deeply([$t1->colormapwindows], [".toplevel1.frame1", ".toplevel1"],
	      "wm colormapwindows reading values");

    my $t1c = $t1->Frame(qw(-width 100 -height 30 -colormap new))->pack(-side => "top");
    $mw->update;

    {
	local $TODO;
	$TODO = "May fail on KDE" if !$TODO && $kwin_problems;
	$TODO = "May fail on xfwm4" if !$TODO && $xfwm4_problems;
	$TODO = "May fail on MacOSX" if !$TODO && $macosx_x11_problems;
	$TODO = "May fail on fluxbox" if !$TODO && $fluxbox_problems;
	is_deeply([$t1->colormapwindows], [".toplevel1.frame1", ".toplevel1.frame2", ".toplevel1"]);
    }

    $t1->destroy;
}

{
    my $t1 = $mw->Toplevel(qw(Name toplevel2 -width 200 -height 200));
    poswin $t1;
    my @f;
    for (1 .. 3) {
	push @f, $t1->Frame(qw(-width 100 -height 30))->pack(-side => "top");
    }
    $t1->colormapwindows([$f[1], $f[0]]);
    is_deeply([$t1->colormapwindows], [".toplevel2.frame1", ".toplevel2.frame"],
	      "wm colormapwindows, setting and reading values");
}

{
    ### wm command ###
    eval { $mw->command("_", "_") };
    like($@, qr{\Qwrong # args: should be "wm command window ?value?"},
	 "wm command usage");

    is_deeply([$t->command],[], "wm command, setting and reading values");
    {
	local $TODO;
	$TODO = "Fails on windows" if $Tk::platform eq 'MSWin32';
	$t->command([qw(Miffo Foo)]);
	is_deeply([$t->command],[qw(Miffo Foo)]);
    }
    $t->command(undef);
    is_deeply([$t->command],[]);
}

{
    ### wm deiconify ###
    my $icon = $mw->Toplevel(qw(Name icon -width 50 -height 50 -bg red));
    $t->iconwindow($icon);
    eval { $icon->deiconify };
    like($@, qr{can't deiconify .icon: it is an icon for .t}, "wm deiconify");
    $icon->destroy;
}

{
    if ($Tk::platform eq 'MSWin32') {
	# test embedded window for Windows
	my $tf = $t->Frame(-container => 1);
	my $embed = $mw->Toplevel(Name => "embed", -use => $tf->id);
	eval { $embed->deiconify };
	like($@, qr{\Qcan't deiconify .embed: \E(the container does not support the request|it is an embedded window)},
	     "wm deiconify embedded window");
	$embed->destroy;
	$tf->destroy;
    } else {
	my $tf = $t->Frame(-container => 1);
	my $embed = $mw->Toplevel(Name => "embed", -use => $tf->id);
	eval { $embed->deiconify };
	like($@, qr{\Qcan't deiconify .embed: it is an embedded window},
	     "wm deiconify embedded window");
	$embed->destroy;
	$tf->destroy;
    }
}

{
    deleteWindows;
    $t = $mw->Toplevel;
    $t->deiconify;
    ok(!$t->ismapped,
       q{a window that has never been mapped should not be mapped by deiconify()});
}

{
    deleteWindows;
    $t = $mw->Toplevel;
    poswin $t;
    $mw->idletasks;
    $t->withdraw;
    $t->deiconify;
    if ($fvwm_problems && !$t->ismapped) { $t->deiconify }
    ok($t->ismapped,
       q{a window that has already been mapped should be mapped by deiconify()});
}

{
    deleteWindows;
    $t = $mw->Toplevel(qw(-width 200 -height 200));
    is($t->geometry, "1x1+0+0");
    $t->deiconify;
    is($t->geometry, "1x1+0+0",
       q{geometry for an unmapped window should not be calculated by deiconify()});
    poswin $t;
    $mw->idletasks;
    like($t->geometry, qr{^200x200},
	 q{... it should be done at idle time});
}

{
    deleteWindows;
    $t = $mw->Toplevel;
    $t->withdraw;
    $t->deiconify;
    $t->destroy;
    $mw->update;
    pass(q{invoking destroy after a deiconify should not result in a crash});
    # ... because of a callback set on the toplevel
}

{
    ### wm focusmodel ###
    eval { $mw->focusmodel("_", "_") };
    like($@, qr{\Qwrong # args: should be "wm focusmodel window ?active|passive?"},
	 "wm focusmodel usage");

    eval { $mw->focusmodel("bogus") };
    like($@, qr{\Qbad argument "bogus": must be active, or passive});
}

stdWindow;

{
    is($t->focusmodel, "passive",
       "wm focusmodel, setting and reading values");
    $t->focusmodel("active");
    is($t->focusmodel, "active");
    $t->focusmodel("passive");
    is($t->focusmodel, "passive");
}

{
    ### wm frame ###
    ok(defined $mw->frame, "wm frame");
}

{
    ### wm geometry ###
    eval { $mw->geometry("_", "_") };
    like($@, qr{\Qwrong # args: should be "wm geometry window ?newGeometry?"},
	 "wm geometry usage");

    eval { $mw->geometry("bogus") };
    like($@, qr{\Qbad geometry specifier "bogus"});
}

{
    local $TODO;
    $TODO = "May fail on KDE" if !$TODO && $kwin_problems;
    $TODO = "May fail on some window managers (e.g. fvwm 2.4.x)" if !$TODO && $wm_problems;

    $t->geometry("150x150+50+50");
    $t->update;
    is($t->geometry, "150x150+50+50",
       "wm geometry, setting and getting values");
    $t->geometry(undef);
    $t->update;
    isnt($t->geometry, "150x150+50+50", "geometry should change and is now " . $t->geometry);
}

{
    ### wm grid ###
    for my $args (1, 3, 5) {
	eval { $mw->wmGrid(("_")x$args) };
	like($@, qr{\Qwrong # args: should be "wm grid window ?baseWidth baseHeight widthInc heightInc?"},
	     "wm grid usage (tried $args args)");
    }

    eval { $mw->wmGrid(qw(bad 14 16 16)) };
    like($@, qr{'bad' isn't numeric});

    eval { $mw->wmGrid(qw(13 foo 16 16)) };
    like($@, qr{'foo' isn't numeric});

    eval { $mw->wmGrid(qw(13 14 bar 16)) };
    like($@, qr{'bar' isn't numeric});

    eval { $mw->wmGrid(qw(13 14 15 baz)) };
    like($@, qr{'baz' isn't numeric});

    eval { $mw->wmGrid(qw(-1 14 15 16)) };
    like($@, qr{baseWidth can't be < 0});

    eval { $mw->wmGrid(qw(13 -1 15 16)) };
    like($@, qr{baseHeight can't be < 0});

    eval { $mw->wmGrid(qw(13 14 -1 16)) };
    like($@, qr{widthInc can't be <= 0});

    eval { $mw->wmGrid(qw(13 14 15 -1)) };
    like($@, qr{heightInc can't be <= 0});

    is_deeply([$t->wmGrid],[], "wm grid, setting and reading values");
    $t->wmGrid(qw(3 4 10 2));
    is_deeply([$t->wmGrid],[qw(3 4 10 2)]);
    $t->wmGrid((undef)x4);
    is_deeply([$t->wmGrid],[]);
}

{
    ### wm group ###
    eval { $mw->group(12, 13) };
    like($@, qr{\Qwrong # args: should be "wm group window ?pathName?"},
	 q{wm group usage});

    eval { $mw->group("bogus") };
    like($@, qr{bad window path name "bogus"});

    is($t->group, undef, "wm group, setting and reading values");
    $t->group($mw);
    is($t->group, ".");
    $t->group(undef);
    is($t->group, undef);
}

{
    ### wm iconbitmap ###
 SKIP: {
	skip("test only for unix", 1)
	    if $Tk::platform ne "unix";
	eval { $mw->iconbitmap(12, 13) };
	like($@, qr{\Qwrong # args: should be "wm iconbitmap window ?bitmap?"},
	     "wm iconbitmap usage on unix");
    }

 SKIP: {
	skip("test only for windows", 2)
	    if $Tk::platform ne "MSWin32";

	eval { $mw->iconbitmap(12, 13, 14) };
	like($@, qr{\Qwrong # args: should be "wm iconbitmap window ?-default? ?image?"},
	     "wm iconbitmap usage on windows");

	eval { $mw->iconbitmap(12, 13) };
	like($@, qr{\Qillegal option "12" must be "-default"});
    }

    eval { $mw->iconbitmap("bad-bitmap") };
    like($@, qr{bitmap "bad-bitmap" not defined});

    is($t->iconbitmap, undef, "wm iconbitmap, setting and reading values");
    $t->iconbitmap("hourglass");
    is($t->iconbitmap, "hourglass");
    $t->iconbitmap(undef);
    is($t->iconbitmap, undef);
}

{
    ### wm iconify ###
    my $t2 = $mw->Toplevel(qw(Name toplevel2));
    $t2->geometry("+10+10");
    $t2->overrideredirect(1);
    eval { $t2->iconify };
    like($@, qr{\Qcan't iconify ".toplevel2": override-redirect flag is set},
	 "wm iconify, misc errors");
    $t2->destroy;
}

{
    my $t2 = $mw->Toplevel(qw(Name toplevel2));
    poswin $t2;
    $t2->transient($t);
    eval { $t2->iconify };
    like($@, qr{\Qcan't iconify ".toplevel2": it is a transient});
    $t2->destroy;
}

{
    my $t2 = $mw->Toplevel(qw(Name toplevel2));
    poswin $t2;
    $t->iconwindow($t2);
    eval { $t2->iconify };
    like($@, qr{can't iconify .toplevel2: it is an icon for .toplevel});
    $t2->destroy;
}

{
    if ($Tk::platform eq 'MSWin32') {
	# test embedded window for Windows
	my $tf = $t->Frame(qw(Name f -container 1));
	my $t2 = $mw->Toplevel(qw(Name toplevel2), -use => $tf->id);
	eval { $t2->iconify };
	like($@, qr{\Qcan't iconify .toplevel2: \E(the container does not support the request|it is an embedded window)});
	$t2->destroy;
    } else {
	# test embedded window for other platforms
	my $tf = $t->Frame(qw(Name f -container 1));
	my $t2 = $mw->Toplevel(qw(Name toplevel2), -use => $tf->id);
	eval { $t2->iconify };
	like($@, qr{\Qcan't iconify .toplevel2: it is an embedded window});
	$t2->destroy;
    }
}

SKIP: {
    skip("Needs a window manager", 2)
	if !$wm_running;

    my $t2 = $mw->Toplevel;
    $t2->geometry("-0+0");
    $mw->update;
    ok($t2->ismapped);
    $t2->iconify;
    $mw->update;
    ok(!$t2->ismapped);
}

{
    ### wm iconmask ###
    eval { $mw->iconmask(12, 13) };
    like($@, qr{\Qwrong # args: should be "wm iconmask window ?bitmap?"},
	 q{wm iconmask usage});

    eval { $mw->iconmask("bad-bitmap") };
    like($@, qr{\Qbitmap "bad-bitmap" not defined});

    is($t->iconmask, undef, "wm iconmask, setting and reading values");
    $t->iconmask("hourglass");
    is($t->iconmask, "hourglass");
    $t->iconmask(undef);
    is($t->iconmask, undef);
}

{
    ### wm iconname ###
    eval { $mw->iconname(12, 13) };
    like($@, qr{\Qwrong # args: should be "wm iconname window ?newName?"},
	 q{wm iconname usage});

    # This is somewhat inconsistent ('' vs. undef)
    is($t->iconname, '', "wm iconname, setting and reading values");
    $t->iconname("ThisIconHasAName");
    is($t->iconname, "ThisIconHasAName");
    $t->iconname(undef);
    is($t->iconname, '');
}

SKIP: {
    skip("iconphoto not implemented on Windows", 4)
	if $Tk::platform eq 'MSWin32';

    ### wm iconphoto ###
    eval { $mw->iconphoto };
    like($@, qr{\Qwrong # args: should be "wm iconphoto window ?-default? image1 ?image2 ...?"},
	 "wm iconphoto usage");

    eval { $mw->iconphoto("notanimage") };
    like($@, qr{\Qcan't use "notanimage" as iconphoto: not a photo image});

    eval { $mw->iconphoto("-default") };
    like($@, qr{\Qwrong # args: should be "wm iconphoto window ?-default? image1 ?image2 ...?"});

    my $photo = $mw->Photo(-file => Tk->findINC("icon.gif"));
    $mw->iconphoto($photo);
    pass("Set iconphoto");

    # All other iconphoto tests are platform specific
}

{
    ### wm iconposition ###
    eval { $mw->iconposition(12) };
    like($@, qr{\Qwrong # args: should be "wm iconposition window ?x y?"},
	 "wm iconposition usage");

    eval { $mw->iconposition(12,13,14) };
    like($@, qr{\Qwrong # args: should be "wm iconposition window ?x y?"});

    eval { $mw->iconposition('bad', 13) };
    like($@, qr{\Q'bad' isn't numeric});

    eval { $mw->iconposition(13, 'lousy') };
    like($@, qr{\Q'lousy' isn't numeric});

    is_deeply([$mw->iconposition], [], "wm iconposition, setting and reading values");
    $mw->iconposition(10, 20);
    is_deeply([$mw->iconposition], [10, 20]);
    $mw->iconposition(undef, undef);
    is_deeply([$mw->iconposition], []);
}

{
    ### wm iconwindow ###
    eval { $mw->iconwindow(12, 13) };
    like($@, qr{\Qwrong # args: should be "wm iconwindow window ?pathName?"},
	 q{wm iconwindow usage});

    eval { $mw->iconwindow("bogus") };
    like($@, qr{bad window path name "bogus"});
}

{
    my $b = $mw->Button(Name => "b", -text => "Help");
    eval { $t->iconwindow($b) };
    like($@, qr{\Qcan't use .b as icon window: not at top level});
    $b->destroy;
}

{
    my $icon = $mw->Toplevel(Name => "icon",
			     qw(-width 50 -height 50 -bg green));
    my $t2 = $mw->Toplevel(Name => "t2");
    poswin $t2;
    $t2->iconwindow($icon);
    eval { $t->iconwindow($icon) };
    like($@, qr{\Q.icon is already an icon for .t2});

    $t2->destroy;
    $icon->destroy;
}

{
    is($t->iconwindow, undef, "wm iconwindow, setting and reading values");
    my $icon = $mw->Toplevel(Name => "icon",
			     qw(-width 50 -height 50 -bg green));
    $t->iconwindow($icon);
    is($t->iconwindow, $icon);
    $t->iconwindow(undef);
    is($t->iconwindow, undef);
}

{
    ### wm maxsize ###
    eval { $mw->maxsize("a") };
    like($@, qr{\Qwrong # args: should be "wm maxsize window ?width height?"},
	 q{wm maxsize usage});

    eval { $mw->maxsize(qw(a b c)) };
    like($@, qr{\Qwrong # args: should be "wm maxsize window ?width height?"});

    eval { $mw->maxsize(qw(x 100)) };
    like($@, qr{'x' isn't numeric});

    eval { $mw->maxsize(qw(100 bogus)) };
    like($@, qr{'bogus' isn't numeric});
}

{
    my $t2 = $mw->Toplevel;
    poswin $t2;
    $t2->maxsize(300, 200);
    is_deeply([$t2->maxsize], [300,200]);
    $t2->destroy;
}

{
    my $t = $mw->Toplevel;
    poswin $t;
    my($t_width, $t_height) = $t->maxsize;
    my($s_width, $s_height) = ($t->screenwidth, $t->screenheight);
    cmp_ok($t_width, "<=", $s_width, 
	   "maxsize must be <= screen size");
    cmp_ok($t_height, "<=", $s_height);
    $t->destroy;
}

{
    local $TODO;
    $TODO = "Fails currently on Windows" if $Tk::platform eq 'MSWin32';
    $TODO = "May fail on KDE" if !$TODO && $kwin_problems;
    $TODO = "May fail on some window managers (e.g. fvwm 2.4.x)" if !$TODO && $wm_problems;

    my $t = $mw->Toplevel(qw(-width 300 -height 300));
    poswin $t;
    $t->update;
    $t->maxsize(200, 150);
    # UpdateGeometryInfo invoked at idle
    $t->update;

    my($w,$h) = $t->geometry =~ m{(\d+)x(\d+)};
    is($w, 200, q{setting the maxsize to a smaller value will resize a toplevel});
    is($h, 150);
    $t->destroy;
}

{
    local $TODO;
    $TODO = "Fails currently on Windows" if $Tk::platform eq 'MSWin32';
    $TODO = "May fail on KDE" if !$TODO && $kwin_problems;
    $TODO = "May fail on some window managers (e.g. fvwm 2.4.x)" if !$TODO && $wm_problems;

    my $t = $mw->Toplevel;
    poswin $t;
    $t->wmGrid(0,0,50,50);
    $t->geometry("6x6");
    $t->update;
    $t->maxsize(4, 3);
    # UpdateGeometryInfo invoked at idle
    $t->update;

    my($w,$h) = $t->geometry =~ m{(\d+)x(\d+)};
    is($w, 4, q{setting the maxsize to a smaller value will resize a gridded toplevel});
    is($h, 3);
    $t->destroy;
}

{
    local $TODO;
    $TODO = "May fail on KDE" if !$TODO && $kwin_problems;
    $TODO = "May fail on xfwm4" if !$TODO && $xfwm4_problems;
    $TODO = "May fail on fluxbox" if !$TODO && $fluxbox_problems;
    $TODO = "May fail on MacOSX" if !$TODO && $macosx_x11_problems;

    my $t = $mw->Toplevel(qw(-width 200 -height 200));
    poswin $t;
    $t->maxsize(300, 250);
    $t->update;
    $t->geometry("400x300");
    $t->update;
    my($w,$h) = $t->geometry =~ m{(\d+)x(\d+)};
    is($w, 300, q{attempting to resize to a value bigger than the current maxsize});
    # ... will set it to the max size
    is($h, 250);
    $t->destroy;
}    

{
    local $TODO;
    $TODO = "May fail on KDE" if !$TODO && $kwin_problems;
    $TODO = "May fail on xfwm4" if !$TODO && $xfwm4_problems;
    $TODO = "May fail on fluxbox" if !$TODO && $fluxbox_problems;

    my $t = $mw->Toplevel;
    poswin $t;
    $t->wmGrid(qw(1 1 50 50));
    $t->geometry("4x4");
    $t->maxsize(6, 5);
    $t->update;
    $t->geometry("8x6");
    $t->update;
    my($w,$h) = $t->geometry =~ m{(\d+)x(\d+)};
    is($w, 6, q{attempting to resize a gridded toplevel to a value bigger});
    # ... than the current maxsize will set it to the max size
    is($h, 5);
    $t->destroy;
}

{
    my $t = $mw->Toplevel;
    poswin $t;
    my $tf = $t->Frame(qw(-width 400 -height 400))->pack;
    $t->idletasks;
    is($t->reqwidth, 400);
    is($t->reqheight, 400);
    $t->maxsize(300, 300);
    $t->update;

    local $TODO;
    $TODO = "Fails currently on Windows" if $Tk::platform eq 'MSWin32';
    $TODO = "May fail on KDE" if !$TODO && $kwin_problems;
    $TODO = "May fail on some window managers (e.g. fvwm 2.4.x)" if !$TODO && $wm_problems;

    my($w,$h) = $t->geometry =~ m{(\d+)x(\d+)};
    is($w, 300, q{Use max size if window size is not explicitly set});
    # ... and the reqWidth/reqHeight are bigger than the max size
    is($h, 300);
}    

{
    ### wm minsize ###
    eval { $mw->minsize("a") };
    like($@, qr{\Qwrong # args: should be "wm minsize window ?width height?"},
	 q{wm minsize usage});

    eval { $mw->minsize(qw(a b c)) };
    like($@, qr{\Qwrong # args: should be "wm minsize window ?width height?"});

    eval { $mw->minsize(qw(x 100)) };
    like($@, qr{'x' isn't numeric});

    eval { $mw->minsize(qw(100 bogus)) };
    like($@, qr{'bogus' isn't numeric});
}

{
    my $t2 = $mw->Toplevel;
    poswin $t2;
    $t2->minsize(300, 200);
    is_deeply([$t2->minsize], [300,200]);
    $t2->destroy;
}

{
    local $TODO;
    $TODO = "Fails currently on Windows" if $Tk::platform eq 'MSWin32';
    $TODO = "May fail on KDE" if !$TODO && $kwin_problems;
    $TODO = "May fail on xfwm4" if !$TODO && $xfwm4_problems;
    $TODO = "May fail on MacOSX" if !$TODO && $macosx_x11_problems;
    $TODO = "May fail on fluxbox" if !$TODO && $fluxbox_problems;

    my $t = $mw->Toplevel(qw(-width 200 -height 200));
    poswin $t;
    $t->update;
    $t->minsize(400, 300);
    # UpdateGeometryInfo invoked at idle
    $t->update;

    my($w,$h) = $t->geometry =~ m{(\d+)x(\d+)};
    is($w, 400, q{setting the minsize to a larger value will resize a toplevel});
    is($h, 300);
    $t->destroy;
}

{
    local $TODO;
    $TODO = "Fails currently on Windows" if $Tk::platform eq 'MSWin32';
    $TODO = "May fail on KDE" if !$TODO && $kwin_problems;
    $TODO = "May fail on xfwm4" if !$TODO && $xfwm4_problems;
    $TODO = "May fail on MacOSX" if !$TODO && $macosx_x11_problems;
    $TODO = "May fail on fluxbox" if !$TODO && $fluxbox_problems;

    my $t = $mw->Toplevel;
    poswin $t;
    $t->wmGrid(qw(1 1 50 50));
    $t->geometry("4x4");
    $t->update;
    $t->minsize(8,8);
    # UpdateGeometryInfo invoked at idle
    $t->update;

    my($w,$h) = $t->geometry =~ m{(\d+)x(\d+)};
    is($w, 8, q{setting the minsize to a larger value will resize a gridded toplevel});
    is($h, 8);
    $t->destroy;
}    

{
    local $TODO;
    $TODO = "May fail on KDE" if !$TODO && $kwin_problems;
    $TODO = "May fail on some window managers (e.g. fvwm 2.4.x)" if !$TODO && $wm_problems;

    my $t = $mw->Toplevel(qw(-width 400 -height 400));
    poswin $t;
    $t->minsize(300, 300);
    $t->update;
    $t->geometry("200x200");
    $t->update;
    my($w,$h) = $t->geometry =~ m{(\d+)x(\d+)};
    is($w, 300, q{attempting to resize to a value smaller than the current minsize});
    # ... will set it to the minsize
    is($h, 300);
    $t->destroy;
}

{
    local $TODO;
    $TODO = "May fail on KDE" if !$TODO && $kwin_problems;
    $TODO = "May fail on some window managers (e.g. fvwm 2.4.x)" if !$TODO && $wm_problems;

    my $t = $mw->Toplevel;
    poswin $t;
    $t->wmGrid(qw(1 1 50 50));
    $t->geometry("8x8");
    $t->minsize(6, 6);
    $t->update;
    $t->geometry("4x4");
    $t->update;
    my($w,$h) = $t->geometry =~ m{(\d+)x(\d+)};
    is($w, 6, q{attempting to resize a gridded toplevel to a value smaller});
    # than the current minsize will set it to the minsize when gridded
    is($h, 6);
    $t->destroy;
}

{
    my $t = $mw->Toplevel;
    poswin $t;
    my $tf = $t->Frame(qw(-width 250 -height 250))->pack;
    $t->idletasks;
    is($t->reqwidth, 250);
    is($t->reqheight, 250);
    $t->minsize(300, 300);
    $t->update;

    local $TODO;
    $TODO = "Fails currently on Windows" if $Tk::platform eq 'MSWin32';
    $TODO = "May fail on KDE" if !$TODO && $kwin_problems;
    $TODO = "May fail on xfwm4" if !$TODO && $xfwm4_problems;
    $TODO = "May fail on fluxbox" if !$TODO && $fluxbox_problems;

    my($w,$h) = $t->geometry =~ m{(\d+)x(\d+)};
    is($w, 300, q{Use min size if window size is not explicitly set});
    # ... and the reqWidth/reqHeight are smaller than the min size
    is($h, 300);
    $t->destroy;
}

{
    ### wm overrideredirect ###
    eval { $mw->overrideredirect(1, 2) };
    like($@, qr{\Qwrong # args: should be "wm overrideredirect window ?boolean?"},
	 "wm overrideredirect usage");

    ## In Perl probably interpreted as a true value
    #eval { $mw->overrideredirect("boo") };
    #like($@, qr{\Qexpected boolean value but got "boo"});

    is($mw->overrideredirect, 0, "wm overrideredirect, setting and reading values");
    $mw->overrideredirect(1);
    is($mw->overrideredirect, 1);
    $mw->overrideredirect(0);
    is($mw->overrideredirect, 0);
}

{
    ### wm positionfrom ###
    eval { $mw->positionfrom(1, 2) };
    like($@, qr{\Qwrong # args: should be "wm positionfrom window ?user/program?"},
	 "wm positionfrom usage");

    eval { $mw->positionfrom("none") };
    like($@, qr{bad argument "none": must be program, or user});
}

{
    my $t2 = $mw->Toplevel;
    poswin $t2;
    $t2->positionfrom("user");
    is($t2->positionfrom, "user", "wm positionfrom, setting and reading values");
    $t2->positionfrom("program");
    is($t2->positionfrom, "program");
    $t2->positionfrom(undef);
    is($t2->positionfrom, undef);    
    $t2->destroy;
}

{
    ### wm protocol ###
    eval { $mw->protocol(1, 2, 3) };
    like($@, qr{\Qwrong # args: should be "wm protocol window ?name? ?command?"},
	 "wm protocol usage");
}

{
    my $t = $mw->Toplevel;
    poswin $t;
    $t->protocol("foo a", "a b c");
    $t->protocol("bar", "test script for bar");
    is_deeply([$t->protocol], ["bar", "foo a"],
	      "wm protocol, setting and reading values");
    $t->protocol("foo a", undef);
    $t->protocol("bar", undef);
    is_deeply([$t->protocol], []);
    $t->destroy;
}

{
    my $t = $mw->Toplevel;
    poswin $t;
    $t->protocol("foo", "a b c");
    $t->protocol("bar", "test script for bar");
    isa_ok($t->protocol("foo"), "Tk::Callback");
    isa_ok($t->protocol("bar"), "Tk::Callback");
    $t->protocol("foo", undef);
    $t->protocol("bar", undef);
    is($t->protocol("foo"), undef);
    is($t->protocol("bar"), undef);
    $t->destroy;
}

{
    my $t = $mw->Toplevel;
    poswin $t;
    my $code1 = sub { "a b c" };
    $t->protocol("foo", $code1);
    my $code2 = sub { "test script" };
    $t->protocol("foo", $code2);
    is($t->protocol("foo")->[0], $code2);
    $t->protocol("foo", ["bla"]);
    isa_ok($t->protocol("foo"), "Tk::Callback");
    is($t->protocol("foo")->[0], "bla");
    $t->destroy;
}

{
    ### wm resizable ###
    eval { $mw->resizable(1) };
    like($@, qr{\Qwrong # args: should be "wm resizable window ?width height?"},
	 "wm resizable usage");

    eval { $mw->resizable(1,2,3) };
    like($@, qr{\Qwrong # args: should be "wm resizable window ?width height?"});

    ## Valid in Perl, "bad" is a boolean value
    #eval { $mw->resizable("bad", 0) };

    $mw->resizable(0, 1);
    is_deeply([$mw->resizable], [0, 1], "wm resizable, setting and reading values");
    $mw->resizable(1, 0);
    is_deeply([$mw->resizable], [1, 0]);
    $mw->resizable(1, 1);
    is_deeply([$mw->resizable], [1, 1]);
}

{
    ### wm sizefrom ###
    eval { $mw->sizefrom(1, 2) };
    like($@, qr{\Qwrong # args: should be "wm sizefrom window ?user|program?"},
	 "wm sizefrom usage");

    eval { $mw->sizefrom("bad") };
    like($@, qr{bad argument "bad": must be program, or user});

    $t->sizefrom("user");
    is($t->sizefrom, "user", "wm sizefrom, setting and reading values");
    $t->sizefrom("program");
    is($t->sizefrom, "program");
    $t->sizefrom(undef);
    is($t->sizefrom, undef);
}

{
    ### wm stackorder ###
    eval { $mw->stackorder("_") };
    like($@, qr{\Qwrong # args: should be "wm stackorder window ?isabove|isbelow window?"},
	 "wm stackorder usage");

    eval { $mw->stackorder("_", "_", "_") };
    like($@, qr{\Qwrong # args: should be "wm stackorder window ?isabove|isbelow window?"});

    eval { $mw->stackorder("is", ".") };
    like($@, qr{\Qambiguous argument "is": must be isabove, or isbelow});

    eval { $mw->stackorder("isabove", "_") };
    like($@, qr{\Qbad window path name "_"});
}

for my $is ("isabove", "isbelow") {
    my $t = $mw->Toplevel(Name => "t");
    poswin $t;
    my $tb = $t->Button(Name => "b")->pack;
    $mw->update;
    eval { $mw->stackorder($is, $tb) };
    like($@, qr{\Qwindow ".t.b" isn't a top-level window});
    $t->destroy;
}

for my $is ("isabove", "isbelow") {
    my $t = $mw->Toplevel(Name => "t");
    poswin $t;
    $t->update;
    $t->withdraw;
    eval { $t->stackorder($is, $mw) };
    like($@, qr{\Qwindow ".t" isn't mapped},
	 "wm stackorder usage, isabove|isbelow toplevels must be mapped");
    $t->destroy;
}
    
deleteWindows;

# stackorder was not defined before Tk804.027_001
eval {
    {
	local $TODO;
	$TODO = "May fail on KDE" if !$TODO && $kwin_problems;
	$TODO = "May fail on xfwm4" if !$TODO && $xfwm4_problems;

	my $t = $mw->Toplevel(Name => "t");
	poswin $t;
	$t->update;
	is_deeply([$mw->stackorder], [".", ".t"]);
	$t->destroy;
    }

    {
	local $TODO;
	$TODO = "May fail on KDE" if !$TODO && $kwin_problems;
	$TODO = "May fail sometimes on some older window managers (e.g. metacity 2.10.x)" if !$TODO && $wm_problems;

	my $t = $mw->Toplevel(Name => "t");
	poswin $t;
	$t->update;
	$mw->raise;
	raiseDelay;
	is_deeply([$mw->stackorder], [".t", "."]);
	$t->destroy;
    }

    {
	local $TODO;
	$TODO = "May fail sometimes on some window managers (e.g. metacity)"
	    if $wm_problems;

	my $t = $mw->Toplevel(Name => "t");
	poswin $t;
	$t->update;
	my $t2 = $mw->Toplevel(Name => "t2");
	poswin $t2;
	$t2->update;
	$mw->raise;
	$t2->raise;
	raiseDelay;
	is_deeply([$mw->stackorder], [".t", ".", ".t2"]);
	Tk::destroy($t, $t2);
    }

    {
	local $TODO;
	$TODO = "May fail sometimes on some window managers (e.g. metacity)"
	    if $wm_problems;

	my $t = $mw->Toplevel(Name => "t");
	poswin $t;
	$t->update;
	my $t2 = $mw->Toplevel(Name => "t2");
	poswin $t2;
	$t2->update;
	$mw->raise;
	$t2->lower;
	raiseDelay;
	is_deeply([$mw->stackorder], [".t2", ".t", "."]);
	Tk::destroy($t, $t2);
    }

    {
	local $TODO;
	$TODO = "May fail sometimes on some window managers (e.g. metacity)"
	    if $wm_problems;

	my $parent = $mw->Toplevel(Name => "parent");
	poswin $parent;
	$parent->update;
	my $parent_child1 = $parent->Toplevel(Name => "child1");
	poswin $parent_child1;
	$parent_child1->update;
	my $parent_child2 = $parent->Toplevel(Name => "child2");
	poswin $parent_child2;
	$parent_child2->update;
	my $extra = $mw->Toplevel(Name => "extra");
	poswin $extra;
	$extra->update;
	$parent->raise;
	$parent_child2->lower;
	raiseDelay;
	is_deeply([$parent->stackorder], [qw(.parent.child2 .parent.child1 .parent)]);
    }

    deleteWindows;

    {
	local $TODO;
	$TODO = "May fail on KDE" if !$TODO && $kwin_problems;
	$TODO = "May fail on fluxbox" if !$TODO && $fluxbox_problems;

	my $t1 = $mw->Toplevel(Name => "t1");
	poswin $t1;
	my $t1b = $t1->Button->pack;
	$mw->update;
	is_deeply([$mw->stackorder], [".", ".t1"],
		  q{non-toplevel widgets ignored});
    }

    deleteWindows;

    {
	is_deeply([$mw->stackorder], ["."],
		  q{no children returns self});
    }

    deleteWindows;

 SKIP: {
	skip("Needs a window manager", 1)
	    if !$wm_running;

	local $TODO;
	$TODO = "May fail on KDE" if !$TODO && $kwin_problems;

	my $t1 = $mw->Toplevel(Name => "t1");
	poswin $t1;
	$t1->update;
	my $t2 = $mw->Toplevel(Name => "t2");
	poswin $t2;
	$t2->update;
	$t1->iconify;
	is_deeply([$mw->stackorder], [".", ".t2"],
		  "unmapped toplevel");
	$t2->destroy;
	$t1->destroy;
    }


    {
	local $TODO;
	$TODO = "May fail on fluxbox" if !$TODO && $fluxbox_problems;

	my $t1 = $mw->Toplevel(Name => "t1");
	poswin $t1;
	$t1->update;
	my $t2 = $mw->Toplevel(Name => "t2");
	poswin $t2;
	$t2->update;
	$t2->withdraw;
	is_deeply([$mw->stackorder], [".", ".t1"]);
	$t2->destroy;
	$t1->destroy;
    }

    {
	my $t1 = $mw->Toplevel(Name => "t1");
	poswin $t1;
	$t1->update;
	my $t2 = $mw->Toplevel(Name => "t2");
	poswin $t2;
	$t2->update;
	$t2->withdraw;
	is_deeply([$t2->stackorder], []);
	$t2->destroy;
	$t1->destroy;
    }

    {
	my $t1 = $mw->Toplevel(Name => "t1");
	poswin $t1;
	$t1->update;
	my $t1t2 = $t1->Toplevel(Name => "t2");
	poswin $t1t2;
	$t1t2->update;
	$t1t2->withdraw;
	is_deeply([$t1->stackorder], [".t1"]);
	$t1t2->destroy;
	$t1->destroy;
    }

    {
	my $t1 = $mw->Toplevel(Name => "t1");
	poswin $t1;
	$t1->update;
	my $t1t2 = $t1->Toplevel(Name => "t2");
	poswin $t1t2;
	$t1t2->update;
	$t1->withdraw;
	is_deeply([$t1->stackorder], [".t1.t2"]);
	$t1t2->destroy;
	$t1->destroy;
    }

    {
	local $TODO;
	$TODO = "May fail on KDE" if !$TODO && $kwin_problems;
	$TODO = "May fail on fluxbox" if !$TODO && $fluxbox_problems;

	my $t1 = $mw->Toplevel(Name => "t1");
	poswin $t1;
	$t1->update;
	my $t1t2 = $t1->Toplevel(Name => "t2");
	poswin $t1t2;
	$t1t2->update;
	my $t1t2t3 = $t1t2->Toplevel(Name => "t3");
	poswin $t1t2t3;
	$t1t2t3->update;
	$t1t2->withdraw;
	is_deeply([$t1->stackorder],[".t1", ".t1.t2.t3"]);
	$t1t2t3->destroy;
	$t1t2->destroy;
	$t1->destroy;
    }

    {
	my $t1 = $mw->Toplevel(Name => "t1");
	poswin $t1;
	$t1->update;
	my $t1t2 = $t1->Toplevel(Name => "t2");
	poswin $t1t2;
	$t1t2->update;
	$t1->withdraw;
	is_deeply([$t1->stackorder], [".t1.t2"],
		  q{unmapped toplevel, mapped children returned});
	$t1t2->destroy;
	$t1->destroy;
    }

    {
	my $t1 = $mw->Toplevel;
	is_deeply([$mw->stackorder], ["."],
		  q{toplevel mapped in idle callback});
	$t1->destroy;
    }

    deleteWindows;

    {
	local $TODO;
	$TODO = "May fail on KDE" if !$TODO && $kwin_problems;
	$TODO = "May fail on fluxbox" if !$TODO && $fluxbox_problems;

	my $t = $mw->Toplevel;
	poswin $t;
	$t->update;
	$t->raise;
	is($mw->stackorder("isabove", $t), 0,
	   q{wm stackorder isabove|isbelow});
	$t->destroy;
    }

    {
	local $TODO;
	$TODO = "May fail on KDE" if !$TODO && $kwin_problems;
	$TODO = "May fail on fluxbox" if !$TODO && $fluxbox_problems;

	my $t = $mw->Toplevel;
	poswin $t;
	$t->update;
	$t->raise;
	is($mw->stackorder("isbelow", $t), 1);
	$t->destroy;
    }

    {
	local $TODO;
	$TODO = "May fail sometimes on some older window managers (e.g. metacity 2.10.x)"
	    if $wm_problems;

	my $t = $mw->Toplevel;
	poswin $t;
	$t->update;
	$mw->raise;
	raiseDelay;
	is($t->stackorder("isa", $mw), 0);
	$t->destroy;
    }

    {
	local $TODO;
	$TODO = "May fail sometimes on some older window managers (e.g. metacity 2.10.x)"
	    if $wm_problems;

	my $t = $mw->Toplevel;
	poswin $t;
	$t->update;
	$mw->raise;
	raiseDelay;
	is($t->stackorder("isb", $mw), 1);
	$t->destroy;
    }

    deleteWindows;

    {
	local $TODO;
	$TODO = "May fail sometimes on some older window managers (e.g. metacity 2.10.x)"
	    if $wm_problems;

	my $t = $mw->Toplevel(Name => "t");
	poswin $t;
	my $tm = $t->Menu(-type => "menubar");
	$tm->add("cascade", -label => "File");
	$t->configure(-menu => $tm);
	$mw->update;
	$mw->raise;
	raiseDelay;
	is_deeply([$mw->stackorder], [".t", "."],
		  q{a menu is not a toplevel});
	$t->destroy;
    }

    {
	my $t = $mw->Toplevel(Name => "t");
	poswin $t;
	$t->overrideredirect(1);
	$mw->raise;
	$mw->update;
	raiseDelay;
	if ($mw->stackorder("isabove", $t)) {
	    # Problem seen with twm on travis-ci system
	    # and on a Mac OS X system
	    # (http://www.cpantesters.org/cpan/report/404e4ab4-3738-11e3-850b-7bc3a04c628c)
	    diag "Window manager too slow? Delay and retry...";
	    raiseDelayLonger;
	}
	is($mw->stackorder("isabove", $t), 0,
	   q{A normal toplevel can't be raised above an overrideredirect toplevel});
	$t->destroy;
    }

    {
	my $t = $mw->Toplevel(Name => "t");
	poswin $t;
	$t->overrideredirect(1);
	$mw->lower;
	$mw->update;
	raiseDelay;
	is($mw->stackorder("isbelow", $t), 1,
	   q{A normal toplevel can be explicitely lowered});
	$t->destroy;
    }

    {
	local $TODO;
	$TODO = "May fail on KDE" if !$TODO && $kwin_problems;

	my $real = $mw->Toplevel(Name => "real", -container => 1);
	poswin $real;
	my $embd = $mw->Toplevel(Name => "embd",
				 -bg => "blue", -use => $real->id);
	poswin $embd;
	$mw->update;
	is_deeply([$mw->stackorder], [".", ".real"],
		  q{An embedded toplevel does not appear in the stacking order});
	$embd->destroy;
	$real->destroy;
    }
};
if ($@) {
    fail("stackorder tests failed: $@");
}

stdWindow;

{
    ### wm title ###
    eval { $mw->title("1", "2") };
    like($@, qr{\Qwrong # args: should be "wm title window ?newTitle?"},
	 "wm title usage");

    my $t = $mw->Toplevel;
    is($t->title, "Toplevel", "wm title, setting and reading values");
    $t->title("Apa");
    is($t->title, "Apa");
    $t->title(undef);
    is($t->title, "");
    $t->destroy;
}

{
    ### wm transient ###
    my $t = $mw->Toplevel(Name => "t");
    eval { $t->transient(1, 2) };
    like($@, qr{\Qwrong # args: should be "wm transient window ?master?"},
	 "wm transient usage");

    eval { $t->transient("foo") };
    like($@, qr{bad window path name "foo"});    
}

{
    deleteWindows;
    my $master = $mw->Toplevel(Name => "master");
    my $subject = $mw->Toplevel(Name => "subject");
    $subject->transient($master);
    eval { $subject->iconify };
    like($@, qr{\Qcan't iconify ".subject": it is a transient});
}

{
    deleteWindows;
    my $icon = $mw->Toplevel(Name => "icon", -bg => "blue");
    my $top = $mw->Toplevel(Name => "top");
    $top->iconwindow($icon);
    my $dummy = $mw->Toplevel;
    eval { $icon->transient($dummy) };
    like($@, qr{\Qcan't make ".icon" a transient: it is an icon for .top});
}

{
    deleteWindows;
    my $icon = $mw->Toplevel(Name => "icon", -bg => "blue");
    my $top = $mw->Toplevel(Name => "top");
    $top->iconwindow($icon);
    my $dummy = $mw->Toplevel;
    eval { $dummy->transient($icon) };
    like($@, qr{\Qcan't make ".icon" a master: it is an icon for .top});
}

{
    deleteWindows;
    my $master = $mw->Toplevel(Name => "master");
    eval { $master->transient($master) };
    like($@, qr{\Qcan't make ".master" its own master});
}

{
    deleteWindows;
    my $master = $mw->Toplevel(Name => "master");
    my $f = $master->Frame(Name => "f");
    eval { $master->transient($f) };
    like($@, qr{\Qcan't make ".master" its own master});
}

{
    deleteWindows;
    my $master = $mw->Toplevel(Name => "master");
    my $subject = $mw->Toplevel(Name => "subject");
    is($subject->transient, undef, "basic get/set of master");
    $subject->transient($master);
    is($subject->transient, $master);
    $subject->transient(undef);
    is($subject->transient, undef);
}

{
    deleteWindows;
    my $master = $mw->Toplevel(Name => "master");
    my $f = $master->Frame;
    my $subject = $mw->Toplevel(Name => "subject");
    $subject->transient($f);
    is($subject->transient, $master,
       "first toplevel parent of non-toplevel master is used");
}

{
    deleteWindows;
    my $master = $mw->Toplevel(Name => "master");
    poswin $master;
    $master->withdraw;
    $mw->update;
    my $subject = $mw->Toplevel(Name => "subject");
    poswin $subject;
    $subject->transient($master);
    $mw->update;
    is($subject->state, "withdrawn",
       "transient toplevel is withdrawn when mapped if master is withdrawn");
    is($subject->ismapped, 0);
}

{
    deleteWindows;
    my $master = $mw->Toplevel(Name => "master");
    poswin $master;
    $master->withdraw;
    $mw->update;
    my $subject = $mw->Toplevel(Name => "subject");
    poswin $subject;
    $mw->update;
    $subject->transient($master);
    $mw->update;
    is($subject->state, "withdrawn",
       q{already mapped transient toplevel takes on withdrawn state of master});
    is($subject->ismapped, 0);
}

{
    deleteWindows;
    my $master = $mw->Toplevel;
    my $subject = $mw->Toplevel;
    poswin $master, $subject;
    $mw->update;
    $subject->transient($master);
    $master->withdraw;
    $mw->update;
    is($subject->state, "withdrawn",
       q{withdraw on the master also does a withdraw on the transient});
    is($subject->ismapped, 0);
    $master->deiconify;
    $mw->update;

    local $TODO;
    $TODO = "May fail on some window managers (e.g. fvwm 2.5.x)"
	if $wm_problems;

    is($subject->state, "normal",
       q{deiconify on the master also does a deiconify on the transient});
    is($subject->ismapped, 1);
}

SKIP: {
    skip("Needs a window manager", 2)
	if !$wm_running;

    local $TODO;
    $TODO = "May fail on some window managers (e.g. metacity)"
	if $wm_problems;

    deleteWindows;
    my $master = $mw->Toplevel;
    poswin $master;
    $master->iconify;
    $mw->update;
    my $subject = $mw->Toplevel;
    poswin $subject;
    $subject->transient($master);
    $mw->update;
    is($subject->state, "withdrawn",
       q{transient toplevel is withdrawn when mapped if master is iconic});
    is($subject->ismapped, 0);
}

SKIP: {
    skip("Needs a window manager", 2)
	if !$wm_running;

    local $TODO;
    $TODO = "May fail on some window managers (e.g. metacity)"
	if $wm_problems;

    deleteWindows;
    my $master = $mw->Toplevel;
    poswin $master;
    $master->iconify;
    $mw->update;
    my $subject = $mw->Toplevel;
    poswin $subject;
    $mw->update;
    $subject->transient($master);
    $mw->update;
    is($subject->state, "withdrawn",
       q{already mapped transient toplevel is withdrawn if master is iconic});
    is($subject->ismapped, 0);
}

SKIP: {
    skip("Needs a window manager", 4)
	if !$wm_running;

    deleteWindows;
    my $master = $mw->Toplevel;
    my $subject = $mw->Toplevel;
    poswin $master, $subject;
    $mw->update;
    $subject->transient($master);
    $master->iconify;
    $mw->update;
    is($subject->state, "withdrawn",
       q{iconify on the master does a withdraw on the transient});
    is($subject->ismapped, 0);
    $master->deiconify;
    $mw->update;

    local $TODO;
    $TODO = "May fail on fluxbox" if !$TODO && $fluxbox_problems;

    is($subject->state, "normal",
       q{deiconify on the master does a deiconify on the transient});
    is($subject->ismapped, 1);
}

{
    deleteWindows;
    my $master = $mw->Toplevel;
    my $subject = $mw->Toplevel;
    poswin $master, $subject;
    $mw->update;
    $subject->transient($master);
    eval { $subject->transient(".bad") };
    ok($@, q{error during transient should not cause deletion of map/unmap binding});
    $master->withdraw;
    $mw->update;

    local $TODO;
    $TODO = "May fail on some window managers (e.g. fvwm 2.5.x)"
	if $wm_problems;

    is($subject->state, "withdrawn");
    $master->deiconify;
    $mw->update;
    is($subject->state, "normal");
}

{
    deleteWindows;
    my $master = $mw->Toplevel;
    my $subject = $mw->Toplevel;
    poswin $master, $subject;
    $subject->transient($master);
    $mw->update;
    $master->destroy;
    $mw->update;
    is($subject->transient, undef,
       q{remove transient property when master is destroyed});
}

{
    deleteWindows;
    my $master = $mw->Toplevel;
    my $subject = $mw->Toplevel;
    $subject->transient($master);
    $master->destroy;
    is($subject->transient, undef,
       q{remove transient property from unmapped window when master is destroyed});
}

{
    deleteWindows;
    my $master = $mw->Toplevel;
    my $subject = $mw->Toplevel;
    poswin $master, $subject;
    $mw->update;
    $subject->transient($master);
    $subject->withdraw;
    $master->withdraw;
    $master->deiconify;
    # idle handler should not map the transient
    $mw->update;
    is($subject->state, q{withdrawn},
       q{a withdrawn transient does not track state changes in the master});
}

{
    local $TODO;
    $TODO = "May fail on some window managers (e.g. fvwm 2.5.x)"
	if $wm_problems;

    deleteWindows;
    my $master = $mw->Toplevel;
    my $subject = $mw->Toplevel;
    poswin $master, $subject;
    $mw->update;
    $subject->transient($master);
    $subject->withdraw;
    $master->withdraw;
    $master->deiconify;
    # idle handler should not map the transient
    $mw->update;
    is($subject->state, "withdrawn",
       q{a withdrawn transient does not track state changes in the master});
    $subject->deiconify;
    is($subject->state, "normal");
    $master->withdraw;
    is($subject->state, "withdrawn");
    $master->deiconify;
    # idle handler should map transient
    $mw->update;
    is($subject->state, "normal");
}

{
    deleteWindows;
    my $master = $mw->Toplevel;
    my $subject = $mw->Toplevel;
    poswin $master, $subject;
    $mw->update;
    # withdraw before making window a transient
    $subject->withdraw;
    $subject->transient($master);
    $master->withdraw;
    $master->deiconify;
    # idle handler should not map the transient
    $mw->update;
    is($subject->state, q{withdrawn},
       q{a withdrawn transient does not track state changes in the master});
}

{
    # wm-transient-7.*: See SF Tk Bug #592201 "wm transient fails with two masters"
    # wm-transient-7.3 through 7.5 all caused panics on Unix in Tk 8.4b1.
    # 7.1 and 7.2 added to catch (potential) future errors.

    deleteWindows;
    my $t = $mw->Toplevel;
    my $transient = $mw->Toplevel;
    $transient->transient($t);
    $transient->destroy;
    $t->destroy;
    pass("Destroying transient did not cause a panic");
}

{
    my $t = $mw->Toplevel;
    my $transient = $mw->Toplevel;
    $transient->transient($t);
    $t->destroy;
    is($transient->transient, undef);
    $transient->destroy;
    pass("Destroying master did not cause a panic");
}

{
    deleteWindows;
    my $t1 = $mw->Toplevel;
    my $t2 = $mw->Toplevel;
    my $transient = $mw->Toplevel;
    $transient->transient($t1);
    $transient->transient($t2);
    $t1->destroy; # Caused panic in 8.4b1
    $t2->destroy;
    $transient->destroy;
    pass(q{Reassign transient, destroy old master});
}

{
    deleteWindows;
    my $t1 = $mw->Toplevel;
    my $t2 = $mw->Toplevel;
    my $transient = $mw->Toplevel;
    $transient->transient($t1);
    $transient->transient($t2);
    $t2->destroy; # caused panic in 8.4b1
    $t1->destroy;
    $transient->destroy;
    pass(q{Reassign transient, destroy new master});
}

{
    my $t1 = $mw->Toplevel;
    my $t2 = $mw->Toplevel;
    my $transient = $mw->Toplevel;
    $transient->transient($t1);
    $transient->transient($t2);
    $transient->destroy;
    $t2->destroy; # caused panic in 8.4b1
    $t1->destroy; # so did this
    pass(q{Reassign transient, destroy transient});
}

{
    ### wm state ###
    eval { $mw->state("_", "_") };
    like($@, qr{\Qwrong # args: should be "wm state window ?state?"},
	 "wm state usage");
}

{
    deleteWindows;
    my $t = $mw->Toplevel;
    is($t->state, "normal", "initial state");
}

{
    deleteWindows;
    my $t = $mw->Toplevel;
    $t->state("withdrawn");
    is($t->state, "withdrawn",
       q{state change before map});
}

{
    deleteWindows;
    my $t = $mw->Toplevel;
    $t->withdraw;
    is($t->state, "withdrawn",
       q{state change before map});
}

{
    deleteWindows;
    my $t = $mw->Toplevel;
    poswin $t;
    $mw->update;
    $t->state("withdrawn");
    is($t->state, "withdrawn",
       q{state change after map});
}

{
    deleteWindows;
    my $t = $mw->Toplevel;
    poswin $t;
    $mw->update;
    $t->withdraw;
    is($t->state, "withdrawn",
       q{state change after map});
}

SKIP: {
    skip("Needs a window manager", 1)
	if !$wm_running;

    deleteWindows;
    my $t = $mw->Toplevel;
    $t->state("iconic");
    is($t->state, q{iconic},
       q{state change before map, iconic});
}

{
    deleteWindows;
    my $t = $mw->Toplevel;
    $t->iconify;
    is($t->state, q{iconic},
       q{state change before map, iconic});
}

SKIP: {
    skip("Needs a window manager", 1)
	if !$wm_running;

    deleteWindows;
    my $t = $mw->Toplevel;
    poswin $t;
    $mw->update;
    $t->state("iconic");
    is($t->state, q{iconic},
       q{state change after map, iconic});
}

SKIP: {
    skip("Needs a window manager", 1)
	if !$wm_running;

    local $TODO;
    $TODO = "May fail on some window managers (e.g. fvwm 2.4.x or xfwm4 4.2.3.2)" if !$TODO && $wm_problems;

    deleteWindows;
    my $t = $mw->Toplevel;
    poswin $t;
    $mw->update;
    $t->iconify;
    is($t->state, q{iconic},
       q{state change after map, iconic});
}

{
    deleteWindows;
    my $t = $mw->Toplevel;
    $t->withdraw;
    $t->state("normal");
    is($t->state, "normal",
       q{state change before map, normal});
}

{
    deleteWindows;
    my $t = $mw->Toplevel;
    $t->withdraw;
    $t->deiconify;
    is($t->state, "normal",
       q{state change before map, normal});
}

{
    deleteWindows;
    my $t = $mw->Toplevel;
    poswin $t;
    $mw->update;
    $t->withdraw;
    $t->state("normal");
    if ($fvwm_problems && $t->state ne 'normal') { $t->deiconify }
    is($t->state, "normal",
       q{state change after map, normal});
}

{
    deleteWindows;
    my $t = $mw->Toplevel;
    poswin $t;
    $mw->update;
    $t->withdraw;
    $t->deiconify;
    if ($fvwm_problems && $fvwm_problems && $t->state ne 'normal') { $t->deiconify }
    is($t->state, "normal",
       q{state change after map, normal});
}

{
    deleteWindows;
    my $t = $mw->Toplevel;
    $t->iconify;
    $t->state("normal");
    is($t->state, "normal",
       q{state change before map, iconify+normal});
}

{
    deleteWindows;
    my $t = $mw->Toplevel;
    $t->iconify;
    $t->deiconify;
    is($t->state, "normal",
       q{state change before map, iconify+normal});
}

{
    deleteWindows;
    my $t = $mw->Toplevel;
    poswin $t;
    $mw->update;
    $t->iconify;
    $t->state("normal");
    is($t->state, "normal",
       q{state change after map, iconify+normal});
}

{
    deleteWindows;
    my $t = $mw->Toplevel;
    poswin $t;
    $mw->update;
    $t->iconify;
    $t->deiconify;
    is($t->state, "normal",
       q{state change after map, iconify+normal});
}

SKIP: {
    skip("zoomed only implemented on win", 1)
	if $Tk::platform ne 'MSWin32';
    deleteWindows;
    my $t = $mw->Toplevel;
    poswin $t;
    $mw->update;
    $t->state("zoomed");
    is($t->state, "zoomed",
       q{state change after map, zoomed});
}

{
    ### wm withdraw ###
    eval { $mw->withdraw("_") };
    like($@, qr{\Qwrong # args: should be "wm withdraw window"},
	 "wm withdraw usage");
}

{
    deleteWindows;
    my $t = $mw->Toplevel(Name => "t");
    my $t2 = $mw->Toplevel(Name => "t2");
    poswin $t;
    $t->iconwindow($t2);
    eval { $t2->withdraw };
    like($@, qr{\Qcan't withdraw .t2: it is an icon for .t});
    
    $mw->update;
    $t->withdraw;
    is($t->state, "withdrawn");
    is($t->ismapped, 0);
    $t->deiconify;
    if ($fvwm_problems && $t->state ne 'normal') { $t->deiconify }
    is($t->state, "normal");
    is($t->ismapped, 1);
}

### Misc. wm tests ###

if (0) {
    ## This test probably cannot be translated to Perl/Tk;
    ## also needs TK_ALT_DISPLAY defined

    # See Tk Bug #671330 "segfault when e.g. deiconifying destroyed window"
    my $w = $mw->Toplevel(Name => "t", -screen => $ENV{TK_ALT_DISPLAY});
    my $w2 = $w;
    $w->deiconify  ;# this caches the WindowRep
    $w->destroy;
    eval { $w2->deiconify };
    like($@, qr{\Qbad window path name ".t"},
	 q{Deletion epoch on multiple displays});
}

# FIXME:

# Test delivery of virtual events to the WM. We could check to see
# if the window was raised after a button click for example.
# This sort of testing may not be possible.

__END__
