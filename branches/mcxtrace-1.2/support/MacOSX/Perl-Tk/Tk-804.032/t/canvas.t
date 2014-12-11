#!/usr/bin/perl -w
# -*- perl -*-

#
# $Id: $
# Translated by: Slaven Rezic
#
# Original copyright from tk/tests/canvas.test, version 1.23 from
# tktoolkit CVS on sourceforge:

# This file is a Tcl script to test out the procedures in tkCanvas.c,
# which implements generic code for canvases.  It is organized in the
# standard fashion for Tcl tests.
#
# Copyright (c) 1995-1996 Sun Microsystems, Inc.
# Copyright (c) 1998-2000 Ajuba Solutions.
# All rights reserved.
#
# RCS: @(#) $Id: canvas.test,v 1.23 2004/12/07 21:22:19 dgp Exp $

use strict;
use FindBin;
use lib $FindBin::RealBin;

use Getopt::Long;
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

use TkTest qw(is_float_pair);

plan tests => 166;

use_ok("Tk::Canvas");

my $verbose = 0;
GetOptions("v" => \$verbose)
    or die "usage: $0 [-v]";

# XXX - This test file is woefully incomplete.  At present, only a
# few of the features are tested.

my $mw = MainWindow->new;
$mw->geometry("+10+10");

if ($^O eq 'darwin') {
    # Under some newer MacOSX versions it seems that there are
    # problems by creating and destroying widgets with different
    # widths. The canvas widget with requested -width 100 created
    # after the one with -width 60 will only be 60px wide. This is
    # probably some wm-related bug, but as we test here for canvas
    # features, we need to workaround this bug. This is just done with
    # a dummy frame, which just makes sure that the mainwindow width
    # is at least 100 pixels.
    $mw->Frame(-width => 100, -height => 1)->pack;
}

my $c = $mw->Canvas;
isa_ok($c, "Tk::Canvas");
isa_ok($c, "Tk::Widget");
$c->pack;
$c->update;

sub deleteWindows () {
    eval { $_->destroy } for $mw->children;
}

use constant SKIP_CGET    => 5;
use constant SKIP_CONF    => 6;
use constant SKIP_ERROR   => 7;
use constant SKIP_RESTORE => 8;

my @tests =
   (
    ['-background', '#ff0000', '#ff0000', 'non-existent',
     'unknown color name "non-existent"'],
    ['-bg', '#ff0000', '#ff0000', 'non-existent',
     'unknown color name "non-existent"'],
    [qw(-bd 4 4 badValue), 'bad screen distance "badValue"'],
    [qw(-borderwidth 1.3 1 badValue), 'bad screen distance "badValue"'],
    [qw(-closeenough 24 24 bogus), q{'bogus' isn't numeric}],
    [qw(-confine true 1 silly), 'expected boolean value but got "silly"', 0,0,1,0], # probably auto-converted to some boolean value?
    [qw(-cursor arrow arrow badValue), 'bad cursor spec "badValue"'],
    [qw(-height 2.1 2 x42), 'bad screen distance "x42"'],
    [qw(-highlightbackground), '#112233', '#112233', 'ugly', 'unknown color name "ugly"'],
    [qw(-highlightcolor), '#110022', '#110022',	'bogus', 'unknown color name "bogus"'],
    [qw(-highlightthickness 18 18 badValue), 'bad screen distance "badValue"'],
    [qw(-insertbackground), '#110022', '#110022', 'bogus', 'unknown color name "bogus"'],
    [qw(-insertborderwidth 1.3 1 2.6x), 'bad screen distance "2.6x"'],
    [qw(-insertofftime 100 100 3.2), q{expected integer but got "3.2"}, 0,0,1,0], # probably auto-converted to integer?
    [qw(-insertontime 100 100 3.2), q{expected integer but got "3.2"}, 0,0,1,0], # probably auto-converted to integer?
    [qw(-insertwidth 1.3 1 6x), q{bad screen distance "6x"}],
    [qw(-relief groove groove 1.5), q{bad relief type "1.5": must be flat, groove, raised, ridge, solid, or sunken}],
    [qw(-selectbackground), '#110022', '#110022', 'bogus', q{unknown color name "bogus"}],
    [qw(-selectborderwidth 1.3 1 badValue), q{bad screen distance "badValue"}],
    [qw(-selectforeground), '#654321', '#654321', 'bogus', q{unknown color name "bogus"}],
    [qw(-takefocus), "any string", "any string", undef, undef],
    [qw(-width 402 402 xyz), q{bad screen distance "xyz"}],
    [qw(-xscrollcommand), q{Some command}, q{Some command}, undef, undef, 1],
    [qw(-yscrollcommand), q{Another command}, q{Another command}, undef, undef, 1],
);

foreach my $test (@tests) {
    my $name = $test->[0];
    $c->configure($name, $test->[1]);
    if (!$test->[SKIP_CGET]) {
	is($c->cget($name), $test->[2], "cget $name");
    }
    if (!$test->[SKIP_CONF]) {
	is(($c->configure($name))[4], $c->cget($name), "Comparing configure and cget values for $name");
    }

    if (defined $test->[4]) {
	if (!$test->[SKIP_ERROR]) {
	    eval { $c->configure($name, $test->[3]) };
	    like($@, qr/$test->[4]/, "Expected error message for $name");
	}
    }
    if (!$test->[SKIP_RESTORE]) {
	$c->configure($name, ($c->configure($name))[3]);
    }
}

eval { $c->configure(-gorp => "foo") };
like($@, qr{Bad option `-gorp'}, "configure throws error on bad option");
$c->create("rect",10,10,100,100);
eval { $c->configure(-gorp => "foo") };
like($@, qr{Bad option `-gorp'}, "configure throws error on bad option");

eval { $c->destroy };
$c = $mw->Canvas(qw(-width 60 -height 40),
		 -scrollregion => [qw(0 0 200 150)],
		 -bd => 0,
		 -highlightthickness => 0,
		)->pack;
$c->update;

{
    my $i = $c->createRectangle(10,10,100,100);
    eval { $c->bind($i, "<a>") };
    is($@, "", "bind method");
}

{
    my $i = $c->create('rect',10,10,100,100);
    eval { $c->bind($i, "<") };
    like($@, qr{no event type or button # or keysym}, "bind method with failure");
}

{
    $c->configure(-xscrollincrement => 40, -yscrollincrement => 5);
    $c->xview('moveto', 0);
    $c->update;
    is_float_pair([$c->xview], [0, 0.3], "xview method");
    $c->xview('scroll', 2, 'units');
    $c->update;
    is_float_pair([$c->xview], [0.4, 0.7], "xview method after scroll");
}

{
    # Tcl/Tk comment:
    # This test gives slightly different results on platforms such
    # as NetBSD.  I don't know why...
    # Perl/Tk comment:
    # Everything's ok on a FreeBSD machine.
    $c->configure(-xscrollincrement => 0, -yscrollincrement => 5);
    $c->xviewMoveto(0.6);
    $c->update;
    is_float_pair([$c->xview], [0.6, 0.9], "xview method (2)");
    $c->xviewScroll(2, 'units');
    $c->update;
    is_float_pair([$c->xview], [0.66, 0.96], "xview method after scroll (2)");
}

eval { $c->destroy };
$c = $mw->Canvas(qw(-width 60 -height 40),
		 -scrollregion => [qw(0 0 200 80)],
		 -borderwidth => 0,
		 -highlightthickness => 0,
		)->pack;
$c->update;

{
    $c->configure(qw(-xscrollincrement 40 -yscrollincrement 5));
    $c->yview('moveto', 0);
    $c->update;
    is_float_pair([$c->yview], [0, 0.5], "yview method");
    $c->yview('scroll', 3, 'units');
    $c->update;
    is_float_pair([$c->yview], [0.1875, 0.6875], "yview method after scroll");
}

{
    $c->configure(qw(-xscrollincrement 40 -yscrollincrement 0));
    $c->yviewMoveto(0);
    $c->update;
    is_float_pair([$c->yview], [0, 0.5], "yview method (2)");
    $c->yviewScroll(2, 'units');
    $c->update;
    is_float_pair([$c->yview], [0.1, 0.6], "yview method after scroll (2)");
}

{
    eval { $c->destroy };
    $c = $mw->Canvas(qw(-width 100 -height 50),
		     -scrollregion => [qw(-200 -100 305 102)],
		     -borderwidth => 2,
		     -highlightthickness => 3,
		    )->pack;
    $c->update;
    $c->configure(qw(-xscrollincrement 0 -yscrollincrement 0));
    $c->xview('moveto', 0);
    $c->yview('moveto', 0);
    $c->update;
    is($c->canvasx(0), -205, "canvasx after scrolling to origin");
    is($c->canvasy(0), -105, "canvasy after scrolling to origin");
}

{
    $c->configure(qw(-xscrollincrement 20 -yscrollincrement 10));
    my @x;
    for my $i (qw(.08 .10 .48 .50)) {
	$c->xviewMoveto($i);
	$c->update;
	push @x, $c->canvasx(0);
    }
    is_deeply(\@x, [-165, -145, 35, 55], "canvasx after multiple scroll");
}

{
    $c->configure(qw(-xscrollincrement 20 -yscrollincrement 10));
    my @x;
    for my $i (qw(.06 .08 .70 .72)) {
	$c->yviewMoveto($i);
	$c->update;
	push @x, $c->canvasy(0);
    }
    is_deeply(\@x, [-95, -85, 35, 45], "canvasy after multiple scroll");
}

{
    $c->configure(qw(-xscrollincrement 20 -yscrollincrement 10));
    $c->xview('moveto', 1.0);
    is($c->canvasx(0), 215);
}

{
    $c->configure(qw(-xscrollincrement 20 -yscrollincrement 10));
    $c->yview(moveto => 1.0);
    is($c->canvasy(0), 55);
}

deleteWindows;

{
    eval { $c->destroy } if Tk::Exists($c); # without existence test dumps SV contents ...
    $c = $mw->Canvas;
    $c->create(qw(arc -100 10 100 210 -start 10 -extent 50 -style arc -tags arc1));
    is_deeply([$c->bbox("arc1")], [qw(48 21 100 94)], "BBox of arc");
    $c->createArc(qw(100 10 300 210 -start 10 -extent 50 -style chord -tags arc2));
    is_deeply([$c->bbox("arc2")], [qw(248 21 300 94)], "BBox of chord");
    $c->create(qw(arc 300 10 500 210 -start 10 -extent 50 -style pieslice -tags arc3));
    is_deeply([$c->bbox("arc3")], [qw(398 21 500 112)], "BBox of pieslice");
}

{
    eval { $c->destroy } if Tk::Exists($c); # without existence test dumps SV contents ...
    $c = $mw->Canvas;

    # With Tk 8.0.4 the ids are now stored in a hash table.  You
    # can use this test as a performance test with older versions
    # by changing the value of size.
    my $size = 15;

    for my $i (0 .. $size-1) {
	my $x = -10 + 3*$i;
	for(my $j = 0, my $y = -10; $j < 10; $j++, $y+=3) {
	    $c->create('rect', "${x}c", "${y}c", ($x+2)."c", ($y+2)."c",
		       qw(-outline black -fill blue -tags rect));
	    $c->create('text', ($x+1)."c", ($y+1)."c", -text => "$i,$j",
		       qw(-anchor center -tags text));
	}
    }

    # The actual bench mark - this code also exercises all the hash
    # table changes.

    my $time = Tk::timeofday();
    foreach my $id ($c->find(withtag => "all")) {
	$c->lower($id);
	$c->raise($id);
	$c->find(withtag => $id);
	$c->bind('<Return>', $id, '');
	$c->delete($id);
    }
    my $delta = Tk::timeofday() - $time;
    diag "Canvas creation and deletion test needed $delta s"
	if $verbose;
}

{
    eval { $c->destroy } if Tk::Exists($c); # without existence test dumps SV contents ...
    $c = $mw->Canvas;

    $c->create(qw(oval 20 20 40 40 -fill red -tag) , [qw(a b c d)]);
    $c->create(qw(oval 20 60 40 80 -fill yellow -tag), [qw(b a)]);
    $c->create(qw(oval 20 100 40 120 -fill green -tag), [qw(c b)]);
    $c->create(qw(oval 20 140 40 160 -fill blue -tag), [qw(b)]);
    $c->create(qw(oval 20 180 40 200 -fill bisque -tag), [qw(a d e)]);
    $c->create(qw(oval 20 220 40 240 -fill bisque -tag b));
    $c->create(qw(oval 20 260 40 280 -fill bisque -tag), ['d', "tag with spaces"]);

    is_deeply([$c->find(withtag => q{!a})],[qw(3 4 6 7)], "Tag expressions");
    is_deeply([$c->find(withtag => q{b&&c})],[qw(1 3)]);
    is_deeply([$c->find(withtag => q{b||c})],[qw(1 2 3 4 6)]);
    is_deeply([$c->find(withtag => q{a&&!b})],[qw(5)]);
    is_deeply([$c->find(withtag => q{!b&&!c})],[qw(5 7)]);
    is_deeply([$c->find(withtag => q{d&&a&&c&&b})],[qw(1)]);
    is_deeply([$c->find(withtag => q{b^a})],[qw(3 4 5 6)]);
    is_deeply([$c->find(withtag => q{(a&&!b)||(!a&&b)})],[qw(3 4 5 6)]);
    is_deeply([$c->find(withtag => q{ ( a && ! b ) || ( ! a && b ) })],[qw(3 4 5 6)]);
    is_deeply([$c->find(withtag => q{a&&!(c||d)})],[qw(2)]);
    is_deeply([$c->find(withtag => q{d&&"tag with spaces"})],[qw(7)], "Tag with spaces");
    is_deeply([$c->find(withtag => q"tag with spaces")],[qw(7)]);
}

for my $testdef (
		 [q{&&c}, qr{Unexpected operator in tag search expression}],
		 [q{!!c}, qr{Too many '!' in tag search expression}],
		 [q{b||}, qr{Missing tag in tag search expression}],
		 [q{b&&(c||)}, qr{Unexpected operator in tag search expression}],
		 [q{d&&""}, qr{Null quoted tag string in tag search expression}],
		 [q"d&&\"tag with spaces", qr{Missing endquote in tag search expression}],
		 [q{a&&"tag with spaces"z}, qr{Invalid boolean operator in tag search expression}],
		 [q{a&&b&c}, qr{Singleton '&' in tag search expression}],
		 [q{a||b|c}, qr{Singleton '|' in tag search expression}],
		) {
    my($tag_expr, $error_rx) = @$testdef;

    eval { $c->destroy } if Tk::Exists($c); # without existence test may dump SV contents ...
    $c = $mw->Canvas;

    $c->create(qw(oval 20 20 40 40 -fill red -tag), [qw(a b c d)]);
    $c->create(qw(oval 20 260 40 280 -fill bisque -tag), ['d', "tag with spaces"]);
    eval { $c->find(withtag => $tag_expr) };
    like($@, $error_rx, "Tag expression error ($tag_expr)");
}

{
    eval { $c->destroy } if Tk::Exists($c); # without existence test may dump SV contents ...
    $c = $mw->Canvas;

    $c->create(qw(oval 20 20 40 40 -fill red -tag), [q{ strange tag(xxx&yyy|zzz) " && \" || ! ^ " }]);
    ok($c->find(withtag => q{ strange tag(xxx&yyy|zzz) " && \" || ! ^ " }), 
       q{backward compatility - strange tags that are not expressions});
} 

{
    eval { $c->destroy } if Tk::Exists($c); # without existence test may dump SV contents ...
    $c = $mw->Canvas;

    $c->bind(q{a && b}, '<Enter>' => sub {warn "Enter"});
    $c->bind(q{a && b}, '<Leave>' => sub {warn "Leave"});

    pass(q{multple events bound to same tag expr});
}

{
    eval { $c->destroy } if Tk::Exists($c); # without existence test may dump SV contents ...
    $c = $mw->Canvas->pack;

    # This would crash in 8.3.0 and 8.3.1
    $c->create(qw(polygon 0 0 100 100 200 50),
	       -fill => undef,
	       qw(-stipple gray50 -outline black));
    pass(q{canvas poly fill check, bug 5783});
}
   
{
    eval { $c->destroy } if Tk::Exists($c); # without existence test may dump SV contents ...
    $c = $mw->Canvas->pack;

    $c->create(qw(poly 30 30 90 90 30 90 90 30));
    ok($c->find(qw(over 40 40 45 45)), "rect region inc. edge; canvas poly overlap fill check, bug 226357");
    ok($c->find(qw(over 60 40 60 40)), "top-center point");
    ok(!$c->find(qw(over 0 0 0 0)), "not on poly");
    {
	# The failure only occurs with the real X server, but not
	# with Xnest and Xvfb, and it seems to occur only on some
	# X server versions (maybe driver dependent?). An equivalent
	# wish8.4 program had the same problem.
	local $TODO;
	$TODO = "Failure observed under some conditions on Debian"
	    if $^O eq 'linux';
	ok($c->find(qw(over 60 60 60 60)), "center-point");
    }
    ok(!$c->find(qw(over 45 50 45 50)), "outside poly");

    $c->itemconfigure(1, -fill => "", -outline => "black");
    ok($c->find(qw(over 40 40 45 45)), "rect region inc. edge");
    ok($c->find(qw(over 60 40 60 40)), "top-center point");
    ok(!$c->find(qw(over 0 0 0 0)), "not on poly");
    ok($c->find(qw(over 60 60 60 60)), "center-point");
    ok(!$c->find(qw(over 45 50 45 50)), "outside poly");

    $c->itemconfigure(1, -width => 8);
    ok($c->find(qw(over 45 50 45 50)), "outside poly?");
}

{
    eval { $c->destroy } if Tk::Exists($c); # without existence test may dump SV contents ...
    $c = $mw->Canvas->pack;

    my $qx = 1.+1.;
    # qx has type double and no string representation (in Tcl?)
    $c->scale('all', $qx, 0, 1., 1.);
    # qx has now type MMRep and no string representation (in Tcl?);
    is($qx, 2, q{canvas mm obj, patch SF-403327, 102471});
}

{
    eval { $c->destroy } if Tk::Exists($c); # without existence test may dump SV contents ...
    $c = $mw->Canvas->pack;

    my $val = 10;
    $val++;
    # qx has type double and no string representation (in Tcl?)
    $c->scale('all', $val, 0, 1, 1);
    # qx has now type MMRep and no string representation (in Tcl?)
    $val++;
    is($val, 12, q{canvas mm obj, patch SF-403327, 102471});
}

{
    my $x = "";
    
    my $kill_canvas = sub {
	my $w = shift;
	$w->destroy;
	$w = $mw->Canvas(qw(-height 200 -width 200))->pack(qw(-fill both -expand yes));
	$mw->idletasks;
	$w->create('rectangle', qw(80 80 120 120 -fill blue -tags blue));
	# bind a button press to re-build the canvas
	$w->bind('blue', '<ButtonRelease-1>' => sub { $x .= "ok" });
	$w;
    };

    $c = $kill_canvas->($c);

    # do this many times to improve chances of triggering the crash
    for my $i (0 .. 29) {
	$c->eventGenerate('<1>', qw(-x 100 -y 100));
	$c->eventGenerate('<ButtonRelease-1>', qw(-x 100 -y 100));
    }
    is($x, "okokokokokokokokokokokokokokokokokokokokokokokokokokokokokok",
       q{canvas delete during event, SF bug-228024});
}

{
    eval { $c->destroy } if Tk::Exists($c); # without existence test may dump SV contents ...
    $c = $mw->Canvas->pack;

    eval { $c->scan };
    like($@, qr{\Qwrong # args: should be ".canvas scan mark|dragto x y ?dragGain?"\E},
	 q{canvas scan SF bug 581560});

    eval { $c->scan("bogus") };
    like($@, qr{\Qwrong # args: should be ".canvas scan mark|dragto x y ?dragGain?"\E},
	 "canvas scan");

    eval { $c->scan("mark") };
    like($@, qr{\Qwrong # args: should be ".canvas scan mark|dragto x y ?dragGain?"\E});

    $c->scan(qw(mark 10 10));
    pass("correct canvas scan mark");

    eval { $c->scan(qw(mark 10 10 5)) };
    like($@, qr{wrong # args: should be ".canvas scan mark x y"});

    $c->scan(qw(dragto 10 10 5));
    pass("correct canvas scan dragto");
}

{
    foreach my $type (qw{arc bitmap image line oval polygon rect text window}) {
	eval { $c->destroy } if Tk::Exists($c); # without existence test may dump SV contents ...
	$c = $mw->Canvas->pack;

	eval { $c->create($type) };
	like($@, qr{wrong # args: should be ".canvas create $type coords \Q?arg arg ...?"\E},
	     "basic types check: $type requires coords");

	eval { $c->destroy } if Tk::Exists($c); # without existence test may dump SV contents ...
	$c = $mw->Canvas->pack;

	eval { $c->create($type, 0) };
	like($@, qr{wrong # coordinates: expected},
	     "basic coords check: $type coords are paired");
    }
}

{
    eval { $c->destroy } if Tk::Exists($c); # without existence test may dump SV contents ...
    $c = $mw->Canvas->pack;

    my $id = $c->createArc(qw(0 10 20 30 -start 33));
    is($c->itemcget($id, "-start"), 33, "arc coords check");
}

{
    local $TODO = "Decide whether test failures are expected or not...";

    eval { $c->destroy } if Tk::Exists($c); # without existence test may dump SV contents ...
    $c = $mw->Canvas->pack;

    my $id = $c->createLine(qw{0 0 1 1 2 2 3 3 4 4 5 5 6 6});
    is($c->itemcget($id, '-smooth'), 0);

    foreach my $smoothtest (
			    ['yes', 'true'],
			    [1, 'true'],
			    ['bezier', 'true'],
			    ['raw', 'raw'],
			    ['r', 'raw'],
			    ['b', 'b']
			   ) {
	my($smoother, $expected) = @$smoothtest;
	$c->itemconfigure($id, -smooth => $smoother);
	is($c->itemcget($id, '-smooth'), $expected, "smooth test");
    }
}

__END__
