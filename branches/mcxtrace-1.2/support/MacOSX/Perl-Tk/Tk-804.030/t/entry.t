#!/usr/bin/perl -w
# -*- perl -*-

# This file is a Tcl script to test entry widgets in Tk.  It is
# organized in the standard fashion for Tcl tests.
#
# Copyright (c) 1994 The Regents of the University of California.
# Copyright (c) 1994-1997 Sun Microsystems, Inc.
# Copyright (c) 1998-1999 by Scriptics Corporation.
# All rights reserved.
#
# Translated to Perl/Tk by Slaven Rezic

use strict;
use FindBin;
use lib $FindBin::RealBin;

use Tk;
use Tk::Trace;
use Tk::Config ();
my $Xft = $Tk::Config::xlib =~ /-lXft\b/;

use TkTest qw(wm_info);

BEGIN {
    if (!eval q{
	use Test::More;
	1;
    }) {
	print "# tests only work with installed Test::More module\n";
	print "1..1\n";
	print "ok 1\n";
	exit;
    }
}

plan tests => 351;

use Getopt::Long;

my $do_wm_test   = 0; # tests seem to pass only with Kwin
my $do_font_test = 1;

GetOptions("wmtest!"   => \$do_wm_test,
	   "fonttest!" => \$do_font_test,
	  )
    or die "usage: $0 [-[no]wmtest] [-[no]fonttest]";

my $skip_wm_test = "window manager dependent tests";
my $skip_font_test = "font-related tests";
my $skip_wm_font_test = "window manager and/or font-related tests";

my $mw = Tk::MainWindow->new();
$mw->geometry('+10+10');

my %wm_info = wm_info($mw);
my $wm_name = $wm_info{name};

my $kwin_problems     = defined $wm_name && $wm_name eq 'KWin';
my $fluxbox_problems  = defined $wm_name && $wm_name eq 'Fluxbox';
my $metacity_problems = defined $wm_name && $wm_name eq 'Metacity';
my $xfwm4_problems    = defined $wm_name && $wm_name eq 'Xfwm4';

# It seems that scripts using -xscrollcommand have the same problem
# with wish8.4 (Tcl/Tk 8.4.19)
sub TODO_xscrollcommand_problem () {
    $TODO = "May fail under some conditions (another grab?) on Metacity" if !$TODO && $metacity_problems;
    $TODO = "May fail under some conditions (another grab?) on xfwm4"    if !$TODO && $xfwm4_problems;
}

# Some WMs are slow when resizing the main window. This may cause test
# failures, because the test suite does not wait for completion of the
# WM (and probably cannot do it anyway). To avoid resizing the main
# window, a placeholder widget is created. This widget has to be
# re-created every time the main window is re-created, or if all
# children are destroyed.
sub create_placeholder_widget () {
    if ($kwin_problems || $fluxbox_problems) {
	$mw->Frame(-width => 640, -height => 1)->pack;
    }
}

create_placeholder_widget;

my $e0 = $mw->Entry;
ok(Tk::Exists($e0), "Entry widget exists");
isa_ok($e0, "Tk::Entry");

my @scrollInfo;
sub scroll {
    (@scrollInfo) = @_;
}

# Create additional widget that's used to hold the selection at times.

my $sel = $mw->Entry;
$sel->insert("end", "This is some sample text");

# Font names

my $big   = $Xft ? '{Adobe Helvetica} -24' : "-adobe-helvetica-medium-r-normal--24-240-75-75-p-*-iso8859-1";
## Always use the X11 font, even with Xft, otherwise the measurements would
## be wrong.
#my $fixed = $Xft ? '{Adobe Courier} -12' : "-adobe-courier-medium-r-normal--12-120-75-75-m-*-iso8859-1";
my $fixed = "-adobe-courier-medium-r-normal--12-120-75-75-m-*-iso8859-1";

# Create entries in the option database to be sure that geometry options
# like border width have predictable values.

$mw->option("add", "*Entry.borderWidth", 2);
$mw->option("add", "*Entry.highlightThickness", 2);
## Again, prefer the X11 font.
#$mw->option("add", "*Entry.font", $Xft ? '{Adobe Helvetica} -12' : "Helvetica -12");
$mw->option("add", "*Entry.font", "Helvetica -12");

my $e = $mw->Entry(qw(-bd 2 -relief sunken))->pack;
$mw->update;

if (!$Xft) { # XXX Is this condition necessary?
    my %fa = $mw->fontActual($e->cget(-font));
    my %expected = (
		    "-weight" => "normal",
		    "-underline" => 0,
		    "-family" => "helvetica",
		    "-slant" => "roman",
		    "-size" => -12,
		    "-overstrike" => 0,
		   );
    while(my($k,$v) = each %expected) {
	if ($v ne $fa{$k}) {
	    $do_font_test = 0;
	    last;
	}
    }
}

my $i;

use constant SKIP_CGET    => 5;
use constant SKIP_CONF    => 6;
use constant SKIP_ERROR   => 7;
use constant SKIP_RESTORE => 8;

my @tests = (
    [qw(-background), '#ff0000', '#ff0000', 'non-existent',
	    'unknown color name "non-existent"'],
    [qw(-bd 4 4 badValue), 'bad screen distance "badValue"'],
    [qw(-bg), '#ff0000', '#ff0000', 'non-existent', 'unknown color name "non-existent"'],
    [qw(-borderwidth 1.3 1 badValue), 'bad screen distance "badValue"'],
    [qw(-cursor arrow arrow badValue), 'bad cursor spec "badValue"'],
    [qw(-disabledbackground green green non-existent),
        q{unknown color name "non-existent"}],
    [qw(-disabledforeground blue blue non-existent),
        q{unknown color name "non-existent"}],
    [qw(-exportselection yes 1 xyzzy), 'expected boolean value but got "xyzzy"', 0,0,1],
    [qw(-fg), '#110022', '#110022', 'bogus', 'unknown color name "bogus"'],
    [qw(-font -Adobe-Helvetica-Medium-R-Normal--*-120-*-*-*-*-*-*
	-Adobe-Helvetica-Medium-R-Normal--*-120-*-*-*-*-*-*), undef,
        'font "" doesn\'t exist',
        1,0,1],
    [qw(-foreground), '#110022', '#110022', 'bogus', 'unknown color name "bogus"'],
    [qw(-highlightbackground), '#123456', '#123456', 'ugly', 'unknown color name "ugly"'],
    [qw(-highlightcolor), '#123456', '#123456', 'bogus', 'unknown color name "bogus"'],
    [qw(-highlightthickness 6 6 bogus), 'bad screen distance "bogus"'],
    [qw(-highlightthickness -2 0), undef, undef],
    [qw(-insertbackground), '#110022', '#110022', 'bogus', 'unknown color name "bogus"'],
    [qw(-insertborderwidth 1.3 1 2.6x), 'bad screen distance "2.6x"'],
    [qw(-insertofftime 100 100 3.2), 'expected integer but got "3.2"', 0,0,1],
    [qw(-insertontime 100 100 3.2), 'expected integer but got "3.2"', 0,0,1],
    [qw(-invalidcommand), \&Tk::NoOp, \&Tk::NoOp, undef, undef],
    [qw(-invcmd), \&Tk::NoOp, \&Tk::NoOp, undef, undef],
    [qw(-justify right right bogus), 'bad justification "bogus": must be left, right, or center'],
    [qw(-readonlybackground green green non-existent),
        q{unknown color name "non-existent"}],
    [qw(-relief groove groove 1.5), 'bad relief "1.5": must be flat, groove, raised, ridge, solid, or sunken'],
    [qw(-selectbackground), '#110022', '#110022', 'bogus', 'unknown color name "bogus"'],
    [qw(-selectborderwidth 1.3 1 badValue), 'bad screen distance "badValue"'],
    [qw(-selectforeground), '#654321', '#654321', 'bogus', 'unknown color name "bogus"'],
    [qw(-show * *), undef, undef],
    [qw(-state n normal bogus), 'bad state "bogus": must be disabled, normal, or readonly'],
    [qw(-takefocus), "any string", "any string", undef, undef],
    [qw(-textvariable), \$i, \$i, undef, undef],
    [qw(-width 402 402 3p), "'3p' isn't numeric"],
    [qw(-xscrollcommand), 'Some command', 'Some command', undef, undef, 1,1,1,1],
);

foreach my $test (@tests) {
    my $name = $test->[0];
    $e->configure($name, $test->[1]);
    if (!$test->[SKIP_CGET]) {
	my $val = $e->cget($name);
	if (UNIVERSAL::isa($val, "Tk::Callback")) {
	    $val = $val->[0];
	}
	is($val, $test->[2], "cget $name");
    }
    if (!$test->[SKIP_CONF]) {
	is(($e->configure($name))[4], $e->cget($name), "Comparing configure and cget values for $name");
    }

    if (defined $test->[4]) {
	if (!$test->[SKIP_ERROR]) {
	    eval { $e->configure($name, $test->[3]) };
	    like($@, qr/$test->[4]/, "Expected error message for $name");
	}
    }
    if (!$test->[SKIP_RESTORE]) {
	$e->configure($name, ($e->configure($name))[3]);
    }
}

eval { $e->destroy };
$e = $mw->Entry;
ok(Tk::Exists($e));
is($e->class, 'Entry', "Tk class name");
isa_ok($e, 'Tk::Entry');

eval { $e->destroy; undef $e };
eval { $e = $mw->Entry(-gorp => 'foo') };
like($@, qr/unknown option "-gorp"/);
ok(!Tk::Exists($e), "Expected failure while creating entry widget with bad options");
ok(!defined $e);

eval { $e->destroy };
$e = $mw->Entry(-font => $fixed)->pack;
$e->update;

my $cx = $mw->fontMeasure($fixed, 'a');
my $cy = $mw->fontMetrics($fixed, '-linespace');
my $ux = $mw->fontMeasure($fixed, "\x{4e4e}");

eval { $e->bbox };
like($@, qr/wrong \# args: should be ".* bbox index"/, "bbox error message");

eval { $e->bbox(qw(a b)) };
like($@, qr/wrong \# args: should be ".* bbox index"/);

eval { $e->bbox(qw(bogus)) };
like($@, qr/bad entry index "bogus"/);

$e->delete(0,"end");
is_deeply([$e->bbox(0)],[5,5,0,$cy], "Expected bbox");

$e->delete(0,"end");
$e->insert(0,"abc");
is_deeply([$e->bbox(3)],[5+2*$cx,5,$cx,$cy]);
is_deeply([$e->bbox("end")],[$e->bbox(3)]);

$e->delete(0,"end");
$e->insert(0,"ab\x{4e4e}");
is_deeply([$e->bbox("end")],[5+2*$cx,5,$ux,$cy], "Bbox check with unicode char (at end)");

$e->delete(0,"end");
$e->insert(0,"ab\x{4e4e}c");
is_deeply([$e->bbox(3)],[5+2*$cx+$ux,5,$cx,$cy], "Bbox check with unicode char (before index)");

$e->delete(0,"end");
is_deeply([5,5,0,$cy],[$e->bbox("end")]);

$e->delete(0,"end");
$e->insert(0,"abcdefghij\x{4e4e}klmnop");
is_deeply([[$e->bbox(0)],
	   [$e->bbox(1)],
	   [$e->bbox(10)],
	   [$e->bbox("end")]],
	  [[5,5,$cx,$cy],
	   [5+$cx,5,$cx,$cy],
	   [5+10*$cx,5,$ux,$cy],
	   [5+$ux+15*$cx,5,$cx,$cy]], "More bbox checks with unicode char");

eval { $e->cget };
like($@, qr/wrong \# args: should be ".* cget option"/, "cget error message");

eval { $e->cget(qw(a b)) };
like($@, qr/wrong \# args: should be ".* cget option"/);

eval { $e->cget(-gorp) };
like($@, qr/unknown option "-gorp"/);

$e->configure(-bd => 4);
is($e->cget(-bd), 4);
is(scalar @{$e->configure}, 36);

eval { $e->configure('-foo') };
like($@, qr/unknown option "-foo"/, "Unknown option error message");

$e->configure(-bd => 4);
$e->configure(-bg => '#ffffff');
is(($e->configure(-bd))[4], 4);

eval { $e->delete };
like($@, qr/wrong \# args: should be ".* delete firstIndex \?lastIndex\?"/, "delete error message");

eval { $e->delete(qw(a b c)) };
like($@, qr/wrong \# args: should be ".* delete firstIndex \?lastIndex\?"/);

eval { $e->delete("foo") };
like($@, qr/bad entry index "foo"/);

eval { $e->delete(0, "bar") };
like($@, qr/bad entry index "bar"/);

$e->delete(0, "end");
$e->insert("end", "01234567890");
$e->delete(2, 4);
is($e->get, "014567890", "Expected get after insert");

$e->delete(0, "end");
$e->insert("end", "01234567890");
$e->delete(6);
is($e->get, "0123457890");

$e->delete(0, "end");
$e->insert("end", "01234\x{4e4e}67890");
$e->delete(6);
is($e->get, "01234\x{4e4e}7890", "Delete with unicode character before");

$e->delete(0, "end");
$e->insert("end", "012345\x{4e4e}7890");
$e->delete(6);
is($e->get, "0123457890", "Delete unicode character");

$e->delete(0, "end");
$e->insert("end", "0123456\x{4e4e}890");
$e->delete(6);
is($e->get, "012345\x{4e4e}890", "Delete with unicode character after");

$e->delete(0,"end");
$e->insert("end", "01234567890");
$e->delete(6, 5);
is($e->get, "01234567890", "Delete reversed range");

$e->delete(0,"end");
$e->insert("end", "01234567890");
$e->configure(-state => 'disabled');
$e->delete(2, 8);
$e->configure(-state => 'normal');
is($e->get, "01234567890", "Delete while disabled state");

eval { $e->get("foo") };
like($@, qr/wrong \# args: should be ".* get"/, "get error message");

eval { $e->icursor };
like($@, qr/wrong \# args: should be ".* icursor pos"/);

eval { $e->icursor("foo") };
like($@, qr/bad entry index "foo"/);

$e->delete(0,"end");
$e->insert("end", "01234567890");
$e->icursor(4);
is($e->index('insert'), 4, "Index method");

eval { $e->in };
like($@, qr/Can\'t locate(?: file)? auto\/Tk\/Entry\/in\.al/, "Invalid abbreviated method name (index)");

eval { $e->index };
like($@, qr/wrong \# args: should be ".* index string"/, "index error message");

eval { $e->index("foo") };
like($@, qr/bad entry index "foo"/);

is($e->index(0), 0);

$e->delete(0,"end");
$e->insert(0, "abc\x{4e4e}\x{0153}def");
is_deeply([$e->index(3), $e->index(4), $e->index("end")],[3,4,8], "Index with unicode characters");

eval { $e->insert(qw(a)) };
like($@, qr/wrong \# args: should be ".* insert index text"/, "insert error message");

eval { $e->insert(qw(a b c)) };
like($@, qr/wrong \# args: should be ".* insert index text"/);

eval { $e->insert(qw(foo Text)) };
like($@, qr/bad entry index "foo"/);

$e->delete(0,"end");
$e->insert("end", "01234567890");
$e->insert(3, "xxx");
is($e->get, '012xxx34567890', "get after insert method call");

$e->delete(0,"end");
$e->insert("end", "01234567890");
$e->configure(qw(-state disabled));
$e->insert(qw(3 xxx));
$e->configure(qw(-state normal));
is($e->get, "01234567890");

eval { $e->insert(qw(a b c)) };
like($@, qr/wrong \# args: should be ".* insert index text"/);

eval { $e->scan(qw(a)) };
like($@, qr/wrong \# args: should be ".* scan mark\|dragto x"/, "scan error message");

eval { $e->scan(qw(a b c)) };
like($@, qr/wrong \# args: should be ".* scan mark\|dragto x"/);

eval { $e->scan(qw(foobar 20)) };
like($@, qr/bad scan option "foobar": must be mark or dragto/);

eval { $e->scan(qw(mark 20.1)) };
is($@, '', "Correct mark method call");

# This test is non-portable because character sizes vary.

$e->delete(qw(0 end));
$e->update;
$e->insert(end => "This is quite a long string, in fact a ");
$e->insert(end => "very very long string");
$e->scan(qw(mark 30));
$e->scan(qw(dragto 28));
is($e->index('@0'), 2, "Index of a scrolled string");

eval {$e->select };
like($@, qr/Can\'t locate(?: file)? auto\/Tk\/Entry\/select\.al/, "Invalid abbreviated method name (selection)");

eval {$e->selection };
like($@, qr/wrong \# args: should be ".* selection option \?index\?"/, "selection error message");

eval {$e->selection('foo') };
like($@, qr/bad selection option "foo": must be adjust, clear, from, present, range, or to/);

eval { $e->selection("clear", "gorp") };
like($@, qr/wrong \# args: should be ".* selection clear"/);

$e->delete(0, "end");
$e->insert("end", "01234567890");
$e->selection("from", 1);
$e->selection("to", 4);
$e->update;
$e->selection("clear");
eval { $mw->SelectionGet };
like($@, qr/PRIMARY selection doesn\'t exist or form "(UTF8_)?STRING" not defined/);
is($mw->SelectionOwner, $e, "Expected selection owner");

eval { $e->selection("present", "foo") };
like($@, qr/wrong \# args: should be ".* selection present"/);

$e->delete(0, "end");
$e->insert("end", "01234567890");
$e->selectionFrom(3);
$e->selectionTo(6);
ok($e->selectionPresent, "Selection is now present");

$e->delete(0, "end");
$e->insert("end", "01234567890");
$e->selectionFrom(3);
$e->selectionTo(6);
$e->configure(-exportselection => 0);
ok($e->selection('present'), "Selection is also present with no exportselection");

$e->configure(-exportselection => 1);

$e->delete(0, "end");
$e->insert("end", "01234567890");
$e->selectionFrom(3);
$e->selectionTo(6);
$e->delete(0,"end");
ok(!$e->selectionPresent, "Selection is not present, -exportselection set to true");

eval { $e->selectionAdjust("x") };
like($@, qr/bad entry index "x"/, "selectionAdjust error message");

eval { $e->selection(qw(adjust 2 3)) };
like($@, qr/wrong \# args: should be ".* selection adjust index"/);

$e->delete(0, "end");
$e->insert("end", "01234567890");
$e->selectionFrom(1);
$e->selection(qw(to 5));
$e->update;
$e->selectionAdjust(4);
is($mw->SelectionGet, "123", "Expected result with selectionGet");

$e->delete(0, "end");
$e->insert("end", "01234567890");
$e->selectionFrom(1);
$e->selection(qw(to 5));
$e->update;
$e->selectionAdjust(2);
is($mw->SelectionGet, "234");

eval { $e->selectionFrom(qw(2 3)) };
like($@, qr/wrong \# args: should be ".* selection from index"/, "selectionFrom error message");

eval { $e->selection(qw(range 2)) };
like($@, qr/wrong \# args: should be ".* selection range start end"/, "selectionRange error message");

eval { $e->selection(qw(range 2 3 4)) };
like($@, qr/wrong \# args: should be ".* selection range start end"/);

$e->delete(0, "end");
$e->insert("end", "01234567890");
$e->selectionFrom(1);
$e->selection(qw(to 5));
$e->selection(qw(range 4 4 ));
eval { $e->index("sel.first") };
like($@, qr/selection isn\'t in widget/);

$e->delete(0, "end");
$e->insert("end", "0123456789");
$e->selectionFrom(3);
$e->selection(qw(to 7));
$e->selection(qw(range 2 9));
is($e->index("sel.first"), 2, "Index of selection");
is($e->index("sel.last"), 9);
is($e->index("anchor"), 3);

$e->delete(qw(0 end));
$e->insert(end => "This is quite a long text string, so long that it ");
$e->insert(end => "runs off the end of the window quite a bit.");

eval { $e->selectionTo(2,3) };
like($@, qr/wrong \# args: should be ".* selection to index"/);

$e->xview(5);
is_deeply([map { substr($_, 0, 8) } $e->xview],["0.053763","0.268817"], "Expected xview result");

eval { $e->xview(qw(gorp)) };
like($@, qr/bad entry index "gorp"/, "xview error message");

$e->xview(0);
$e->icursor(10);
$e->xview('insert');
is_deeply([map { substr($_, 0, 7) } $e->xview],["0.10752","0.32258"]);

eval { $e->xviewMoveto(qw(foo bar)) };
like($@, qr/wrong \# args: should be ".* xview moveto fraction"/);

eval { $e->xview(qw(moveto foo)) };
like($@, qr/\'foo\' isn\'t numeric/);

$e->xviewMoveto(0.5);
is_deeply([map { substr($_, 0, 7) } $e->xview],["0.50537","0.72043"]);

eval { $e->xviewScroll(24) };
like($@, qr/wrong \# args: should be ".* xview scroll number units\|pages"/, "xviewScroll error message");

eval { $e->xviewScroll(qw(gorp units)) };
like($@, qr/\'gorp\' isn\'t numeric/);

$e->xviewMoveto(0);
$e->xview(qw(scroll 1 pages));
is_deeply([map { substr($_, 0, 7) } $e->xview],["0.19354","0.40860"]);

$e->xview(qw(moveto .9));
$e->update;
$e->xview(qw(scroll -2 p));
is_deeply([map { substr($_, 0, 7) } $e->xview],["0.39784","0.61290"]);

$e->xview(30);
$e->update;
$e->xview(qw(scroll 2 units));
is($e->index('@0'), 32);

$e->xview(30);
$e->update;
$e->xview(qw(scroll -1 units));
is($e->index('@0'), 29);

eval { $e->xviewScroll(23,"foobars") };
like($@, qr/bad argument "foobars": must be units or pages/);

eval { $e->xview(qw(eat 23 hamburgers)) };
like($@, qr/unknown option "eat": must be moveto or scroll/);

$e->xview(0);
$e->update;
$e->xview(-4);
is($e->index('@0'), 0);

$e->xview(300);
is($e->index('@0'), 73);

{
    $e->insert(10, "\x{4e4e}");

    my @x;
    $e->xviewMoveto(0.1);
    push @x, ($e->xview)[0];
    $e->xviewMoveto(0.11);
    push @x, ($e->xview)[0];
    $e->xviewMoveto(0.12);
    push @x, ($e->xview)[0];

    is_deeply([map { substr($_, 0, 7) } @x],["0.09574","0.10638","0.11702"], "xviewMoveto with unicode character");
}

eval { $e->gorp };
like($@, qr/Can\'t locate(?: file)? auto\/Tk\/Entry\/gorp\.al/, "Invalid method");

# The test below doesn't actually check anything directly, but if run
# with Purify or some other memory-allocation-checking program it will
# ensure that resources get properly freed.

eval { $e->destroy };
my $x;
$e = $mw->Entry(-textvariable => \$x, -show => '*')->pack;
$e->insert('end', "Sample text");
$e->update;
$e->destroy;

my $f = $mw->Frame(qw(-width 200 -height 50 -relief raised -bd 2))
    ->pack(-side => "right");

#eval { $e->destroy };
$x = 12345;
$e = $mw->Entry(-textvariable => \$x);
is($e->get, "12345", "-textvariable check");

eval { $e->destroy };
$x = "12345";
$e = $mw->Entry(-textvariable => \$x);
my $y = "abcde";
$e->configure(-textvariable => \$y);
$x = 54321;
is($e->get, "abcde");

eval { $e->destroy };
undef $x;
$e = $mw->Entry;
$e->configure(-textvariable => \$x);
$e->insert(0, "Some text");
is($x, "Some text");

eval { $e->destroy };
undef $x;
$e = $mw->Entry;
$e->insert(0, "Some text");
$e->configure(-textvariable => \$x);
#is($x, "Some text"); # XXX does not work with Perl/Tk!

sub override {
    $x = 12345;
}

## XXX traceVariable does not work?
eval { $e->destroy };
undef $x;
$mw->traceVariable(\$x, 'w' => \&override);
$e = $mw->Entry;
$e->configure(-textvariable => \$x);
$e->insert('0', "Some text");
my @result = ($x,$e->get);
undef $x;
is($result[0], "12345", "-textvariable with traceVariable");
is($result[1], "12345");

eval { $e->destroy };
$e = $mw->Entry(-exportselection => 0)->pack;
$e->insert(qw(end 0123456789));
$sel->selectionFrom(0);
$sel->selectionTo(10);
is($mw->SelectionGet, "This is so");
$e->selectionFrom(1);
$e->selectionTo(5);
is($mw->SelectionGet, "This is so");
$e->configure(-exportselection => 1);
is($mw->SelectionGet, "1234");

eval { $e->destroy };
$e = $mw->Entry->pack;
$e->insert(qw(end 0123456789));
$e->selectionFrom(1);
$e->selectionTo(5);
$e->configure(-exportselection => 0);
eval { $mw->SelectionGet };
like($@, qr/PRIMARY selection doesn\'t exist or form "(UTF8_)?STRING" not defined/);
is($e->index("sel.first"), 1);
is($e->index("sel.last"), 5);

eval { $e->destroy };
$e = $mw->Entry(-font => $fixed, qw(-width 4 -xscrollcommand), \&scroll)->pack;
$e->insert(qw(end 01234567890));
$e->update;
$e->configure(qw(-width 5));
if (!do {
    local $TODO;
    TODO_xscrollcommand_problem;
    is_deeply([map { substr($_, 0, 8) } @scrollInfo], [0,0.363636]);
}) {
    diag "Scrollinfo not as expected (after insert): <@scrollInfo>"
}

eval { $e->destroy };

$e = $mw->Entry(-width => 0)->pack;
$e->insert(end => "0123");
$e->update;
$e->configure(-font => $big);
$e->update;
SKIP: {
    skip($skip_wm_test, 1)
	if !$do_wm_test;
    like($e->geometry, qr/62x(?:29|3\d)\+0\+0/, "Geometry check"); # under kwin the result is 62x29+0+0, otherwise 62x3\d+0+0
}

eval { $e->destroy };
$e = $mw->Entry(-font => $fixed, -bd => 2, -relief => "raised")->pack;
$e->insert(end => "0123");
$e->update;
is($e->index('@10'), 0, "index with raised relief");
is($e->index('@11'), 0);
is($e->index('@12'), 1);
is($e->index('@13'), 1);

eval { $e->destroy };
$e = $mw->Entry(-font => $fixed, -bd => 2, -relief => "flat")->pack;
$e->insert(end => "0123");
$e->update;
is($e->index('@10'), 0, "index with flat relief");
is($e->index('@11'), 0);
is($e->index('@12'), 1);
is($e->index('@13'), 1);

# If "0" in selected font had 0 width, caused divide-by-zero error.

eval { $e->destroy };
$e = $mw->Entry(-font => '{open look glyph}')->pack;
$e->scan('dragto', 30);
$e->update;

# No tests for DisplayEntry.

eval { $e->destroy };
$e = $mw->Entry(-font => $fixed, -bd => 2, -relief => "raised", -width => 20, -highlightthickness => 3)->pack;
$e->insert("end", "012\t45");
$e->update;
is($e->index('@61'), 3, "index with highlightthickness");
is($e->index('@62'), 4);

eval { $e->destroy };
$e = $mw->Entry(-font => $fixed, qw(-bd 2 -relief raised -width 20 -justify center -highlightthickness 3))->pack;
$e->insert("end", "012\t45");
$e->update;
is($e->index('@96'), 3, "index with center justify");
is($e->index('@97'), 4);

eval { $e->destroy };
$e = $mw->Entry(-font => $fixed, qw(-bd 2 -relief raised -width 20 -justify right -highlightthickness 3))->pack;
$e->insert("end", "012\t45");
$e->update;
is($e->index('@131'), 3, "index with right justify");
is($e->index('@132'), 4);

eval { $e->destroy };
$e = $mw->Entry(-font => $fixed, qw(-bd 2 -relief raised -width 5))->pack;
$e->insert(qw(end 01234567890));
$e->update;
$e->xview(6);
is($e->index('@0'), 6);

eval { $e->destroy };
$e = $mw->Entry(-font => $fixed, qw(-bd 2 -relief raised -width 5))->pack;
$e->insert(qw(end 01234567890));
$e->update;
$e->xview(7);
is($e->index('@0'), 6);

eval { $e->destroy };
$e = $mw->Entry(-font => $fixed, qw(-bd 2 -relief raised -width 10))->pack;
$e->insert(qw(end), "01234\t67890");
$e->update;
$e->xview(3);
is($e->index('@39'), 5, "index with tabulator in entry");
is($e->index('@40'), 6);

SKIP: {
    skip($skip_wm_test, 10)
	if !$do_wm_test;

    eval { $e->destroy };
    $e = $mw->Entry(-font => $big, qw(-bd 3 -relief raised -width 5))->pack;
    $e->insert(qw(end), "01234567");
    $e->update;
    is($e->reqwidth, 77, "Expected reqwidth");
    is($e->reqheight, 39, "Expected reqheight");

    eval { $e->destroy };
    $e = $mw->Entry(-font => $big, qw(-bd 3 -relief raised -width 0))->pack;
    $e->insert(qw(end), "01234567");
    $e->update;
    is($e->reqwidth, 116);
    is($e->reqheight, 39);

    eval { $e->destroy };
    $e = $mw->Entry(-font => $big, qw(-bd 3 -relief raised -width 0 -highlightthickness 2))->pack;
    $e->update;
    is($e->reqwidth, 25);
    is($e->reqheight, 39);

    eval { $e->destroy };
    $e = $mw->Entry(qw(-bd 1 -relief raised -width 0 -show .))->pack;
    $e->insert(0, "12345");
    $e->update;
    is($e->reqwidth, 23);
    $e->configure(-show => 'X');
    is($e->reqwidth, 53);
    #$e->configure(-show => '');
    #is($e->reqwidth, 43);

    eval { $e->destroy };
    $e = $mw->Entry(qw(-bd 1 -relief raised -width 0 -show .),
		    -font => 'helvetica 12')->pack;
    $e->insert(0, "12345");
    $e->update;
    is($e->reqwidth, 8+5*$mw->fontMeasure("helvetica 12", "."));
    $e->configure(-show => 'X');
    is($e->reqwidth, 8+5*$mw->fontMeasure("helvetica 12", "X"));
    #$e->configure(-show => '');
    #is($e->reqwidth, 8+$mw->fontMeasure("helvetica 12", "12345"));
}

eval { $e->destroy };
my $contents;
$e = $mw->Entry(qw(-width 10 -font), $fixed, -textvariable => \$contents,
		-xscrollcommand => \&scroll)->pack;
$e->focus;
$e->delete(0, "end");
$e->insert(0, "abcde");
$e->insert(2, "XXX");
$e->update;
is($e->get, "abXXXcde");
is($contents, "abXXXcde");
if (!do {
    local $TODO;
    TODO_xscrollcommand_problem;
    is(join(" ", @scrollInfo), "0 1", "Result collected in -xscrollcommand callback");
}) {
    diag "Scrollinfo not as expected (after delete/insert): <@scrollInfo>";
}

$e->delete(0, "end");
$e->insert(0, "abcde");
$e->insert(500, "XXX");
$e->update;
is($e->get, "abcdeXXX");
is($contents, "abcdeXXX");
is(join(" ", @scrollInfo), "0 1");

$e->delete(0, "end");
$e->insert(0, "0123456789");
$e->selectionFrom(2);
$e->selectionTo(6);
$e->insert(2, "XXX");
is($e->index("sel.first"), 5);
is($e->index("sel.last"), 9);
$e->selectionTo(8);
is($e->index("sel.first"), 5);
is($e->index("sel.last"), 8);

$e->delete(0, "end");
$e->insert(0, "0123456789");
$e->selectionFrom(2);
$e->selectionTo(6);
$e->insert(3, "XXX");
is($e->index("sel.first"), 2);
is($e->index("sel.last"), 9);
$e->selectionTo(8);
is($e->index("sel.first"), 2);
is($e->index("sel.last"), 8);

$e->delete(0, "end");
$e->insert(0, "0123456789");
$e->selectionFrom(2);
$e->selectionTo(6);
$e->insert(5, "XXX");
is($e->index("sel.first"), 2);
is($e->index("sel.last"), 9);
$e->selectionTo(8);
is($e->index("sel.first"), 2);
is($e->index("sel.last"), 8);

$e->delete(0, "end");
$e->insert(0, "0123456789");
$e->selectionFrom(2);
$e->selectionTo(6);
$e->insert(6, "XXX");
is($e->index("sel.first"), 2);
is($e->index("sel.last"), 6);
$e->selectionTo(5);
is($e->index("sel.first"), 2);
is($e->index("sel.last"), 5);

$e->delete(0, "end");
$e->insert(0, "0123456789");
$e->icursor(4);
$e->insert(4, "XXX");
is($e->index("insert"), 7);

$e->delete(0, "end");
$e->insert(0, "0123456789");
$e->icursor(4);
$e->insert(5, "XXX");
is($e->index("insert"), 4);

$e->delete(0, "end");
$e->insert(0, "This is a very long string");
$e->update;
$e->xview(4);
$e->insert(qw(3 XXX));
is($e->index('@0'), 7);

$e->delete(0, "end");
$e->insert(0, "This is a very long string");
$e->update;
$e->xview(4);
$e->insert(qw(4 XXX));
is($e->index('@0'), 4);

$e->delete(0, "end");
$e->insert(0, "xyzzy");
$e->update;
$e->insert(2, "00");
## XXX is($e->reqwidth, 59);

$e->delete(qw(0 end));
$e->insert(qw(0 abcde));
$e->delete(qw(2 4));
$e->update;
is($e->get, "abe");
is($contents, "abe");
is($scrollInfo[0], 0);
is($scrollInfo[1], 1);

$e->delete(qw(0 end));
$e->insert(qw(0 abcde));
$e->delete(qw(-2 2));
$e->update;
is($e->get, "cde");
is($contents, "cde");
is($scrollInfo[0], 0);
is($scrollInfo[1], 1);

$e->delete(qw(0 end));
$e->insert(qw(0 abcde));
$e->delete(qw(3 1000));
$e->update;
is($e->get, "abc");
is($contents, "abc");
is($scrollInfo[0], 0);
is($scrollInfo[1], 1);

$e->delete(qw(0 end));
$e->insert(qw(0 0123456789abcde));
$e->selection(qw(from 3));
$e->selection(qw(to 8));
$e->delete(qw(1 3));
$e->update;
is($e->index("sel.first"), 1);
is($e->index("sel.last"), 6);
$e->selectionTo(5);
is($e->index("sel.first"), 1);
is($e->index("sel.last"), 5);

$e->delete(qw(0 end));
$e->insert(qw(0 0123456789abcde));
$e->selection(qw(from 3));
$e->selection(qw(to 8));
$e->delete(qw(1 4));
$e->update;
is($e->index("sel.first"), 1);
is($e->index("sel.last"), 5);
$e->selectionTo(4);
is($e->index("sel.first"), 1);
is($e->index("sel.last"), 4);

$e->delete(qw(0 end));
$e->insert(qw(0 0123456789abcde));
$e->selection(qw(from 3));
$e->selection(qw(to 8));
$e->delete(qw(1 7));
$e->update;
is($e->index("sel.first"), 1);
is($e->index("sel.last"), 2);
$e->selectionTo(5);
is($e->index("sel.first"), 1);
is($e->index("sel.last"), 5);

$e->delete(qw(0 end));
$e->insert(qw(0 0123456789abcde));
$e->selection(qw(from 3));
$e->selection(qw(to 8));
$e->delete(qw(1 8));
eval { $e->index("sel.first") };
like($@, qr/selection isn\'t in widget/, "Expected error, no selection");

$e->delete(qw(0 end));
$e->insert(qw(0 0123456789abcde));
$e->selection(qw(from 3));
$e->selection(qw(to 8));
$e->delete(qw(3 7));
$e->update;
is($e->index("sel.first"), 3);
is($e->index("sel.last"), 4);
$e->selectionTo(8);
is($e->index("sel.first"), 3);
is($e->index("sel.last"), 8);

$e->delete(qw(0 end));
$e->insert(qw(0 0123456789abcde));
$e->selection(qw(from 3));
$e->selection(qw(to 8));
$e->delete(qw(3 8));
eval { $e->index("sel.first") };
like($@, qr/selection isn\'t in widget/);

$e->delete(qw(0 end));
$e->insert(qw(0 0123456789abcde));
$e->selection(qw(from 8));
$e->selection(qw(to 3));
$e->delete(qw(5 8));
$e->update;
is($e->index("sel.first"), 3);
is($e->index("sel.last"), 5);
$e->selectionTo(8);
is($e->index("sel.first"), 5);
is($e->index("sel.last"), 8);

$e->delete(qw(0 end));
$e->insert(qw(0 0123456789abcde));
$e->selection(qw(from 8));
$e->selection(qw(to 3));
$e->delete(qw(8 10));
$e->update;
is($e->index("sel.first"), 3);
is($e->index("sel.last"), 8);
$e->selectionTo(4);
is($e->index("sel.first"), 4);
is($e->index("sel.last"), 8);

$e->delete(qw(0 end));
$e->insert(qw(0 0123456789abcde));
$e->icursor(4);
$e->delete(qw(1 4));
is($e->index("insert"), 1);

$e->delete(qw(0 end));
$e->insert(qw(0 0123456789abcde));
$e->icursor(4);
$e->delete(qw(1 5));
is($e->index("insert"), 1);

$e->delete(qw(0 end));
$e->insert(qw(0 0123456789abcde));
$e->icursor(4);
$e->delete(qw(4 6));
is($e->index("insert"), 4);

$e->delete(qw(0 end));
$e->insert(qw(0), "This is a very long string");
$e->xview(4);
$e->delete(qw(1 4));
is($e->index('@0'), 1);

$e->delete(qw(0 end));
$e->insert(qw(0), "This is a very long string");
$e->xview(4);
$e->delete(qw(1 5));
is($e->index("\@0"), 1);

$e->delete(qw(0 end));
$e->insert(qw(0), "This is a very long string");
$e->xview(4);
$e->delete(qw(4 6));
is($e->index("\@0"), 4);

$e->configure(qw(-width 0));

$e->delete(qw(0 end));
$e->insert(0, "xyzzy");
$e->update;
$e->delete(qw(2 4));
is($e->reqwidth, 31);

eval { $e->destroy };

sub _override2 {
    $x = "12345";
}
undef $x;
$mw->traceVariable(\$x, 'w', \&_override2);
$e = $mw->Entry(-textvariable => \$x);
$e->insert(0, "foo");
is($x, 12345);
is($e->get, 12345);
undef $x;

Tk::catch {$e->destroy};
$e = $mw->Entry->pack;
$e->configure(-width => 0);

$x = "abcde";
$y = "ab";
$e->configure(-textvariable => \$x);
$e->update;
$e->configure(-textvariable => \$y);
$e->update;
is($e->get, "ab");
# On Unix/X11 and Windows it's 24, on cygwin/X11 with Xvfb running it's 25,
# on Mac OS X with XFT=1 and a remote Xserver it's 23:
cmp_ok($e->reqwidth, ">=", 23);
cmp_ok($e->reqwidth, "<=", 25);

$mw->traceVdelete(\$x); # XXX why?

Tk::catch {$e->destroy};
$e = $mw->Entry(-textvariable => \$x);
$e->insert(0, "abcdefghjklmnopqrstu");
$e->selection(qw(range 4 10));
$x = "a";
eval { $e->index("sel.first") };
like($@, qr/selection isn\'t in widget/);

Tk::catch {$e->destroy};
$e = $mw->Entry(-textvariable => \$x);
$e->insert(0, "abcdefghjklmnopqrstu");
$e->selection(qw(range 4 10));
$x = "abcdefg";
is($e->index("sel.first"), 4);
is($e->index("sel.last"), 7);

Tk::catch {$e->destroy};
$e = $mw->Entry(-textvariable => \$x);
$e->insert(0, "abcdefghjklmnopqrstu");
$e->selection(qw(range 4 10));
$x = "abcdefghijklmn";
is($e->index("sel.first"), 4);
is($e->index("sel.last"), 10);

Tk::catch {$e->destroy};
$e = $mw->Entry(-textvariable => \$x)->pack;
$e->insert(0, "abcdefghjklmnopqrstu");
$e->xview(10);
$e->update;
$x = "abcdefg";
$e->update;
is($e->index('@0'), 0);

Tk::catch {$e->destroy};
$e = $mw->Entry(-font => $fixed, -width => 10, -textvariable => \$x)->pack;
$e->insert(0, "abcdefghjklmnopqrstu");
$e->xview(10);
$e->update;
$x = "1234567890123456789012";
$e->update;
is($e->index('@0'), 10);

Tk::catch {$e->destroy};
$e = $mw->Entry(-font => $fixed, -width => 10, -textvariable => \$x)->pack;
$e->insert(0, "abcdefghjklmnopqrstu");
$e->icursor(5);
$x = "123";
is($e->index("insert"), 3);

Tk::catch {$e->destroy};
$e = $mw->Entry(-font => $fixed, -width => 10, -textvariable => \$x)->pack;
$e->insert(0, "abcdefghjklmnopqrstuvwxyz");
$e->icursor(5);
$x = "123456";
is($e->index("insert"), 5);

Tk::catch {$e->destroy};
$e = $mw->Entry;
$e->insert(0, "abcdefg");
$e->destroy;
$mw->update;

$_->destroy for ($mw->children);
my $e1 = $mw->Entry(-fg => '#112233');
is(($mw->children)[0], $e1);
$e1->destroy;
is(scalar($mw->children), undef); # XXX why not 0?
create_placeholder_widget;

$e = $mw->Entry(-font => $fixed, qw(-width 5 -bd 2 -relief sunken))->pack;
$e->insert(qw(0 012345678901234567890));
$e->xview(4);
$e->update;
is($e->index("end"), 21);

eval { $e->index("abogus") };
like($@, qr/bad entry index "abogus"/);

$e->selection(qw(from 1));
$e->selection(qw(to 6));
is($e->index(qw(anchor)), 1);

$e->selection(qw(from 4));
$e->selection(qw(to 1));
is($e->index(qw(anchor)), 4);

$e->selection(qw(from 3));
$e->selection(qw(to 15));
$e->selection(qw(adjust 4));
is($e->index(qw(anchor)), 15);

eval { $e->index("ebogus") };
like($@, qr/bad entry index "ebogus"/);

$e->icursor(2);
is($e->index('insert'), 2);

eval { $e->index("ibogus") };
like($@, qr/bad entry index "ibogus"/);

$e->selectionFrom(1);
$e->selectionTo(6);
is($e->index("sel.first"), 1);
is($e->index("sel.last"), 6);

$mw->SelectionClear($e);

if ($^O ne 'MSWin32') {
    # On unix, when selection is cleared, entry widget's internal
    # selection range is reset.

    eval { $e->index("sel.first") };
    like($@, qr/selection isn\'t in widget/);
    pass("Dummy to align number of tests with MSWin32");

} else {
    # On mac and pc, when selection is cleared, entry widget remembers
    # last selected range.  When selection ownership is restored to
    # entry, the old range will be rehighlighted.

    is($e->getSelected, '12345');
    is($e->index("sel.first"), 1);
}

if ($^O ne 'MSWin32') {
    eval { $e->index("sbogus") };
    like($@, qr/selection isn\'t in widget/);
} else {
    eval { $e->index("sbogus") };
    like($@, qr/bad entry index "sbogus"/);
}

eval { $e->index('@xyz') };
like($@, qr/bad entry index "\@xyz"/);

is($e->index('@4'), 4);
is($e->index('@11'), 4);
is($e->index('@12'), 5);
is($e->index('@' . ($e->width-6)), 8);
is($e->index('@' . ($e->width-5)), 9);
is($e->index('@1000'), 9);

eval { $e->index('1xyz') };
like($@, qr/bad entry index "1xyz"/);

is($e->index(-10), 0);
is($e->index(12), 12);
is($e->index(49), 21);

Tk::catch { $e->destroy };
$e = $mw->Entry(-show => ".");
$e->insert(qw(0 XXXYZZY));
$e->pack;
$e->update;
SKIP: {
    skip($skip_font_test, 2)
	if !$do_font_test;
    is($e->index('@7'), 0);
    is($e->index('@8'), 1);
}

# XXX Still need to write tests for EntryScanTo and EntrySelectTo.

$x = "";
for my $i (1 .. 500) {
    $x .= "This is line $i, out of 500\n";
}

Tk::catch { $e->destroy };
$e = $mw->Entry;
$e->insert(end => "This is a test string");
$e->selection(qw(from 1));
$e->selection(qw(to 18));
is($mw->SelectionGet, "his is a test str");

Tk::catch { $e->destroy };
$e = $mw->Entry(-show => "*");
$e->insert(end => "This is a test string");
$e->selection(qw(from 1));
$e->selection(qw(to 18));
is($mw->SelectionGet, "*****************");

Tk::catch { $e->destroy };
$e = $mw->Entry;
$e->insert("end", $x);
$e->selectionFrom(0);
$e->selectionTo("end");
is($mw->SelectionGet, $x);

Tk::catch { $e->destroy };
$e = $mw->Entry;
$e->insert(0, "Text");
$e->selectionFrom(0);
$e->selectionTo(4);
is($mw->SelectionGet, "Text");
$mw->SelectionClear;
$e->selectionFrom(0);
$e->selectionTo(4);
is($mw->SelectionGet, "Text");

# No tests for EventuallyRedraw.

Tk::catch {$e->destroy};
$e = $mw->Entry(qw(-width 10 -xscrollcommand), \&scroll)->pack;
$e->update;

$e->delete(qw(0 end));
$e->insert(qw(0 .............................));
SKIP: {
    skip($skip_wm_font_test, 1)
	if !$do_font_test || !$do_wm_test;
    is_deeply([map { substr($_, 0, 8) } $e->xview], [0, 0.827586]);
}

$e->delete(qw(0 end));
$e->insert(qw(0 XXXXXXXXXXXXXXXXXXXXXXXXXXXXX));
my $Xw = join(" ", map { substr($_, 0, 8) } $e->xview);

$e->configure(-show => 'X');
$e->delete(qw(0 end));
$e->insert(qw(0 .............................));
is(join(" ", map { substr($_, 0, 8) } $e->xview), $Xw);

$e->configure(-show => '.');
$e->delete(qw(0 end));
$e->insert(qw(0 XXXXXXXXXXXXXXXXXXXXXXXXXXXXX));
SKIP: {
    skip($skip_wm_font_test, 1)
	if !$do_font_test || !$do_wm_test;
    is_deeply([map { substr($_, 0, 8) } $e->xview], [0, 0.827586]);
}

$e->configure(-show => "");
$e->delete(qw(0 end));
is(($e->xview)[$_], $_) for (0 .. 1);

Tk::catch {$e->destroy};
$e = $mw->Entry(qw(-width 10 -xscrollcommand), \&scroll, -font => $fixed)->pack;
$e->update;

$e->delete(qw(0 end));
$e->insert(qw(0 123));
$e->update;
is(join(" ",@scrollInfo),"0 1");

$e->delete(qw(0 end));
$e->insert(qw(0 0123456789abcdef));
$e->xview(3);
$e->update;
SKIP: {
    skip($skip_wm_font_test, 1)
	if !$do_font_test || !$do_wm_test;
    is_deeply([@scrollInfo],[0.1875, 0.8125]);
}

$e->delete(qw(0 end));
$e->insert(qw(0 abcdefghijklmnopqrs));
$e->xview(6);
$e->update;
SKIP: {
    skip($skip_wm_font_test, 1)
	if !$do_font_test || !$do_wm_test;
    is_deeply([map { sprintf "%8f", $_ } @scrollInfo],[0.315789, 0.842105]);
}

Tk::catch {$e->destroy};
{
    my $e;
    my $err;
    eval {
	local *Tk::Error = sub { $err = $_[1] };
	$e = $mw->Entry(qw(-width 5 -xscrollcommand thisisnotacommand))->pack;
	$e->update;
    };
    if (!do {
	local $TODO;
	TODO_xscrollcommand_problem;
	like($err, qr/Undefined subroutine &main::thisisnotacommand/, "Expected invalid -xscrollcommand callback");;
    }) {
	diag "Undefined subroutine thisisnotacommand not detected";
    }
    $e->destroy;
}

{
    # A variation of the previous test. Define -xscrollcommand after
    # the widget creation, and force the callback by using a xviewMove
    # call.
    my $e;
    my $err;
    eval {
	local *Tk::Error = sub { $err = $_[1] };
	$e = $mw->Entry(qw(-width 5))->pack;
	$e->update;
	$e->configure(qw(-xscrollcommand thisisnotacommand));
	$e->insert("end", "more than 5 chars");
	$e->xviewMoveto(1); # should really scroll
	$e->update;
    };
    if (!do {
	local $TODO;
	TODO_xscrollcommand_problem;
    }) {
	like($err, qr/Undefined subroutine &main::thisisnotacommand/, "Expected invalid -xscrollcommand callback");
    }
    $e->destroy;
}

#      pack .e
#      update
#      rename bgerror {}
#      list $x $errorInfo
#  } {{invalid command name "thisisnotacommand"} {invalid command name "thisisnotacommand"
#      while executing
#  "thisisnotacommand 0 1"
#      (horizontal scrolling command executed by entry)}}

## XXX no interp hidden with Perl/Tk?
#set l [interp hidden]
#eval destroy [winfo children .]
#  test entry-18.1 {Entry widget vs hiding} {
#      catch {destroy .e}
#      entry .e
#      interp hide {} .e
#      destroy .e
#      list [winfo children .] [interp hidden]
#  } [list {} $l]

######################################################################
# Additional tests

{
    my $e = $mw->Entry;
    $e->validate; # check whether validate method is defined
    pass("validate method seems to be defined");
}

__END__

