#!/usr/bin/perl -w
# -*- perl -*-

# This file is the translation of a Tcl script to test out the "listbox"
# command of Tk.  It is organized in the standard fashion for Tcl tests.
#
# Copyright (c) 1993-1994 The Regents of the University of California.
# Copyright (c) 1994-1997 Sun Microsystems, Inc.
# Copyright (c) 1998-1999 by Scriptics Corporation.
# All rights reserved.
#
# RCS: @(#) $Id: listbox.t,v 1.2 2002/04/17 21:06:12 eserte Exp $
#
# Translated to perl by Slaven Rezic
#

use strict;
use vars qw($Listbox);

use Tk;
use Tk::Config ();
my $Xft = $Tk::Config::xlib =~ /-lXft\b/;

use FindBin;
use lib "$FindBin::RealBin";
use TkTest qw(is_float wm_info set_have_fixed_font with_fixed_font);

use Getopt::Long;

my $v;
my $visual;

BEGIN {
    $Listbox = "Listbox";
    #$Listbox = "TextList";
    GetOptions("listboxclass=s" => \$Listbox,
	       "v" => \$v,
	       "visual" => \$visual,
	      )
	or die "usage: $0 [-listboxclass baseclass] [-v] [-visual]";

    eval "use Tk::$Listbox";
}

BEGIN {
    if (!eval q{
	use Test::More;
	1;
    }) {
	print "1..0 # skip: no Test::More module\n";
	exit;
    }
}

plan tests => 537;

my $partial_top;
my $partial_lb;

my $listvar_impl = $Tk::VERSION >= 804;

my $mw = new MainWindow;
$mw->geometry('+10+10');
$mw->raise;
## Always use the X11 font, even with Xft, otherwise the measurements would
## be wrong.
#my $fixed = $Xft ? '{Adobe Courier} -12' : 'Courier -12';
my $fixed = "-adobe-courier-medium-r-normal--12-120-75-75-m-*-iso8859-1";
ok(Tk::Exists($mw));

my %wm_info = wm_info($mw);
my $wm_name = $wm_info{name};

#my $kwin_problems     = defined $wm_name && $wm_name eq 'KWin';
my $fluxbox_problems  = defined $wm_name && $wm_name eq 'Fluxbox';
#my $metacity_problems = defined $wm_name && $wm_name eq 'Metacity';
#my $xfwm4_problems    = defined $wm_name && $wm_name eq 'Xfwm4';

# This is probably the same problem as in t/entry.t
sub TODO_xscrollcommand_problem (&) {
    my $code = shift;
    local $TODO;
    #$TODO = "May fail under some conditions (another grab?) on KDE"      if !$TODO && $kwin_problems;
    #$TODO = "May fail under some conditions (another grab?) on Metacity" if !$TODO && $metacity_problems;
    #$TODO = "May fail under some conditions (another grab?) on Fluxbox"  if !$TODO && $fluxbox_problems;
    #$TODO = "May fail under some conditions (another grab?) on xfwm4"    if !$TODO && $xfwm4_problems;
    $TODO = "May fail under some conditions" if $Tk::platform eq 'unix'; # may happen even on fvwm on heavy load
    local $Test::Builder::Level = $Test::Builder::Level + 2;
    $code->();
}

sub TODO_fluxbox_problem (&) {
    my $code = shift;
    local $TODO;
    $TODO = "May fail under some conditions on Fluxbox"  if !$TODO && $fluxbox_problems;
    local $Test::Builder::Level = $Test::Builder::Level + 2;
    $code->();
}

# Create entries in the option database to be sure that geometry options
# like border width have predictable values.
$mw->optionAdd("*Toplevel.borderWidth",0);
$mw->optionAdd("*$Listbox.borderWidth",2);
$mw->optionAdd("*$Listbox.highlightThickness",2);
## Again, prefer the X11 font.
#$mw->optionAdd("*$Listbox.font",
#                $Xft ? '{Adobe Helvetica} -12 bold' :'Helvetica -12 bold');
$mw->optionAdd("*$Listbox.font", 'Helvetica -12 bold');

my $lb = $mw->$Listbox->pack;
ok(Tk::Exists($lb), "Listbox exists");
isa_ok($lb, "Tk::$Listbox");
$lb->update;

my $skip_font_test;
{
    my %fa = ($mw->fontActual($lb->cget(-font)),
	      $mw->fontMetrics($lb->cget(-font)));
    my %expected = (
		    "-weight"	  => "bold",
		    "-underline"  => 0,
		    "-family"	  => "helvetica",
		    "-slant"	  => "roman",
		    "-size"	  => -12,
		    "-overstrike" => 0,
		    "-ascent"	  => 11,
		    "-descent"	  => 3,
		    "-linespace"  => 14,
		    "-fixed"	  => 0,
		   );
    while(my($key,$val) = each %expected) {
	if (lc $val ne lc $fa{$key}) {
	    diag "Value $key does not match: got $fa{$key}, expected $val\n" if $v;
	    $skip_font_test = "font-related tests (proportional font not std helvetica)";
	    last;
	}
    }
}

{
    my $fixed_lb = $mw->$Listbox(-font => $fixed);
    my %fa = ($mw->fontActual($fixed_lb->cget(-font)),
	      $mw->fontMetrics($fixed_lb->cget(-font)));
    my %expected = (
		    "-weight"     => "normal",
		    "-underline"  => 0,
		    "-family"     => "courier",
		    "-slant"      => "roman",
		    "-size"       => -12,
		    "-overstrike" => 0,
		    "-ascent"     => 10,
		    "-descent"    => 3,
		    "-linespace"  => 13,
		    "-fixed"      => 1,
		   );
    my $have_fixed_font = 1;
    while(my($key,$val) = each %expected) {
	if (lc $val ne lc $fa{$key}) {
	    diag "Value $key does not match: got $fa{$key}, expected $val\n" if $v;
	    $have_fixed_font = 0;
	    last;
	}
    }
    set_have_fixed_font($have_fixed_font);
    $fixed_lb->destroy;
}

resetGridInfo();

$mw->Photo("testimage", -file => Tk->findINC("Xcamel.gif"));

use constant SKIP_CGET    => 5;
use constant SKIP_CONF    => 6;

my $testVariable;

foreach my $test
    (
     ['-activestyle', 'under', 'underline', 'foo',
      'bad activestyle "foo": must be dotbox, none, or underline'],
     ['-background', '#ff0000', '#ff0000', 'non-existent',
      'unknown color name "non-existent"'],
     [qw{-bd 4 4 badValue}, q{bad screen distance "badValue"}],
     ['-bg', '#ff0000', '#ff0000', 'non-existent',
      'unknown color name "non-existent"'],
     [qw{-borderwidth 1.3 1 badValue}, q{bad screen distance "badValue"}],
     [qw{-cursor arrow arrow badValue}, q{bad cursor spec "badValue"}],
     [qw{-exportselection yes 1}, ""], # Perl/Tk thinks different about booleans: "xyzzy", q{expected boolean value but got "xyzzy"}],
     ['-fg', '#110022', '#110022', 'bogus', q{unknown color name "bogus"}],
     ['-font', 'Helvetica 12', 'Helvetica 12', '', "font \"\" doesn't exist", 1, 1],
     ['-foreground', '#110022', '#110022', 'bogus',
      q{unknown color name "bogus"}],
     [qw{-height 30 30 20p}, "'20p' isn't numeric"], # in Tcl/Tk nowadays: expected integer but got "20p"
     ['-highlightbackground', '#112233', '#112233', 'ugly',
      q{unknown color name "ugly"}],
     ['-highlightcolor', '#123456', '#123456', 'bogus',
      q{unknown color name "bogus"}],
     [qw{-highlightthickness 6 6 bogus}, q{bad screen distance "bogus"}],
     [qw{-highlightthickness -2 0}, '', ''],
     ['-offset', '1,1', '1,1', 'wrongside',
      'bad offset "wrongside": expected "x,y", n, ne, e, se, s, sw, w, nw, or center'],
     [qw{-relief groove groove 1.5},
      ($Listbox eq 'TextList'
       ? q{bad relief type "1.5": must be flat, groove, raised, ridge, solid, or sunken}
       : ($Tk::VERSION < 803
	  ? q{bad relief type "1.5": must be flat, groove, raised, ridge, solid, or sunken}
	  : q{bad relief "1.5": must be flat, groove, raised, ridge, solid, or sunken})
      )],
     ['-selectbackground', '#110022', '#110022', 'bogus',
      q{unknown color name "bogus"}],
     [qw{-selectborderwidth 1.3 1 badValue},
      q{bad screen distance "badValue"}],
     ['-selectforeground', '#654321', '#654321', 'bogus',
      q{unknown color name "bogus"}],
     [qw{-selectmode string string}, '', ''],
     [qw{-setgrid false 0}, "", "lousy",
      q{expected boolean value but got "lousy"}],
     ['-state', 'disabled', 'disabled', 'foo',
      ($Listbox eq 'TextList'
       ? q{bad state value "foo": must be normal or disabled}
       : 'bad state "foo": must be disabled, or normal'
      )],
     ['-takefocus', "any string", "any string", '', ''],
     ['-tile', 'testimage', 'testimage', 'non-existant',
      'image "non-existant" doesn\'t exist'],
     [qw{-width 45 45 3p}, "'3p' isn't numeric"], # In Tcl/Tk nowadays q{expected integer but got "3p"}
     [qw(-xscrollcommand), q{Some command}, q{Some command}, '', undef, 1, 1],
     [qw(-yscrollcommand), q{Another command}, q{Another command}, '', undef, 1, 1],
     [qw{-listvar}, \$testVariable,  \$testVariable, '', undef],
    ) {
	my $name = $test->[0];

    SKIP: {
	    skip("$name test not supported for $Listbox", 4)
		if ($Listbox eq 'TextList' &&
		    $name =~ /^-(activestyle|bg|fg|foreground|height|selectborderwidth|listvar)$/);

	    skip("$name not implemented on $Tk::VERSION", 4)
		if ($Listbox eq 'Listbox' && $Tk::VERSION < 804 &&
		    $name =~ /^-(activestyle)$/);

	    skip("*TODO* $name not yet implemented on $Tk::VERSION", 4)
		if ($Tk::VERSION >= 804 &&
		    $name =~ /^-(tile|offset)$/);

	    $lb->configure($name, $test->[1]);
	    pass("configure set for $name");
	SKIP: {
		skip("configure test for $name", 1) if $test->[SKIP_CONF];
		is(($lb->configure($name))[4],$test->[2], "configuration option $name");
	    }
	SKIP: {
		skip("cget test for $name", 1) if $test->[SKIP_CGET];
		is($lb->cget($name), $test->[2], "cget call with $name");
	    }
	SKIP: {
		skip("error test for $name", 1) if $test->[3] eq "";
		eval {
		    $lb->configure($name, $test->[3]);
		};
		like($@,qr/$test->[4]/,"error message for $name");
	    }

	    $lb->configure($name, ($lb->configure($name))[3]);
	}
    }

SKIP: {
    skip("only for Listbox, not for $Listbox", 1)
	if ($Listbox ne 'Listbox');

    eval { Tk::listbox() };
    like($@,qr/Usage \$widget->listbox(...)/, "error message");
}

{
    eval {
	$lb->destroy;
	$lb = $mw->$Listbox;
    };
    ok(Tk::Exists($lb));
    is($lb->class, "$Listbox", "Tk class $Listbox");
}

{
    eval {
	$lb->destroy;
	$lb = $mw->$Listbox(-gorp => "foo");
    };
    like($@,
	 ($Tk::VERSION < 803)
	 ? qr/Bad option \`-gorp\'/
	 : qr/unknown option \"-gorp\"/,
	 "error message");
}

ok(!Tk::Exists($lb));

$lb = $mw->$Listbox(-width => 20, -height => 5, -bd => 4,
		    -highlightthickness => 1,
		    -selectborderwidth => 2)->pack;
$lb->insert(0,
	    'el0','el1','el2','el3','el4','el5','el6','el7','el8','el9','el10',
	    'el11','el12','el13','el14','el15','el16','el17');
$lb->update;
eval { $lb->activate };
like($@,qr/wrong \# args: should be "\.listbox.* activate index"/,
     "Listbox activate error message");

eval { $lb->activate("fooey") };
like($@,qr/bad listbox index "fooey": must be active, anchor, end, \@x,y, or a number/);

$lb->activate(3);
is($lb->index("active"), 3, "Listbox activate");

$lb->activate(-1);
is($lb->index("active"), 0);

$lb->activate(30);
is($lb->index("active"), 17);

$lb->activate("end");
is($lb->index("active"), 17);

eval { $lb->bbox };
like($@, qr/wrong \# args: should be "\.listbox.* bbox index"/,
     "Listbox bbox error message");

eval { $lb->bbox(qw/a b/) };
like($@, qr/wrong \# args: should be "\.listbox.* bbox index"/);

eval { $lb->bbox("fooey") };
like($@,qr/bad listbox index "fooey": must be active, anchor, end, \@x,y, or a number/);

$lb->yview(3);
$lb->update;
is($lb->bbox(2), undef, "Listbox bbox");
is($lb->bbox(8), undef);

# Used to generate a core dump before a bug was fixed (the last
# element would be on-screen if it existed, but it doesn't exist).
eval {
    my $l2 = $mw->$Listbox;
    $l2->pack(-side => "top");
    $l2->waitVisibility;
    my $x = $l2->bbox(0);
    $l2->destroy;
};
is($@, '', "No core dump with bbox");

$lb->yview(3);
$lb->update;
SKIP: {
    skip($skip_font_test, 2) if $skip_font_test;
    is_deeply([$lb->bbox(3)], [qw(7 7 17 14)]);
    is_deeply([$lb->bbox(4)], [qw(7 26 17 14)]);
}

$lb->yview(0);
$lb->update;
is($lb->bbox(-1), undef);
SKIP: {
    skip($skip_font_test, 1) if $skip_font_test;
    is_deeply([$lb->bbox(0)], [qw(7 7 17 14)]);
}

$lb->yview("end");
$lb->update;
SKIP: {
    skip($skip_font_test, 2) if $skip_font_test;
    is_deeply([$lb->bbox(17)], [qw(7 83 24 14)]);
    is_deeply([$lb->bbox("end")], [qw(7 83 24 14)]);
}
is($lb->bbox(18), undef);

{
    my $t = $mw->Toplevel;
    $t->geometry("+0+0");
    my $lb = $t->$Listbox(-width => 10,
			 -height => 5);
    $lb->insert(0, "Short", "Somewhat longer",
		"Really, quite a whole lot longer than can possibly fit on the screen",
		"Short");
    $lb->pack;
    $lb->update;
    $lb->xview(moveto => 0.2);
 SKIP: {
	skip($skip_font_test, 1) if $skip_font_test;
	is_deeply([$lb->bbox(2)], [qw(-72 39 393 14)]);
	$t->destroy;
    }
}

mkPartial();
SKIP: {
    skip($skip_font_test, 2) if $skip_font_test;
    is_deeply([$partial_lb->bbox(3)], [qw(5 56 24 14)]);
    is_deeply([$partial_lb->bbox(4)], [qw(5 73 23 14)]);
}

eval { $lb->cget };
like($@,qr/wrong \# args: should be \"\.listbox.* cget option\"/,
     "Listbox cget message");

eval { $lb->cget(qw/a b/) };
like($@,qr/wrong \# args: should be \"\.listbox.* cget option\"/);

eval { $lb->cget(-gorp) };
like($@,qr/unknown option "-gorp"/);

is($lb->cget(-setgrid), 0);
# XXX why 25 in Tk800? probably because of less configuration options!
is(scalar @{[$lb->configure]}, ($Tk::VERSION < 803 ? 25 : 27), "Listbox configure");
is_deeply([$lb->configure(-setgrid)],
	  [qw(-setgrid setGrid SetGrid 0 0)]);
eval { $lb->configure(-gorp) };
like($@,qr/unknown option "-gorp"/);

{
    my $oldbd = $lb->cget(-bd);
    my $oldht = $lb->cget(-highlightthickness);
    $lb->configure(-bd => 3, -highlightthickness => 0);
    is($lb->cget(-bd), 3);
    is($lb->cget(-highlightthickness), 0);
    $lb->configure(-bd => $oldbd);
    $lb->configure(-highlightthickness => $oldht);
}

eval { $lb->curselection("a") };
like($@,qr/wrong \# args: should be \"\.listbox.* curselection\"/,
     "Listbox curselection error message");

$lb->selection("clear", 0, "end");
$lb->selection("set", 3, 6);
$lb->selection("set", 9);
is_deeply([$lb->curselection], [qw(3 4 5 6 9)],
	  "Listbox curselection");

# alternative perl/Tk methods
$lb->selectionClear(0, "end");
$lb->selectionSet(3, 6);
$lb->selectionSet(9);
is_deeply([$lb->curselection], [qw(3 4 5 6 9)]);

eval { $lb->delete };
like($@,qr/wrong \# args: should be \"\.listbox.* delete firstIndex \?lastIndex\?\"/,
   "Listbox delete error message");

eval { $lb->delete(qw/a b c/) };
like($@,qr/wrong \# args: should be \"\.listbox.* delete firstIndex \?lastIndex\?\"/);

eval { $lb->delete("badindex") };
like($@,qr/bad listbox index "badindex": must be active, anchor, end, \@x,y, or a number/);

eval { $lb->delete(2, "123ab") };
like($@,qr/bad listbox index "123ab": must be active, anchor, end, \@x,y, or a number/);

{
    my $l2 = $mw->$Listbox;
    $l2->insert(0, qw(el0 el1 el2 el3 el4 el5 el6 el7));
    $l2->delete(3);
    is($l2->get(2), "el2", "Listbox delete element");
    is($l2->get(3), "el4");
    is($l2->index("end"), "7");
    $l2->destroy;
}

{
    my $l2 = $mw->$Listbox;
    $l2->insert(0, qw(el0 el1 el2 el3 el4 el5 el6 el7));
    $l2->delete(2, 4);
    is($l2->get(1), "el1");
    is($l2->get(2), "el5");
    is($l2->index("end"), "5");
    $l2->destroy;
}

{
    my $l2 = $mw->$Listbox;
    $l2->insert(0, qw(el0 el1 el2 el3 el4 el5 el6 el7));
    $l2->delete(-3, 2);
    is_deeply([$l2->get(0, "end")], [qw(el3 el4 el5 el6 el7)]);
    $l2->destroy;
}

{
    my $l2 = $mw->$Listbox;
    $l2->insert(0, qw(el0 el1 el2 el3 el4 el5 el6 el7));
    $l2->delete(-3, -1);
    is_deeply([$l2->get(0, "end")], [map { "el$_" } (0 .. 7)]);
    is(scalar @{[$l2->get(0, "end")]}, 8);
    $l2->destroy;
}

{
    my $l2 = $mw->$Listbox;
    $l2->insert(0, qw(el0 el1 el2 el3 el4 el5 el6 el7));
    $l2->delete(2, "end");
    is_deeply([$l2->get(0, "end")], [qw(el0 el1)]);
    is(scalar @{[$l2->get(0, "end")]}, 2);
    $l2->destroy;
}

{
    my $l2 = $mw->$Listbox;
    $l2->insert(0, qw(el0 el1 el2 el3 el4 el5 el6 el7));
    $l2->delete(5, 20);
    is_deeply([$l2->get(0, "end")], [qw(el0 el1 el2 el3 el4)]);
    is(scalar @{[$l2->get(0, "end")]}, 5);
    $l2->destroy;
}

{
    my $l2 = $mw->$Listbox;
    $l2->insert(0, qw(el0 el1 el2 el3 el4 el5 el6 el7));
    $l2->delete("end", 20);
    is_deeply([$l2->get(0, "end")], [qw(el0 el1 el2 el3 el4 el5 el6)]);
    is(scalar @{[$l2->get(0, "end")]}, 7);
    $l2->destroy;
}

{
    my $l2 = $mw->$Listbox;
    $l2->insert(0, qw(el0 el1 el2 el3 el4 el5 el6 el7));
    $l2->delete(8, 20);
    is_deeply([$l2->get(0, "end")], [qw(el0 el1 el2 el3 el4 el5 el6 el7)]);
    is(scalar @{[$l2->get(0, "end")]}, 8);
    $l2->destroy;
}

eval { $lb->get };
like($@, $Tk::VERSION < 803
     ? qr/wrong \# args: should be \"\.listbox.* get first \?last\?\"/
     : qr/wrong \# args: should be \"\.listbox.* get firstIndex \?lastIndex\?\"/,
     "Listbox get error message");

eval { $lb->get(qw/a b c/) };
like($@, $Tk::VERSION < 803
     ? qr/wrong \# args: should be \"\.listbox.* get first \?last\?\"/
     : qr/wrong \# args: should be \"\.listbox.* get firstIndex \?lastIndex\?\"/);

eval { $lb->get("badindex") };
like($@ ,qr/bad listbox index "badindex": must be active, anchor, end, \@x,y, or a number/);

eval { $lb->get("end", "bogus") };
like($@ ,qr/bad listbox index "bogus": must be active, anchor, end, \@x,y, or a number/);

{
    my $l2 = $mw->$Listbox;
    $l2->insert(0, qw(el0 el1 el2 el3 el4 el5 el6 el7));
    is($l2->get(0), "el0");
    is($l2->get(3), "el3");
    # This is valid in perl/Tk, but not Tcl/Tk
    is($lb->get("3.4"), "el3", "get with float");
    is($l2->get("end"), "el7");
    $l2->destroy;
}

{
    my $l2 = $mw->$Listbox;
    is($l2->get(0), undef);
    is($l2->get("end"), undef);
    $l2->destroy;
}

{
    my $l2 = $mw->$Listbox;
    $l2->insert(0, qw(el0 el1 el2), "two words", qw(el4 el5 el6 el7));
    is($l2->get(3), "two words");
    is(($l2->get(3, "end"))[0], "two words");
    is_deeply([$l2->get(3, "end")], ['two words', qw(el4 el5 el6 el7)]);
}

is($lb->get(-1), undef);
is($lb->get(-2, -1), undef);
is_deeply([$lb->get(-2, 3)], [qw(el0 el1 el2 el3)]);
is(scalar @{[ $lb->get(-2, 3) ]}, 4);

is_deeply([$lb->get(12, "end")], [qw(el12 el13 el14 el15 el16 el17)]);
is(scalar @{[ $lb->get(12, "end") ]}, 6);
is_deeply([$lb->get(12, 20)], [qw(el12 el13 el14 el15 el16 el17)]);
is(scalar @{[ $lb->get(12, 20) ]}, 6);

is($lb->get("end"), "el17");
is($lb->get(30), undef);
is_deeply([$lb->get(30, 35)], []);

eval { $lb->index };
like($@ ,qr/wrong \# args: should be \"\.listbox.* index index\"/,
     "Listbox index error message");

eval { $lb->index(qw/a b/) };
like($@ ,qr/wrong \# args: should be \"\.listbox.* index index\"/);

eval { $lb->index(qw/@/) };
like($@ ,qr/bad listbox index "\@": must be active, anchor, end, \@x,y, or a number/);

is($lb->index(2), 2);
is($lb->index(-1), -1);
is($lb->index("end"), 18);
is($lb->index(34), 34);

eval { $lb->insert };
like($@ ,qr/wrong \# args: should be \"\.listbox.* insert index \?element element \.\.\.\?\"/,
     "Listbox insert error message");

eval { $lb->insert("badindex") };
like($@ ,qr/bad listbox index "badindex": must be active, anchor, end, \@x,y, or a number/);

{
    my $l2 = $mw->$Listbox;
    $l2->insert("end", qw/a b c d e/);
    $l2->insert(3, qw/x y z/);
    is_deeply([$l2->get(0, "end")], [qw(a b c x y z d e)], "Listbox insert");
    is(scalar @{[ $l2->get(0, "end") ]}, 8);
    $l2->destroy;
}

{
    my $l2 = $mw->$Listbox;
    $l2->insert("end", qw/a b c/);
    $l2->insert(-1, qw/x/);
    is_deeply([$l2->get(0, "end")], [qw(x a b c)]);
    is(scalar @{[ $l2->get(0, "end") ]}, 4);
    $l2->destroy;
}

{
    my $l2 = $mw->$Listbox;
    $l2->insert("end", qw/a b c/);
    $l2->insert("end", qw/x/);
    is_deeply([$l2->get(0, "end")], [qw(a b c x)]);
    is(scalar @{[ $l2->get(0, "end") ]}, 4);
    $l2->destroy;
}

{
    my $l2 = $mw->$Listbox;
    $l2->insert("end", qw/a b c/);
    $l2->insert(43, qw/x/);
    is_deeply([$l2->get(0, "end")], [qw(a b c x)]);
    is(scalar @{[ $l2->get(0, "end") ]}, 4);
    $l2->insert(4, qw/y/);
    is_deeply([$l2->get(0, "end")], [qw(a b c x y)]);
    $l2->insert(6, qw/z/);
    is_deeply([$l2->get(0, "end")], [qw(a b c x y z)]);
    $l2->destroy;
}

SKIP: {
    skip("nearest not yet implemented", 4)
	if $Listbox eq 'TextList';

    eval { $lb->nearest };
    like($@ ,qr/wrong \# args: should be \"\.listbox.* nearest y\"/,
	 "Listbox nearest error message");

    eval { $lb->nearest(qw/a b/) };
    like($@ ,qr/wrong \# args: should be \"\.listbox.* nearest y\"/);

    eval { $lb->nearest("badindex") };
    like($@ ,qr/\'badindex\' isn\'t numeric/);

    $lb->yview(3);
    is($lb->nearest(1000), 7, "Listbox nearest");
}

eval { $lb->scan };
like($@,qr/wrong \# args: should be \"\.listbox.* scan mark\|dragto x y\"/,
     "Listbox scan error message");

eval { $lb->scan(qw/a b/) };
like($@,qr/wrong \# args: should be \"\.listbox.* scan mark\|dragto x y\"/);

eval { $lb->scan(qw/a b c d/) };
like($@,qr/wrong \# args: should be \"\.listbox.* scan mark\|dragto x y\"/);

eval { $lb->scan(qw/foo bogus 2/) };
like($@ ,qr/\'bogus\' isn\'t numeric/);

eval { $lb->scan(qw/foo 2 3/) };
like($@, $Tk::VERSION < 803
     ? qr/bad scan option \"foo\": must be mark or dragto/
     : qr/bad option \"foo\": must be mark, or dragto/);

{
    my $t = $mw->Toplevel;
    $t->geometry("+0+0");
    my $lb = $t->$Listbox(-width => 10, -height => 5);
    $lb->insert(0, "Short", "Somewhat longer",
		"Really, quite a whole lot longer than can possibly fit on the screen", "Short",
		qw/a b c d e f g h i j/);
    $lb->pack;
    $lb->update;
    $lb->scan("mark", 100, 140);
    $lb->scan("dragto", 90, 137);
    $lb->scan("dragto", "90.1", "137.1"); # ok in Perl/Tk, an error in Tcl/Tk
    $lb->update;
 SKIP: {
	skip($skip_font_test, 2) if $skip_font_test;
	like(join(",",$lb->xview), qr/^0\.24936.*,0\.42748.*$/, "Listbox scan");
	like(join(",",$lb->yview), qr/^0\.071428.*,0\.428571.*$/);
    }
    $t->destroy;
}

eval { $lb->see };
like($@ ,qr/wrong \# args: should be \"\.listbox.* see index\"/,
     "Listbox see error message");

eval { $lb->see("a","b") };
like($@ ,qr/wrong \# args: should be \"\.listbox.* see index\"/);

eval { $lb->see("badindex") };
like($@ ,qr/bad listbox index "badindex": must be active, anchor, end, \@x,y, or a number/);

$lb->yview(7);
$lb->see(7);
is($lb->index('@0,0'), 7, "Listbox see");

$lb->yview(7);
$lb->see(11);
is($lb->index('@0,0'), 7);

$lb->yview(7);
$lb->see(6);
is($lb->index('@0,0'), 6);

$lb->yview(7);
$lb->see(5);
is($lb->index('@0,0'), 3);

$lb->yview(7);
$lb->see(12);
is($lb->index('@0,0'), 8);

$lb->yview(7);
$lb->see(13);
is($lb->index('@0,0'), 11);

$lb->yview(7);
$lb->see(-1);
is($lb->index('@0,0'), 0);

$lb->yview(7);
$lb->see("end");
is($lb->index('@0,0'), 13);

$lb->yview(7);
$lb->see(322);
is($lb->index('@0,0'), 13);

mkPartial();
$partial_lb->see(4);
TODO_fluxbox_problem {
    is($partial_lb->index('@0,0'), 1);
};

eval { $lb->selection };
like($@ ,qr/wrong \# args: should be \"\.listbox.* selection option index \?index\?\"/,
     "Listbox selection error message");

eval { $lb->selection("a") };
like($@ ,qr/wrong \# args: should be \"\.listbox.* selection option index \?index\?\"/);

eval { $lb->selection(qw/a b c d/) };
like($@ ,qr/wrong \# args: should be \"\.listbox.* selection option index \?index\?\"/);

eval { $lb->selection(qw/a bogus/) };
like($@ ,qr/bad listbox index \"bogus\": must be active, anchor, end, \@x,y, or a number/);

eval { $lb->selection(qw/a 0 lousy/) };
like($@ ,qr/bad listbox index \"lousy\": must be active, anchor, end, \@x,y, or a number/);

eval { $lb->selection(qw/anchor 0 0/) };
like($@ ,qr/wrong \# args: should be \"\.listbox.* selection anchor index\"/);

SKIP: {
    skip("anchor index not yet implemented", 5)
	if $Listbox eq 'TextList';

    $lb->selection("anchor", 5);
    is($lb->index("anchor"), 5, "Listbox selection");
    $lb->selectionAnchor(0);
    is($lb->index("anchor"), 0);

    $lb->selectionAnchor(-1);
    is($lb->index("anchor"), 0);
    $lb->selectionAnchor("end");
    is($lb->index("anchor"), 17);
    $lb->selectionAnchor(44);
    is($lb->index("anchor"), 17);
}

$lb->selection("clear", 0, "end");
$lb->selection("set", 2, 8);
$lb->selection("clear", 3, 4);
is_deeply([$lb->curselection], [2,5,6,7,8]);

$lb->selectionClear(0, "end");
$lb->selectionSet(2, 8);
$lb->selectionClear(3, 4);
is_deeply([$lb->curselection], [2,5,6,7,8]);

eval { $lb->selection(qw/includes 0 0/) };
like($@ ,qr/wrong \# args: should be \"\.listbox.* selection includes index\"/,
     "Tk selection includes error message");

$lb->selectionClear(0, "end");
$lb->selectionSet(2,8);
$lb->selectionClear(4);
is($lb->selection("includes", 3), 1, "Listbox selection includes");
is($lb->selection("includes", 4), 0);
is($lb->selection("includes", 5), 1);
is($lb->selectionIncludes(3), 1);

$lb->selectionSet(0, "end");
is($lb->selectionIncludes(-1), 0);

$lb->selectionClear(0, "end");
$lb->selectionSet("end");
is($lb->selection("includes", "end"), 1);

$lb->selectionClear(0, "end");
$lb->selectionSet("end");
is($lb->selection("includes", 44), 0);

{
    my $l2 = $mw->$Listbox;
    is($l2->selectionIncludes(0), 0);
    $l2->destroy;
}

$lb->selection(qw(clear 0 end));
$lb->selection(qw(set 2));
$lb->selection(qw(set 5 7));
is_deeply([$lb->curselection], [qw(2 5 6 7)]);
is(scalar @{[$lb->curselection]}, 4);
$lb->selection(qw(set 5 7));
is_deeply([$lb->curselection], [qw(2 5 6 7)]);
is(scalar @{[$lb->curselection]}, 4);

eval { $lb->selection(qw/badOption 0 0/) };
like($@, qr/bad option \"badOption\": must be anchor, clear, includes, or set/,
     "Listbox selection error message");

eval { $lb->size(qw/a/) };
like($@ ,qr/wrong \# args: should be \"\.listbox.* size\"/,
     "Listbox size error message");

is($lb->size, 18, "Listbox size");

{
    my $l2 = $mw->$Listbox;
    $l2->update;
    is(($l2->xview)[0], 0, "xview, first value");
    is(($l2->xview)[1], 1, "xview, second value");
    $l2->destroy;
}

eval { $lb->destroy };
$lb = $mw->$Listbox(-width => 10, -height => 5, -font => $fixed);
$lb->insert(qw/0 a b c d e f g h i j k l m n o p q r s t/);
$lb->pack;
$lb->update;
is(($lb->xview)[0], 0);
is(($lb->xview)[1], 1);

eval { $lb->destroy };
$lb = $mw->$Listbox(-width => 10, -height => 5, -font => $fixed);
$lb->insert(qw/0 a b c d e f g h i j k l m n o p q r s t/);
$lb->insert(qw/1 0123456789a123456789b123456789c123456789d123456789/);
$lb->pack;
$lb->update;

SKIP: {
    skip("xview not fully implemented", 1)
	if $Listbox eq 'TextList';

    $lb->xview(4);
    with_fixed_font { is_float(join(",",$lb->xview), "0.08,0.28", "Listbox xview with floats") };
}

eval { $lb->xview("foo") };
like($@ ,qr/\'foo\' isn\'t numeric/,
     "Listbox xview error message");

eval { $lb->xview("zoom", "a", "b") };
like($@ ,qr/unknown option \"zoom\": must be moveto or scroll/);

SKIP: {
    skip("xview not fully implemented", 5)
	if $Listbox eq 'TextList';

    $lb->xview(0);
    $lb->xview(moveto => 0.4);
    $lb->update;
    with_fixed_font { is_float(($lb->xview)[0], 0.4) };
    with_fixed_font { is_float(($lb->xview)[1], 0.6) };

    $lb->xview(0);
    $lb->xview(scroll => 2, "units");
    $lb->update;
    with_fixed_font { is_float("@{[ $lb->xview ]}", '0.04 0.24') };

    $lb->xview(30);
    $lb->xview(scroll => -1, "pages");
    $lb->update;
    with_fixed_font { is_float("@{[ $lb->xview ]}", '0.44 0.64') };

    $lb->configure(-width => 1);
    $lb->update;
    $lb->xview(30);
    $lb->xview("scroll", -4, "pages");
    $lb->update;
    with_fixed_font { is_float("@{[ $lb->xview ]}", '0.52 0.54') };
}

eval { $lb->destroy };
$lb = $mw->$Listbox->pack;
$lb->update;
is(($lb->yview)[0], 0);
is(($lb->yview)[1], 1);

eval { $lb->destroy };
$lb = $mw->$Listbox->pack;
$lb->insert(0, "el1");
$lb->update;
is(($lb->yview)[0], 0);
is(($lb->yview)[1], 1);

eval { $lb->destroy };
$lb = $mw->$Listbox(-width => 10, -height => 5, -font => $fixed);
$lb->insert(0,'a','b','c','d','e','f','g','h','i','j','k','l','m','n','o',
	    'p','q','r','s','t');
$lb->pack;
$lb->update;
$lb->yview(4);
$lb->update;
is_float(($lb->yview)[0], 0.2);
is_float(($lb->yview)[1], 0.45);

mkPartial();
is(($partial_lb->yview)[0], 0);
like(($partial_lb->yview)[1] ,qr/^0\.\d+$/,
     "yview returned " . (($partial_lb->yview)[1]));

eval { $lb->yview("foo") };
like($@ ,qr/\Qbad listbox index "foo": must be active, anchor, end, \E\@\Qx,y, or a number/,
     "Listbox yview error message");

eval { $lb->yview("foo", "a", "b") };
like($@ ,qr/unknown option \"foo\": must be moveto or scroll/);

$lb->yview(0);
$lb->yview(moveto => 0.31);
is_float("@{[ $lb->yview ]}", "0.3 0.55");

$lb->yview(2);
$lb->yview(scroll => 2 => "pages");
is_float("@{[ $lb->yview ]}", "0.4 0.65");

$lb->yview(10);
$lb->yview(scroll => -3 => "units");
is_float("@{[ $lb->yview ]}", "0.35 0.6");

$lb->configure(-height => 2);
$lb->update;
$lb->yview(15);
$lb->yview(scroll => -4 => "pages");
is_float("@{[ $lb->yview ]}", "0.55 0.65");

# No tests for DestroyListbox:  I can't come up with anything to test
# in this procedure.

eval { $lb->destroy };
$lb = $mw->$Listbox(-setgrid => 1, -width => 25, -height => 15);
$lb->pack;
$mw->update;
like(getsize($mw), qr/^\d+x\d+$/);
$lb->configure(-setgrid => 0);
$mw->update;
like(getsize($mw), qr/^\d+x\d+$/);

resetGridInfo();

$lb->configure(-highlightthickness => -3);
is($lb->cget(-highlightthickness), 0);

$lb->configure(-exportselection => 0);
$lb->delete(0, "end");
$lb->insert(0, qw(el0 el1 el2 el3 el4 el5 el6 el7 el8));
$lb->selection("set", 3, 5);
$lb->configure(-exportselection => 1);
is($mw->SelectionGet, "el3\nel4\nel5");

my $e = $mw->Entry;
$e->insert(0, "abc");
$e->selection("from", 0);
$e->selection("to", 2);
$lb->configure(-exportselection => 0);
$lb->delete(0, "end");
$lb->insert(0, qw(el0 el1 el2 el3 el4 el5 el6 el7 el8));
$lb->selectionSet(3, 5);
$lb->selectionClear(3, 5);
$lb->configure(-exportselection => 1);
is($mw->SelectionOwner, $e);
is($mw->SelectionGet, "ab");
$e->destroy;

$mw->SelectionClear;
$lb->configure(-exportselection => 1);
$lb->delete(0, "end");
$lb->insert(qw(0 el0 el1 el2 el3 el4 el5 el6 el7 el8));
$lb->selection("set", 1, 1);
is($mw->SelectionGet, "el1");
is(join(',',$lb->curselection), "1"); # join forces list context
$lb->configure(-exportselection => 0);
eval { $mw->SelectionGet };
like($@ ,qr/PRIMARY selection doesn\'t exist or form \"(UTF8_)?STRING\" not defined/,
     "SelectionGet, error message");
is(join(',',$lb->curselection), "1"); # join forces list context
$lb->selection("clear", 0, "end");
eval { $mw->SelectionGet };
like($@ ,qr/PRIMARY selection doesn\'t exist or form \"(UTF8_)?STRING\" not defined/);
is($lb->curselection, undef, "Empty curselection");
$lb->selection("set", 1, 3);
eval { $mw->SelectionGet };
like($@ ,qr/PRIMARY selection doesn\'t exist or form \"(UTF8_)?STRING\" not defined/);
is_deeply([$lb->curselection], [qw(1 2 3)]);
$lb->configure(-exportselection => 1);
is($mw->SelectionGet, "el1\nel2\nel3");
is_deeply([$lb->curselection], [qw(1 2 3)]);

$lb->destroy;
$mw->geometry("300x300");
$mw->update;
$mw->geometry("");
$mw->withdraw;
$lb = $mw->$Listbox(-font => $fixed, -width => 15, -height => 20);
$lb->pack;
$lb->update;
$mw->deiconify;
like(getsize($mw), qr/^\d+x\d+$/);
$lb->configure(-setgrid => 1);
$mw->update;
like(getsize($mw), qr/^\d+x\d+$/);

$lb->destroy;
$mw->withdraw;
$lb = $mw->$Listbox(-font => $fixed, -width => 30, -height => 20,
		   -setgrid => 1);
$mw->geometry("+0+0");
$lb->pack;
$mw->update;
$mw->deiconify;
{
    local $TODO = "Tests may fail (window-manager related?)";

    is(getsize($mw), "30x20");
    $mw->geometry("26x15");
    $mw->update;
    is(getsize($mw), "26x15");
    $lb->configure(-setgrid => 1);
    $lb->update;
    is(getsize($mw), "26x15");
}

$mw->geometry("");
$lb->destroy;
resetGridInfo();

my @log;

$lb = $mw->$Listbox(-width => 15, -height => 20,
		   -xscrollcommand => sub { record("x", @_) },
		   -yscrollcommand => [qw/record y/],
		  )->pack;
$lb->update;
$lb->configure(-fg => "black");
@log = ();
$lb->update;
is($log[0], "y 0 1");
is($log[1], "x 0 1");

$lb->destroy;

SKIP: {
    skip("no -listvar in older Tks", 12)
	if !$listvar_impl;
    {
	my @x = qw/a b c d/;
	my $lb = $mw->$Listbox(-listvar => \@x);
	is_deeply([$lb->get(0, "end")], [qw(a b c d)], "-listvar after initial setting");
	$lb->destroy;
    }

    {
	my @x = qw(a b c d);
	my $lb = $mw->$Listbox;
	$lb->insert(qw(end 1 2 3 4));
	$lb->configure(-listvar => \@x);
	is_deeply([$lb->get(qw(0 end))], [qw(a b c d)],
		  q{ConfigureListbox, no listvar -> existing listvar});
	$lb->destroy;
    }

    {
 	local $TODO = "Not yet implemented in Perl/Tk";

 	my @x = qw(a b c d);
 	my $lb = $mw->$Listbox(-listvar => \@x);
 	$lb->configure(-listvar => undef);
 	$lb->insert(qw(end 1 2 3 4));
 	is_deeply([@x], [qw(a b c d)],
 		  q{ConfigureListbox procedure, listvar -> no listvar});
 	is_deeply([$lb->get(qw(0 end))], [qw(a b c d 1 2 3 4)]);
 	$lb->destroy;
    }
    
    if (0) { # TODO: dumps core!!!
	my @x = qw(a b c d);
	my @y = (1..4);
	my $lb = $mw->$Listbox(-listvar => \@x);
	$lb->configure(-listvar => \@x);
	$lb->configure(-listvar => \@y);
	$lb->insert(qw(end 5 6 7 8));
	is_deeply(\@x, [qw(a b c d)],
		  q{ConfigureListbox procedure, listvar -> different listvar});
	is_deeply(\@y, [qw(1 2 3 4 5 6 7 8)]);
	$lb->destroy;
    }

    {
 	local $TODO = "Not yet implemented in Perl/Tk";

	my @x;
	my $lb = $mw->$Listbox;
	$lb->insert(qw(end a b c d));
	$lb->configure(-listvar => \@x);
	is_deeply(\@x, [qw(a b c d)],
		  q{ConfigureListbox, no listvar -> non-existant listvar});
	$lb->destroy;
    }

    {
	my @x;
	my $lb = $mw->$Listbox(-listvar => \@x);
	is_deeply(\@x, [], q{ConfigureListbox, non-existant listvar});
    }

    {
 	local $TODO = "Not yet implemented in Perl/Tk";

	my @x = qw(a b c d);
	my @y;
	my $lb = $mw->$Listbox(-listvar => \@x);
	$lb->configure(-listvar => \@y);
	is_deeply(\@y, [qw(a b c d)],
		  q{ConfigureListbox, listvar -> non-existant listvar});
	$lb->destroy;
    }


    {
	my @x = qw(a b c d);
	my $lb = $mw->$Listbox(-listvar => \@x);
	$lb->configure(-listvar => \@x);
	is_deeply(\@x, [qw(a b c d)],
		  q{ConfigureListbox, listvar -> same listvar});
	$lb->destroy;
    }

    {
	my $lb = $mw->$Listbox;
	$lb->insert(qw(end a b c d));
	$lb->configure(-listvar => undef);
	is_deeply([$lb->get(qw(0 end))], [qw(a b c d)],
		  q{ConfigureListbox, no listvar -> no listvar});
    }

    {
	local $TODO = "Not yet implemented in Perl/Tk";

	my $lb = $mw->$Listbox;
	$lb->insert(qw(end a b c d));
	my $x = "this is not an array";
	eval { $lb->configure(-listvar => \$x) };
	like($@, qr{Should have an error message about invalid type});
	is_deeply([$lb->get(qw(0 end))], [qw(a b c d)],
		  q{ConfigureListbox, no listvar -> bad listvar});
	is($lb->cget(-listvar), undef);
    }
}

# No tests for DisplayListbox:  I don't know how to test this procedure.

Tk::catch { $lb->destroy if Tk::Exists($lb) };
$lb = $mw->$Listbox(-font => $fixed, -width => 15, -height => 20)->pack;
{
    with_fixed_font { is($lb->reqwidth, 115, "Reqwidth with fixed font") };
    with_fixed_font { is($lb->reqheight, 328, "Reqheight with fixed font") };
}

eval { $lb->destroy };
$lb = $mw->$Listbox(-font => $fixed, -width => 0, -height => 10)->pack;
$lb->update;
{
    with_fixed_font { is($lb->reqwidth, 17) };
    with_fixed_font { is($lb->reqheight, 168) };
}

eval { $lb->destroy };
$lb = $mw->$Listbox(-font => $fixed, -width => 0, -height => 10,
		   -bd => 3)->pack;
$lb->insert(0, "Short", "Really much longer", "Longer");
$lb->update;
{
    with_fixed_font { is($lb->reqwidth, 138) };
    with_fixed_font { is($lb->reqheight, 170) };
}

eval { $lb->destroy };
$lb = $mw->$Listbox(-font => $fixed, -width => 10, -height => 0,
		  )->pack;
$lb->update;
{
    with_fixed_font { is($lb->reqwidth, 80) };
    with_fixed_font { is($lb->reqheight, 24) };
}

eval { $lb->destroy };
$lb = $mw->$Listbox(-font => $fixed, -width => 10, -height => 0,
		   -highlightthickness => 0)->pack;
$lb->insert(0, "Short", "Really much longer", "Longer");
$lb->update;
{
    with_fixed_font { is($lb->reqwidth, 76) };
    with_fixed_font { is($lb->reqheight, 52) };
}

eval { $lb->destroy };
# If "0" in selected font had 0 width, caused divide-by-zero error.
$lb = $mw->$Listbox(-font => '{open look glyph}')->pack;
$lb->update;

eval { $lb->destroy };
$lb = $mw->$Listbox(-height => 2,
		   -xscrollcommand => sub { record("x", @_) },
		   -yscrollcommand => sub { record("y", @_) })->pack;
$lb->update;

$lb->delete(0, "end");
$lb->insert(qw/end a b c d/);
$lb->insert(qw/5 x y z/);
$lb->insert(qw/2 A/);
$lb->insert(qw/0 q r s/);
is_deeply([$lb->get(qw/0 end/)], [qw(q r s a b A c d x y z)]);

$lb->delete(qw/0 end/);
$lb->insert(qw/0 a b c d e f g h i j/);
$lb->selection(qw/anchor 2/);
$lb->insert(qw/2 A B/);
is($lb->index(qw/anchor/), 4);

$lb->delete(qw/0 end/);
$lb->insert(qw/0 a b c d e f g h i j/);
$lb->selection(qw/anchor 2/);
$lb->insert(qw/3 A B/);
is($lb->index(qw/anchor/), 2);

$lb->delete(qw/0 end/);
$lb->insert(qw/0 a b c d e f g h i j/);
$lb->yview(qw/3/);
$lb->update;
$lb->insert(qw/2 A B/);
is($lb->index(q/@0,0/), 5);

$lb->delete(qw/0 end/);
$lb->insert(qw/0 a b c d e f g h i j/);
$lb->yview(qw/3/);
$lb->update;
$lb->insert(qw/3 A B/);
is($lb->index(q/@0,0/), 3);

$lb->delete(qw/0 end/);
$lb->insert(qw/0 a b c d e f g h i j/);
$lb->activate(qw/5/);
$lb->insert(qw/5 A B/);
is($lb->index(qw/active/), 7);

$lb->delete(qw/0 end/);
$lb->insert(qw/0 a b c d e f g h i j/);
$lb->activate(qw/5/);
$lb->insert(qw/6 A B/);
is($lb->index(qw/active/), 5);

$lb->delete(qw/0 end/);
$lb->insert(qw/0 a b c/);
is($lb->index(qw/active/), 2);

$lb->delete(qw/0 end/);
$lb->insert(qw/0/);
is($lb->index(qw/active/), 0);

$lb->delete(qw/0 end/);
$lb->insert(qw/0 a b/, "two words", qw/c d e f g h i j/);
$lb->update;
@log = ();
$lb->insert(qw/0 word/);
$lb->update;
like("@log",qr/^y 0 0\.\d+/);

$lb->delete(qw/0 end/);
$lb->insert(qw/0 a b/, "two words", qw/c d e f g h i j/);
$lb->update;
@log = ();
$lb->insert(0, "much longer entry");
$lb->update;
like("$log[0]",qr/^y 0 0\.\d+/);
like("$log[1]", qr/x 0 \d[\d\.]*/);

SKIP: {
    skip($skip_font_test, 4) if $skip_font_test;
    my $l2 = $mw->$Listbox(-width => 0, -height => 0)->pack(-side => "top");
    $l2->insert(0, "a", "b", "two words", "c", "d");
    is($l2->reqwidth, 80);
    is($l2->reqheight, 93);
    $l2->insert(0, "much longer entry");
    is($l2->reqwidth, 122);
    is($l2->reqheight, 110);
    $l2->destroy;
}

SKIP: {
    skip("-listvar not implemented in older Tks", 1)
	if !$listvar_impl;
    skip("dumps core *TODO*", 1);

    my @x = qw(a b c d);
    my $l2 = $mw->$Listbox(-listvar => \@x);
    $l2->insert(0, 1 .. 4);
    is_deeply([@x], [qw(1 2 3 4 a b c d)]);
    $l2->destroy;
}

{
    my $l2 = $mw->$Listbox;
    $l2->insert(0, 0 .. 4);
    $l2->selection("set", 2, 4);
    $l2->insert(0, "a");
    is_deeply([ $l2->curselection ], [qw(3 4 5)],
	      "curselection after selection set");
    is(scalar @{[ $l2->curselection ]}, 3);
    $l2->destroy;
}

$lb->delete(0, "end");
$lb->insert(0, qw/a b c d e f g h i j/);
$lb->selectionSet(1, 6);
$lb->delete(4, 3);
is($lb->size, 10, "size after negative delete");
is($mw->SelectionGet, "b
c
d
e
f
g", "SelectionGet after selectionSet");

$lb->delete(qw/0 end/);
$lb->insert(qw/0 a b c d e f g h i j/);
$lb->selection(qw/set 3 6/);
$lb->delete(qw/4 4/);
is($lb->size, 9);
is($lb->get(4), "f");
is_deeply([ $lb->curselection ], [3,4,5]);
is(scalar @{[ $lb->curselection ]}, 3);
is(($lb->curselection)[0], 3);
is(($lb->curselection)[-1], 5);

$lb->delete(qw/0 end/);
$lb->insert(qw/0 a b c d e f g h i j/);
$lb->delete(qw/0 3/);
is($lb->size, 6);
is($lb->get(0), "e");
is($lb->get(1), "f");

$lb->delete(qw/0 end/);
$lb->insert(qw/0 a b c d e f g h i j/);
$lb->delete(qw/8 1000/);
is($lb->size, 8);
is($lb->get(7), "h");

$lb-> delete(0, qw/end/);
$lb->insert(qw/0 a b c d e f g h i j/);
$lb->selection(qw/anchor 2/);
$lb->delete(qw/0 1/);
is($lb->index(qw/anchor/), 0);

$lb->delete(qw/0 end/);
$lb->insert(qw/0 a b c d e f g h i j/);
$lb->selection(qw/anchor 2/);
$lb->delete(qw/2/);
is($lb->index(qw/anchor/), 2);

$lb->delete(qw/0 end/);
$lb->insert(qw/0 a b c d e f g h i j/);
$lb->selection(qw/anchor 4/);
$lb->delete(qw/2 5/);
is($lb->index(qw/anchor/), 2);

$lb->delete(qw/0 end/);
$lb->insert(qw/0 a b c d e f g h i j/);
$lb->selection(qw/anchor 3/);
$lb->delete(qw/4 5/);
is($lb->index(qw/anchor/), 3);

$lb->delete(qw/0 end/);
$lb->insert(qw/0 a b c d e f g h i j/);
$lb->yview(qw/3/);
$lb->update;
$lb->delete(qw/1 2/);
is($lb->index(q/@0,0/), 1);

$lb->delete(qw/0 end/);
$lb->insert(qw/0 a b c d e f g h i j/);
$lb->yview(qw/3/);
$lb->update;
$lb->delete(qw/3 4/);
is($lb->index(q/@0,0/), 3);

$lb->delete(qw/0 end/);
$lb->insert(qw/0 a b c d e f g h i j/);
$lb->yview(qw/3/);
$lb->update;
$lb->delete(qw/4 6/);
is($lb->index(q/@0,0/), 3);

$lb->delete(qw/0 end/);
$lb->insert(qw/0 a b c d e f g h i j/);
$lb->yview(qw/3/);
$lb->update;
$lb->delete(qw/3 end/);
like($lb->index(q/@0,0/), qr/^[12]$/);

mkPartial();
$partial_lb->yview(8);
$mw->update;
$partial_lb->delete(10, 13);
like($partial_lb->index('@0,0'), qr/^[67]$/);

$lb->delete(qw/0 end/);
$lb->insert(qw/0 a b c d e f g h i j/);
$lb->activate(qw/6/);
$lb->delete(qw/3 4/);
is($lb->index(qw/active/), 4);

$lb->delete(qw/0 end/);
$lb->insert(qw/0 a b c d e f g h i j/);
$lb->activate(qw/6/);
$lb->delete(qw/5 7/);
is($lb->index(qw/active/), 5);

$lb->delete(qw/0 end/);
$lb->insert(qw/0 a b c d e f g h i j/);
$lb->activate(qw/6/);
$lb->delete(qw/5 end/);
is($lb->index(qw/active/), 4);

$lb->delete(qw/0 end/);
$lb->insert(qw/0 a b c d e f g h i j/);
$lb->activate(qw/6/);
$lb->delete(qw/0 end/);
is($lb->index(qw/active/), 0);

$lb->delete(qw/0 end/);
$lb->insert(qw/0 a b c/, "two words", qw/d e f g h i j/);
$lb->update;
@log = ();
$lb->delete(qw/4 6/);
$lb->update;
like($log[0], qr/y 0 0\.\d+/);

$lb->delete(qw/0 end/);
$lb->insert(qw/0 a b c/, "two words", qw/d e f g h i j/);
$lb->update;
@log = ();
$lb->delete(qw/3/);
$lb->update;
like($log[0], qr/^y 0 0\.\d+$/);
is($log[1], "x 0 1");

SKIP: {
    skip($skip_font_test, 4) if $skip_font_test;
    my $l2 = $mw->$Listbox(-width => 0, -height => 0)->pack(-side => "top");
    $l2->insert(0, "a", "b", "two words", qw/c d e f g/);
    is($l2->reqwidth, 80);
    is($l2->reqheight, 144);
    $l2->delete(2, 4);
    is($l2->reqwidth, 17);
    is($l2->reqheight, 93);
    $l2->destroy;
}

SKIP: {
    skip("-listvar not implemented in older Tks", 1)
	if !$listvar_impl;
    skip("dumps core *TODO*", 1);

    my @x = qw(a b c d);
    my $l2 = $mw->Listbox(-listvar => \@x);
    $l2->delete(0, 1);
    is_deeply(\@x, ["c","d"],
	      q{DeleteEls procedure, check -listvar update});
}

$lb->destroy;
$lb = $mw->$Listbox(-setgrid => 1)->pack;
$lb->update;
like(getsize($mw), qr/^\d+x\d+$/); # still worth it ?
$lb->destroy;
like(getsize($mw), qr/^\d+x\d+$/); # still worth it ?
ok(!Tk::Exists($lb), "Listbox is destroyed");

resetGridInfo();

$lb = $mw->$Listbox(-height => 5, -width => 10);
$lb->insert(qw/0 a b c/, "A string that is very very long",
	    qw/ d e f g h i j k/);
$lb->pack;
$lb->update;
$lb->place(qw/-width 50 -height 80/);
$lb->update;
SKIP: {
    skip($skip_font_test, 2) if $skip_font_test;
    like(join(" ", $lb->xview), qr/^0 0\.2222/);
    like(join(" ", $lb->yview), qr/^0 0\.3333/);
}

map { $_->destroy } $mw->children;
my $l1 = $mw->$Listbox(-bg => "#543210");
my $l2 = $l1;
like(join(",", map { $_->PathName } $mw->children) ,qr/^\.listbox\d*$/);
is($l2->cget(-bg), "#543210");
$l2->destroy;

my $top = $mw->Toplevel;
$top->geometry("+0+0");
my $top_lb = $top->$Listbox(-setgrid => 1,
			    -width => 20,
			    -height => 10)->pack;
$top_lb->update;
like($top->geometry, qr/20x10\+\d+\+\d+/);
$top_lb->destroy;
SKIP: {
    skip($skip_font_test, 1) if $skip_font_test;
    like($top->geometry, qr/150x178\+\d+\+\d+/, "Geometry");
}

$lb = $mw->$Listbox->pack;
$lb->delete(0, "end");
$lb->insert(qw/0 el0 el1 el2 el3 el4 el5 el6 el7 el8 el9 el10 el11/);
$lb->activate(3);
is($lb->index("active"), 3);
$lb->activate(6);
is($lb->index("active"), 6);

$lb->selection(qw/anchor 2/);
is($lb->index(qw/anchor/), 2);

$lb->insert(qw/end A B C D E/);
$lb->selection(qw/anchor end/);
$lb->delete(qw/12 end/);
is($lb->index("anchor"), 12);
is($lb->index("end"), 12);

eval { $lb->index("a") };
like($@ ,qr/bad listbox index \"a\": must be active, anchor, end, \@x,y, or a number/, "Listbox index error message");

eval { $lb->index("\@") };
like($@ ,qr/bad listbox index \"\@\": must be active, anchor, end, \@x,y, or a number/);

eval { $lb->index("\@foo") };
like($@ ,qr/bad listbox index \"\@foo\": must be active, anchor, end, \@x,y, or a number/);

eval { $lb->index("\@1x3") };
like($@ ,qr/bad listbox index \"\@1x3\": must be active, anchor, end, \@x,y, or a number/);

eval { $lb->index("\@1,") };
like($@ ,qr/bad listbox index \"\@1,\": must be active, anchor, end, \@x,y, or a number/);

eval { $lb->index("\@1,foo") };
like($@ ,qr/bad listbox index \"\@1,foo\": must be active, anchor, end, \@x,y, or a number/);

eval { $lb->index("\@1,2x") };
like($@ ,qr/bad listbox index \"\@1,2x\": must be active, anchor, end, \@x,y, or a number/);

eval { $lb->index("1xy") };
like($@ ,qr/bad listbox index \"1xy\": must be active, anchor, end, \@x,y, or a number/);

is($lb->index("end"), 12);

is($lb->get(qw/end/), "el11");

$lb->delete(qw/0 end/);
is($lb->index(qw/end/), 0);

$lb->delete(qw/0 end/);
$lb->insert(qw/0 el0 el1 el2 el3 el4 el5 el6 el7 el8 el9 el10 el11/);
$mw->geometry(""); # XXX hack to force a fully visible listbox, see http://rt.cpan.org/Ticket/Display.html?id=31290
$lb->update;

SKIP: {
    skip($skip_font_test, 2) if $skip_font_test;

    is($lb->index(q/@5,57/), 3);
    is($lb->index(q/@5,58/), 3);
}

is($lb->index(qw/3/), 3);
is($lb->index(qw/20/), 20);

is($lb->get(qw/20/), undef);

is($lb->index(qw/-2/), -2);

$lb->delete(qw/0 end/);
is($lb->index(qw/1/), 1);

$lb->destroy;
$lb = $mw->$Listbox(-height => 5)->pack;
$lb->insert(qw/0 a b c d e f g h i j/);
$lb->yview(qw/3/);
$lb->update;
is($lb->index(q/@0,0/), 3);
$lb->yview(qw/-1/);
$lb->update;
is($lb->index(q/@0,0/), 0);

$lb->destroy;
$lb = $mw->$Listbox(qw/-height 5/)->pack;
$lb->insert(qw/0 a b c d e f g h i j/);
$lb->yview(qw/3/);
$lb->update;
is($lb->index(q/@0,0/), 3);
$lb->yview(qw/20/);
$lb->update;
is($lb->index(q/@0,0/), 5);

$lb->destroy;
$lb = $mw->$Listbox(qw/-height 5 -yscrollcommand/, [qw/record y/])->pack;
$lb->insert(qw/0 a b c d e f g h i j/);
$lb->update;
@log = ();
$lb->yview(qw/2/);
$lb->update;
is_float("@{[ $lb->yview ]}", "0.2 0.7");
is_float($log[0], "y 0.2 0.7");

$lb->destroy;
$lb = $mw->$Listbox(qw/-height 5 -yscrollcommand/, [qw/record y/])->pack;
$lb->insert(qw/0 a b c d e f g h i j/);
$lb->update;
@log = ();
$lb->yview(qw/8/);
$lb->update;
is_float("@{[ $lb->yview ]}", "0.5 1");
is_float($log[0], "y 0.5 1");

$lb->destroy;
$lb = $mw->$Listbox(qw/-height 5 -yscrollcommand/, [qw/record y/])->pack;
$lb->insert(qw/0 a b c d e f g h i j/);
$lb->yview(qw/3/);
$lb->update;
@log = ();
$lb->yview(qw/3/);
$lb->update;
is_float("@{[ $lb->yview ]}", "0.3 0.8");
is(scalar @log, 0);

mkPartial();
$partial_lb->yview(13);
like($partial_lb->index('@0,0'), qr/^1[01]$/);

$lb->destroy;
$lb = $mw->$Listbox(-font => $fixed,
		   -xscrollcommand => ["record", "x"],
		   -width => 10);
$lb->insert(qw/0 0123456789a123456789b123456789c123456789d123456789e123456789f123456789g123456789h123456789i123456789/);
$lb->pack;
$lb->update;

@log = ();
$lb->xview(qw/99/);
$lb->update;
with_fixed_font { is_float("@{[ $lb->xview ]}", "0.9 1") };
with_fixed_font { is_float(($lb->xview)[0], 0.9) };
is(($lb->xview)[1], 1);
with_fixed_font { is_float($log[0], "x 0.9 1") };

@log = ();
$lb->xview(qw/moveto -.25/);
$lb->update;
with_fixed_font { is_float("@{[ $lb->xview ]}", "0 0.1") };
with_fixed_font { is_float($log[0], "x 0 0.1") };

$lb->xview(qw/10/);
$lb->update;
@log = ();
$lb->xview(qw/10/);
$lb->update;
with_fixed_font { is_float("@{[ $lb->xview ]}", "0.1 0.2") };
is(scalar @log, 0);

$lb->destroy;
$lb = $mw->$Listbox(-font => $fixed, -width => 10, -height => 5)->pack;
$lb->insert(qw/0 a bb c d e f g h i j k l m n o p q r s/);
$lb->insert(qw/0 0123456789a123456789b123456789c123456789d123456789/);
$lb->update;
my $width  = ($lb->bbox(2))[2] - ($lb->bbox(1))[2];
my $height = ($lb->bbox(2))[1] - ($lb->bbox(1))[1];

$lb->yview(qw/0/);
$lb->xview(qw/0/);
$lb->scan(qw/mark 10 20/);
$lb->scan(qw/dragto/, 10-$width, 20-$height);
$lb->update;
with_fixed_font { is_float("@{[ $lb->xview ]}", "0.2 0.4") };
is_float("@{[ $lb->yview ]}", "0.5 0.75");

$lb->yview(qw/5/);
$lb->xview(qw/10/);
$lb->scan(qw/mark 10 20/);
$lb->scan(qw/dragto 20 40/);
$lb->update;
with_fixed_font { is_float("@{[ $lb->xview ]}", "0 0.2") };
is_float("@{[ $lb->yview ]}", "0 0.25");

$lb->scan(qw/dragto/, 20-$width, 40-$height);
$lb->update;
with_fixed_font { is_float("@{[ $lb->xview ]}", "0.2 0.4") };
with_fixed_font { is_float(join(',',$lb->xview), "0.2,0.4") };  # just to prove it is a list
is_float("@{[ $lb->yview ]}", "0.5 0.75");
is_float(join(',',$lb->yview), "0.5,0.75"); # just to prove it is a list

$lb->yview(qw/moveto 1.0/);
$lb->xview(qw/moveto 1.0/);
$lb->scan(qw/mark 10 20/);
$lb->scan(qw/dragto 5 10/);
$lb->update;
with_fixed_font { is_float("@{[ $lb->xview ]}", "0.8 1") };
is_float("@{[ $lb->yview ]}", "0.75 1");
$lb->scan(qw/dragto/, 5+$width, 10+$height);
$lb->update;
with_fixed_font { is_float("@{[ $lb->xview ]}", "0.64 0.84") };
is_float("@{[ $lb->yview ]}", "0.25 0.5");

mkPartial();
is($partial_lb->nearest($partial_lb->height), 4);

$lb->destroy;
$lb = $mw->$Listbox(-font => $fixed,
		    -width => 20,
		    -height => 10);
$lb->insert(qw/0 a b c d e f g h i j k l m n o p q r s t/);
$lb->yview(qw/4/);
$lb->pack;
$lb->update;

{
    with_fixed_font { is($lb->index(q/@50,0/), 4) };
    with_fixed_font { is($lb->index(q/@50,35/), 5) };
    with_fixed_font { is($lb->index(q/@50,36/), 6) };
}

like($lb->index(q/@50,200/), qr/^\d+/);

$lb->delete(qw/0 end/);
$lb->insert(qw/0 a b c d e f g h i j k l m n o p/);
$lb->selection(qw/set 2 4/);
$lb->selection(qw/set 7 12/);
$lb->selection(qw/clear 4 7/);
is_deeply([ $lb->curselection ], [qw(2 3 8 9 10 11 12)]);

$lb->delete(qw/0 end/);
$lb->insert(qw/0 a b c d e f g h i j k l m n o p/);

$e = $mw->Entry;
$e->insert(0, "This is some text");
$e->selection(qw/from 0/);
$e->selection(qw/to 7/);
$lb->selection(qw/clear 2 4/);
is($mw->SelectionOwner, $e);
$lb->selection(qw/set 3/);
is($mw->SelectionOwner, $lb);
is($mw->SelectionGet, "d");

$lb->delete(qw/0 end/);
$lb->selection(qw/clear 0 end/);
$lb->selection(qw/set 0 end/);
is($lb->curselection, undef);

$lb->delete(qw/0 end/);
$lb->insert(qw/0 a b c d e f/);
$lb->selection(qw/clear 0 end/);
$lb->selection(qw/set -2 -1/);
is($lb->curselection, undef);

$lb->delete(qw/0 end/);
$lb->insert(qw/0 a b c d e f/);
$lb->selection(qw/clear 0 end/);
$lb->selection(qw/set -1 3/);
is_deeply([$lb->curselection], [0,1,2,3]);

$lb->delete(qw/0 end/);
$lb->insert(qw/0 a b c d e f/);
$lb->selection(qw/clear 0 end/);
$lb->selection(qw/set 2 4/);
is_deeply([$lb->curselection], [qw(2 3 4)]);

$lb->delete(qw/0 end/);
$lb->insert(qw/0 a b c d e f/);
$lb->selection(qw/clear 0 end/);
$lb->selection(qw/set 4 end/);
is_deeply([$lb->curselection], [4, 5]);

$lb->delete(qw/0 end/);
$lb->insert(qw/0 a b c d e f/);
$lb->selection(qw/clear 0 end/);
$lb->selection(qw/set 4 30/);
is_deeply([$lb->curselection], [4, 5]);

$lb->delete(qw/0 end/);
$lb->insert(qw/0 a b c d e f/);
$lb->selection(qw/clear 0 end/);
$lb->selection(qw/set end 30/);
is(join(",", $lb->curselection), 5);
is(scalar @{[ $lb->curselection ]}, 1);

$lb->delete(qw/0 end/);
$lb->insert(qw/0 a b c d e f/);
$lb->selection(qw/clear 0 end/);
$lb->selection(qw/set 20 25/);
is($lb->curselection, undef);

$lb->delete(qw/0 end/);
$lb->insert(qw/0 a b c/, "two words", qw/ e f g h i \ k l m n o p/);
$lb->selection(qw/set 2 4/);
$lb->selection(qw/set 9/);
$lb->selection(qw/set 11 12/);
is($mw->SelectionGet, "c\ntwo words\ne\n\\\nl\nm");

$lb->delete(qw/0 end/);
$lb->insert(qw/0 a b c/, "two words", qw/ e f g h i \ k l m n o p/);
$lb->selection(qw/set 3/);
is($mw->SelectionGet, "two words");

my $long = "This is quite a long string\n" x 11;
$lb->delete(qw/0 end/);
$lb->insert(0, "1$long", "2$long", "3$long", "4$long", "5$long");
$lb->selection(qw/set 0 end/);
is($mw->SelectionGet, "1$long\n2$long\n3$long\n4$long\n5$long");

$lb->delete(qw/0 end/);
$lb->insert(qw/0 a b c d e/);
$lb->selection(qw/set 0 end/);
$e->destroy;
$e = $mw->Entry;
$e->insert(0, "This is some text");
$e->selection(qw/from 0/);
$e->selection(qw/to 5/);
is($lb->curselection, undef);

$lb->delete(qw/0 end/);
$lb->insert(qw/0 a b c d e/);
$lb->selection(qw/set 0 end/);
$lb->configure(qw/-exportselection 0/);
$e->destroy;
$e = $top->Entry;
$e->insert(0, "This is some text");
$e->selection(qw/from 0/);
$e->selection(qw/to 5/);
is_deeply([$lb->curselection], [qw(0 1 2 3 4)]);

$lb->destroy;
$lb = $mw->$Listbox(-font => $fixed, -width => 10, -height => 5);
$lb->pack;
$lb->update;

$lb->configure(qw/-yscrollcommand/, [qw/record y/]);
@log = ();
$lb->insert(qw/0 a b c/);
$lb->update;
$lb->insert(qw/end d e f g h/);
$lb->update;
$lb->delete(qw/0 end/);
$lb->update;
is($log[0], "y 0 1");
TODO_fluxbox_problem {
    is_float($log[1], "y 0 0.625");
    is($log[2], "y 0 1");
};

mkPartial();
$partial_lb->configure(-yscrollcommand => ["record", "y"]);
@log = ();
$partial_lb->yview(3);
$partial_lb->update;
like($log[0], qr/^y 0\.2(0000+\d+)? 0\.\d+/);

my @x = ();

sub Tk::Error {
    push @x, @_;
}

# XXX dumps core with 5.7.0 and 803.023
$lb->configure(qw/-yscrollcommand gorp/);
$lb->insert(qw/0 foo/);
$lb->update;
like("@x" ,qr/Undefined subroutine &main::gorp called.*vertical scrolling command executed by listbox/s);

$lb->destroy;
$lb = $mw->$Listbox(-font => $fixed, qw/-width 10 -height 5/)->pack;
$lb->update;

$lb->configure(qw/-xscrollcommand/, ["record", "x"]);
@log = ();
$lb->insert(qw/0 abc/);
$lb->update;
$lb->insert(qw/0/, "This is a much longer string...");
$lb->update;
$lb->delete(qw/0 end/);
$lb->update;
is($log[0], "x 0 1");
with_fixed_font { like($log[1] ,qr/^x 0 0\.32258/) };
is($log[2], "x 0 1");

@x = ();
$lb->configure(qw/-xscrollcommand bogus/);
$lb->insert(qw/0 foo/);
$lb->update;
like("@x" ,qr/Undefined subroutine &main::bogus.*horizontal scrolling command executed by listbox/s);

foreach ($mw->children) { $_->destroy }

SKIP: {
    skip("no -listvar in older Tks", 12)
	if !$listvar_impl;

    {
	my @x;
	my $lb = $mw->$Listbox(-listvar => \@x);
	@x = qw(a b c d);
	is_deeply([$lb->get(0, "end")], [qw(a b c d)],
		  "ListboxListVarProc");
	$lb->destroy;
    }

    {
	my @x;
	@x = qw(a b c d);
	my $lb = $mw->$Listbox(-listvar => \@x);
	$lb->configure(-listvar => undef);
	@x = ();
	is_deeply(\@x, []);
	$lb->destroy;
    }

    {
	my @x = qw(a b c d);
	my $lb = $mw->$Listbox(-listvar => \@x);
	push @x, qw(e f g);
	is(scalar(@x), 7);
	$lb->destroy;
    }

    {
	my @x = qw(a b c d e f g);
	my $lb = $mw->$Listbox(-listvar => \@x);
	$lb->selection(qw(set end));
	@x = qw(a b c d);
	@x = (0..6);
	is($lb->curselection, undef,
	   q{ListboxListVarProc, test selection after listvar mod});
	$lb->destroy;
    }

    {
	my @x = qw(a b c d);
	my $lb = $mw->$Listbox(-listvar => \@x);
	$lb->selection(qw(set 3));
	push @x, qw(e f g);
	is_deeply([$lb->curselection], [3]);
	$lb->destroy;
    }

    {
	my @x = qw(a b c d);
	my $lb = $mw->$Listbox(-listvar => \@x);
	$lb->selection(qw(set 0));
	splice @x, 0, 0, (1, 2, 3, 4); # not sure if this is the correct linsert translation
	is_deeply([$lb->curselection], [0]);
	$lb->destroy;
    }

    {
	local $TODO = "Not yet implemented in Perl/Tk";

	my @x = qw(a b c d);
	my $lb = $mw->$Listbox(-listvar => \@x);
	$lb->selection(qw(set 2));
	@x = qw(a b c);
	is_deeply([$lb->curselection], [2]);
	$lb->destroy;
    }

#  test listbox-21.9 {ListboxListVarProc, test hscrollbar after listvar mod} {
#      catch {destroy $_lb}
#      catch {unset x}
#      set log {}
#      listbox $_lb -font $fixed -width 10 -xscrollcommand "record x" -listvar x
#      pack $_lb
#      update
#      lappend x "0000000000"
#      update
#      lappend x "00000000000000000000"
#      update
#      set log
#  } [list {x 0 1} {x 0 1} {x 0 0.5}]
#  test listbox-21.10 {ListboxListVarProc, test hscrollbar after listvar mod} {
#      catch {destroy $_lb}
#      catch {unset x}
#      set log {}
#      listbox $_lb -font $fixed -width 10 -xscrollcommand "record x" -listvar x
#      pack $_lb
#      update
#      lappend x "0000000000"
#      update
#      lappend x "00000000000000000000"
#      update
#      set x [list "0000000000"]
#      update
#      set log
#  } [list {x 0 1} {x 0 1} {x 0 0.5} {x 0 1}]
#  test listbox-21.11 {ListboxListVarProc, bad list} {
#      catch {destroy $_lb}
#      catch {unset x}
#      listbox $_lb -listvar x
#      set x [list a b c d]
#      catch {set x {this is a " bad list}} result
#      set result
#  } {can't set "x": invalid listvar value}
#  test listbox-21.12 {ListboxListVarProc, cleanup item attributes} {
#      catch {destroy $_lb}
#      set x [list a b c d e f g]
#      listbox $_lb -listvar x
#      $_lb itemconfigure end -fg red
#      set x [list a b c d]
#      set x [list 0 1 2 3 4 5 6]
#      $_lb itemcget end -fg
#  } {}
#  test listbox-21.12 {ListboxListVarProc, cleanup item attributes} {
#      catch {destroy $_lb}
#      set x [list a b c d e f g]
#      listbox $_lb -listvar x
#      $_lb itemconfigure end -fg red
#      set x [list a b c d]
#      set x [list 0 1 2 3 4 5 6]
#      $_lb itemcget end -fg
#  } {}
#  test listbox-21.13 {listbox item configurations and listvar based deletions} {
#      catch {destroy $_lb}
#      catch {unset x}
#      listbox $_lb -listvar x
#      $_lb insert end a b c
#      $_lb itemconfigure 1 -fg red
#      set x [list b c]
#      $_lb itemcget 1 -fg
#  } red
#  test listbox-21.14 {listbox item configurations and listvar based inserts} {
#      catch {destroy $_lb}
#      catch {unset x}
#      listbox $_lb -listvar x
#      $_lb insert end a b c
#      $_lb itemconfigure 0 -fg red
#      set x [list 1 2 3 4 a b c]
#      $_lb itemcget 0 -fg
#  } red
#  test listbox-21.15 {ListboxListVarProc, update vertical scrollbar} {
#      catch {destroy $_lb}
#      catch {unset x}
#      set log {}
#      listbox $_lb -listvar x -yscrollcommand "record y" -font fixed -height 3
#      pack $_lb
#      update
#      lappend x a b c d e f
#      update
#      set log
#  } [list {y 0 1} {y 0 0.5}]
#  test listbox-21.16 {ListboxListVarProc, update vertical scrollbar} {
#      catch {destroy $_lb}
#      catch {unset x}
#      listbox $_lb -listvar x -height 3
#      pack $_lb
#      update
#      set x [list 0 1 2 3 4 5]
#      $_lb yview scroll 3 units
#      update
#      set result {}
#      lappend result [$_lb yview]
#      set x [lreplace $x 3 3]
#      set x [lreplace $x 3 3]
#      set x [lreplace $x 3 3]
#      update
#      lappend result [$_lb yview]
#      set result
#  } [list {0.5 1} {0 1}]
}

# UpdateHScrollbar

@log = ();
$lb = $mw->Listbox(-font => $fixed, -width => 10, -xscrollcommand => ["record", "x"])->pack;
$mw->update;
$lb->insert("end", "0000000000");
$mw->update;
$lb->insert("end", "00000000000000000000");
$mw->update;
TODO_xscrollcommand_problem {
    is_deeply(\@log, ["x 0 1", "x 0 1", "x 0 0.5"]);
};

SKIP: {
    skip("no itemconfigure in Tk800.x", 35)
	if $Tk::VERSION < 804;

    {
	my $lb = $mw->$Listbox;
	eval { $lb->itemconfigure(0) };
	like($@, qr{item number "0" out of range},
	     "ConfigureListboxItem error");
	$lb->destroy;
    }

    {
	my $lb = $mw->$Listbox;
	$lb->insert(qw(end a b c d));
	is_deeply([$lb->itemconfigure(0)],
		  [[qw(-background background Background), undef, undef],
		   [qw(-bg -background)],
		   [qw(-fg -foreground)],
		   [qw(-foreground foreground Foreground), undef, undef],
		   [qw(-selectbackground selectBackground Foreground), undef, undef],
		   [qw(-selectforeground selectForeground Background), undef, undef],
		  ], "itemconfigure options");
	$lb->destroy;
    }

    {
	my $lb = $mw->$Listbox;
	$lb->insert(qw(end a b c d));
	is_deeply([$lb->itemconfigure(0, -background)],[qw(-background background Background),undef,undef],
		  "itemconfigure with just one option"
		 );
	$lb->destroy;
    }

    {
	my $lb = $mw->$Listbox(Name => "lb");
	$lb->insert(qw(end a));
	eval { $lb->itemconfigure };
	like($@, qr{\Qwrong # args: should be ".lb itemconfigure index ?option? ?value? ?option value ...?"},
	     q{ConfigureListboxItem, wrong num args});
	$lb->destroy;
    }

    {
	my $lb = $mw->$Listbox(Name => "lb");
	my $i = 0;
	my @colors = qw(red orange yellow green blue darkblue violet);
	for my $color (@colors) {
	    $lb->insert(end => $color);
	    $lb->itemconfigure($i, -bg => $color);
	    $i++;
	}
	$lb->pack;
	$lb->update;
	for my $i (0 .. 6) {
	    is($lb->itemcget($i, -bg), $colors[$i],
	       q{ConfigureListboxItem, multiple calls, } . $colors[$i]);
	}
	$lb->destroy;
    }

    {
	my $lb = $mw->$Listbox;
	$lb->insert(qw(end a b c d));

	no warnings 'qw';

	foreach my $test
	    (
	     [qw(-background #ff0000 #ff0000 non-existent),
	      qr{unknown color name "non-existent"}],
	     [qw(-bg #ff0000 #ff0000 non-existent),
	      qr{unknown color name "non-existent"}],
	     [qw(-fg #110022 #110022 bogus),
	      qr{unknown color name "bogus"}],
	     [qw(-foreground #110022 #110022 bogus),
	      qr{unknown color name "bogus"}],
	     [qw(-selectbackground #110022 #110022 bogus),
	      qr{unknown color name "bogus"}],
	     [qw(-selectforeground #654321 #654321 bogus),
	      qr{unknown color name "bogus"}],
	    ) {
		my($name, $set_val, $get_val, $err_val, $err_msg) = @$test;
		$lb->itemconfigure(0, $name, $set_val);
		is($lb->itemcget(0, $name), $get_val, "itemconfigure $name");
		eval { $lb->itemconfigure(0, $name, $err_val) };
		like($@, $err_msg, "itemconfigure $name error ");
		is($lb->itemcget(0, $name), $get_val, "still same value after error");
	    }
	$lb->destroy;
    }

    {
	my $lb = $mw->$Listbox;
	$lb->insert(qw(end a b c d));
	is($lb->itemcget(0, -fg), undef, "itemcget with undef value");
	$lb->destroy;
    }

    {
	my $lb = $mw->$Listbox;
	$lb->insert(qw(end a b c d));
	$lb->itemconfigure(0, -fg => "red");
	is($lb->itemcget(0, -fg), "red");
	$lb->destroy;
    }

    {
	my $lb = $mw->$Listbox(Name => "lb");
	$lb->insert(qw(end a b c d));
	eval { $lb->itemcget(0) };
	like($@, qr{\Qwrong # args: should be ".lb itemcget index option"},
	     "itemcget error");
	$lb->destroy;
    }

    # No shortcuts in Perl/Tk, skipping shortcut test

    # General item configuration issues
    {
	my $lb = $mw->$Listbox(Name => "lb");
	$lb->insert(qw(end a));
	$lb->itemconfigure(0, -fg => "red");
	$lb->delete(0, "end");
	$lb->insert("end", "a");
	is($lb->itemcget(0, -fg), undef,
	   q{listbox item configurations and widget based deletions});
	$lb->destroy;
    }

    {
	my $lb = $mw->$Listbox(Name => "lb");
	$lb->insert(end => a => b => "c");
	$lb->itemconfigure(0 => -fg => "red");
	$lb->insert(0, 1, 2, 3, 4);
	is($lb->itemcget(0, -fg), undef,
	   q{listbox item configurations and widget based inserts});
	is($lb->itemcget(4, -fg), "red");
	$lb->destroy;
    }
}

# Additional visual itemconfigure tests
if ($visual) {
    skip("no itemconfigure in Tk800.x", 1) # XXX correct!
	if $Tk::VERSION < 804;

    my $lb = $mw->$Listbox->pack;
    my $next;
    my $b = $mw->Button(-text => "Next",
			-command => sub { $next++ })->pack;
    $lb->insert("end", 1..10);
    $lb->itemconfigure(0, -background => "red");
    $lb->waitVariable(\$next);
    $lb->itemconfigure(1, -background => "green");
    $lb->waitVariable(\$next);
    $lb->itemconfigure(0, -foreground => "white");
    $lb->waitVariable(\$next);
    $lb->itemconfigure(1, -foreground => "red");
    $lb->waitVariable(\$next);
    $lb->selectionSet(2,3);
    $lb->waitVariable(\$next);
    $lb->itemconfigure(2, -selectforeground => "cyan");
    $lb->waitVariable(\$next);
    $lb->itemconfigure(3, -selectbackground => "orange");
    $lb->waitVariable(\$next);
    $lb->destroy;
}

resetGridInfo();

sub record {
    push @log, join(" ", @_);
}

sub getsize {
    my $w = shift;
    my $geom = $w->geometry;
    $geom =~ /(\d+x\d+)/;
    $1;
}

sub resetGridInfo {
    # Some window managers, such as mwm, don't reset gridding information
    # unless the window is withdrawn and re-mapped.  If this procedure
    # isn't invoked, the window manager will stay in gridded mode, which
    # can cause all sorts of problems.  The "wm positionfrom" command is
    # needed so that the window manager doesn't ask the user to
    # manually position the window when it is re-mapped.
    #
    # On the other hand, calling these lines seem to cause strange
    # test failures with almost all window managers. The same lines of
    # code in Tcl/Tk seem also to be problematic. So run these lines only
    # for mwm
    if (eval {
	require Tk::Mwm;
	$mw->mwm('ismwmrunning');
    }) {
	$mw->withdraw;
	$mw->positionfrom('user');
	$mw->deiconify;
    }
}

# Procedure that creates a second listbox for checking things related
# to partially visible lines.
sub mkPartial {
    eval {
	$partial_top->destroy
	    if Tk::Exists($partial_top);
    };
    $partial_top = $mw->Toplevel;
    $partial_top->geometry('+0+0');
    $partial_lb = $partial_top->Listbox(-width => 30, -height => 5);
    $partial_lb->pack('-expand',1,'-fill','both');
    $partial_lb->insert('end','one','two','three','four','five','six','seven',
			'eight','nine','ten','eleven','twelve','thirteen',
			'fourteen','fifteen');
    $partial_top->update;
    my $geom = $partial_top->geometry;
    my($width, $height) = $geom =~ /(\d+)x(\d+)/;
    $partial_top->geometry($width . "x" . ($height-3));
    $partial_top->update;
}

__END__

