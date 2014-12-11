#!/usr/bin/perl -w
# -*- perl -*-

#
# $Id: $
# Author: Slaven Rezic
#

use strict;

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

my $all_tests = 37;

plan tests => $all_tests;

SKIP: {
    skip('Set $ENV{TEST_AUTHOR} to a true value to run.', $all_tests)
	unless $ENV{TEST_AUTHOR};

    my $mw = tkinit;
    $mw->geometry("+10+10");
    my $l = $mw->Label(-text => "The Tk.xs test")->pack;
    
    {
        my $winid = $mw->PointToWindow(1,1);
        ok(defined $winid, "PointToWindow: got window id <$winid>");
    }
    
    # XXX FontInfo_* ?
    # XXX LangFontRank ?
    
    {
        ok(Tk::BLACK, "Colors: Black");
        ok(Tk::WHITE, "White");
        ok(Tk::NORMAL_BG, "Normal bg");
        ok(Tk::ACTIVE_BG, "Active bg");
        ok(Tk::SELECT_BG, "Select bg");
        ok(Tk::SELECT_FG, "Select fg");
        ok(Tk::TROUGH, "Trough");
        ok(Tk::INDICATOR, "Indicator");
        ok(Tk::DISABLED, "Disabled");
    }
    
    {
        is($mw->Count, 1, "Exactly one main window");
        $mw->Synchronize();
        pass("Called pTk_Synchronize");
    }
    
    {
        my $time_before = time;
        my $timeofday   = Tk::timeofday;
        my $time_after  = time;
        cmp_ok($time_before-1, "<=", $timeofday, "Time of day");
        cmp_ok($time_after+1, ">=", $timeofday);
    }
    
    {
        my($x1,$y1) = $mw->GetPointerCoords;
        my($x2,$y2) = $mw->pointerxy;
        is($x1, $x2, "GetPointerCoords and pointerxy should get the same");
        is($y1, $y2);
    }
    
    {
        ok($mw->IsTopLevel, "IsTopLevel positive");
        ok(!$l->IsTopLevel, "IsTopLevel negative");
        ok($mw->IsWidget, "IsWidget positive on toplevel");
        ok($l->IsWidget, "IsWidget positive on label");
        my $l2 = $mw->Label; $l2->destroy;
        ok(!$l2->IsWidget, "IsWidget negative (destroyed label)");
    }
    
    {
        ok(!$mw->IsMapped, "Toplevel not yet mapped");
    }
    
    # From now on we have a mapped window
    $mw->update;
    
    {
        ok($mw->IsMapped, "Toplevel is now mapped");
    }
    
    {
        my($x1,$y1) = $mw->WindowXY;
        my($x2,$y2) = ($mw->rootx, $mw->rooty);
        my($x3,$y3) = $mw->GetRootCoords;
    
        is($x1,$x2, "WindowXY gets the same as rootx/rooty");
        is($y1,$y2);
        is("$x1/$y1", "$x3/$y3", "GetRootCoords also the same");
    }
    
    {
        my $MYBITMAP = __PACKAGE__ . "::mybitmap";
        my $hbits = pack("b8"x5,
                         ".....11.",
                         "...11.1.",
                         ".11...1.",
                         "...11.1.",
                         ".....11.");
        $mw->DefineBitmap($MYBITMAP => 8,5, $hbits);
        $mw->Label(-bitmap => $MYBITMAP)->pack;
        pass("Using bitmap defined with DefineBitmap");
    
        my $pixmapid = $mw->GetBitmap($MYBITMAP);
        ok(defined $pixmapid, "GetBitmap returned <$pixmapid>");
        my $invalid = $mw->GetBitmap("this_really_does_not_exist");
        ok(!defined $invalid, "Invalid GetBitmap");
    }
    
    {
        $mw->XSync(0);
        pass("Called XSync(0)");
        $mw->XSync(1);
        pass("Called XSync(1)");
    }
    
    {
        $mw->MoveWindow(11,11);
        my($x1,$y1) = $mw->GetRootCoords;
        is("$x1/$y1", "11/11", "MoveWindow on toplevel");
    
        $mw->MoveToplevelWindow(12,12);
        # no change in GetRootCoords here, is this intended?
        pass("Called MoveToplevelWindow");
    
        $mw->MoveResizeWindow(13,13,200,50);
        pass("Called MoveResizeWindow");
    
        $mw->ResizeWindow(100,50);
        pass("Called ResizeWindow");
    }
    
    {
        my $font = $l->cget(-font);
        my $psname;
        $font->PostscriptFontName($psname);
        ok($psname, "Found postscript font name <$psname>");
    }
    
    {
        local $TODO = "GetFocusWin does not seem to return anything";
        isa_ok($mw->GetFocusWin, "Tk::Widget", "GetFocusWin returns a widget");
    }
    
    # missing: UnmanageGeometry, DisableButtonEvents, MakeAtom,
    # SendClientMessage, GetVRootGeometry, Colormap, Display, ScreenNumber etc.
}

__END__
