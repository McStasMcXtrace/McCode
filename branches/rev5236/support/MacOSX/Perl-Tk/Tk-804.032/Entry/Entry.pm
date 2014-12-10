package Tk::Entry;

# Converted from entry.tcl --
#
# This file defines the default bindings for Tk entry widgets.
#
# @(#) entry.tcl 1.22 94/12/17 16:05:14
#
# Copyright (c) 1992-1994 The Regents of the University of California.
# Copyright (c) 1994 Sun Microsystems, Inc.
# Copyright (c) 1995-2003 Nick Ing-Simmons. All rights reserved.
# This program is free software; you can redistribute it and/or

use vars qw($VERSION);
use strict;
$VERSION = '4.018'; # sprintf '4.%03d',q$Revision: #17 $ =~ /#(\d+)/;

# modify it under the same terms as Perl itself, subject
# to additional disclaimer in license.terms due to partial
# derivation from Tk4.0 sources.

use Tk::Widget ();
use Tk::Clipboard ();
use base  qw(Tk::Clipboard Tk::Widget);

import Tk qw(Ev $XS_VERSION);

Construct Tk::Widget 'Entry';

bootstrap Tk::Entry;

sub Tk_cmd { \&Tk::entry }

Tk::Methods('bbox','delete','get','icursor','index','insert','scan',
            'selection','validate','xview');

use Tk::Submethods ( 'selection' => [qw(clear range adjust present to from)],
		     'xview'     => [qw(moveto scroll)],
                   );

sub wordstart
{my ($w,$pos) = @_;
 my $string = $w->get;
 $pos = $w->index('insert')-1 unless(defined $pos);
 $string = substr($string,0,$pos);
 $string =~ s/\S*$//;
 length $string;
}

sub wordend
{my ($w,$pos) = @_;
 my $string = $w->get;
 my $anc = length $string;
 $pos = $w->index('insert') unless(defined $pos);
 $string = substr($string,$pos);
 $string =~ s/^(?:((?=\s)\s*|(?=\S)\S*))//x;
 $anc - length($string);
}

sub deltainsert
{
 my ($w,$d) = @_;
 return $w->index('insert')+$d;
}

#
# Bind --
# This procedure is invoked the first time the mouse enters an
# entry widget or an entry widget receives the input focus. It creates
# all of the class bindings for entries.
#
# Arguments:
# event - Indicates which event caused the procedure to be invoked
# (Enter or FocusIn). It is used so that we can carry out
# the functions of that event in addition to setting up
# bindings.
sub ClassInit
{
 my ($class,$mw) = @_;

 $class->SUPER::ClassInit($mw);

 # <<Cut>>, <<Copy>> and <<Paste>> defined in Tk::Clipboard
 $mw->bind($class,'<<Clear>>' => sub {
	       my $w = shift;
	       $w->delete("sel.first", "sel.last");
	   });
 $mw->bind($class,'<<PasteSelection>>' => [sub {
	       my($w, $x) = @_;
	       # XXX logic in Tcl/Tk version screwed up?
	       if (!$Tk::strictMotif && !$Tk::mouseMoved) {
		   $w->Paste($x);
	       }
	   }, Ev('x')]);

 # Standard Motif bindings:
 # The <Escape> binding is different from the Tcl/Tk version:
 $mw->bind($class,'<Escape>','selectionClear');

 $mw->bind($class,'<1>',['Button1',Ev('x'),Ev('y')]);
 $mw->bind($class,'<ButtonRelease-1>',['Button1Release',Ev('x'),Ev('y')]);
 $mw->bind($class,'<B1-Motion>',['Motion',Ev('x'),Ev('y')]);

 $mw->bind($class,'<Double-1>',['MouseSelect',Ev('x'),'word','sel.first']);
 $mw->bind($class,'<Double-Shift-1>',['MouseSelect',Ev('x'),'word']);
 $mw->bind($class,'<Triple-1>',['MouseSelect',Ev('x'),'line',0]);
 $mw->bind($class,'<Triple-Shift-1>',['MouseSelect',Ev('x'),'line']);

 $mw->bind($class,'<Shift-1>','Shift_1');


 $mw->bind($class,'<B1-Leave>',['AutoScan',Ev('x')]);
 $mw->bind($class,'<B1-Enter>','CancelRepeat');
 $mw->bind($class,'<Control-1>','Control_1');
 $mw->bind($class,'<Left>', ['SetCursor',Ev('deltainsert',-1)]);
 $mw->bind($class,'<Right>',['SetCursor',Ev('deltainsert',1)]);
 $mw->bind($class,'<Shift-Left>',['KeySelect',Ev('deltainsert',-1)]);
 $mw->bind($class,'<Shift-Right>',['KeySelect',Ev('deltainsert',1)]);
 $mw->bind($class,'<Control-Left>',['SetCursor',Ev(['wordstart'])]);
 $mw->bind($class,'<Control-Right>',['SetCursor',Ev(['wordend'])]);
 $mw->bind($class,'<Shift-Control-Left>',['KeySelect',Ev(['wordstart'])]);
 $mw->bind($class,'<Shift-Control-Right>',['KeySelect',Ev(['wordend'])]);
 $mw->bind($class,'<Home>',['SetCursor',0]);
 $mw->bind($class,'<Shift-Home>',['KeySelect',0]);
 $mw->bind($class,'<End>',['SetCursor','end']);
 $mw->bind($class,'<Shift-End>',['KeySelect','end']);
 $mw->bind($class,'<Delete>','Delete');

 $mw->bind($class,'<BackSpace>','Backspace');

 $mw->bind($class,'<Control-space>',['selectionFrom','insert']);
 $mw->bind($class,'<Select>',['selectionFrom','insert']);
 $mw->bind($class,'<Control-Shift-space>',['selectionAdjust','insert']);
 $mw->bind($class,'<Shift-Select>',['selectionAdjust','insert']);

 $mw->bind($class,'<Control-slash>',['selectionRange',0,'end']);
 $mw->bind($class,'<Control-backslash>','selectionClear');

 # $class->clipboardOperations($mw,qw[Copy Cut Paste]);

 $mw->bind($class,'<KeyPress>', ['Insert',Ev('A')]);

 # Ignore all Alt, Meta, and Control keypresses unless explicitly bound.
 # Otherwise, if a widget binding for one of these is defined, the
 # <KeyPress> class binding will also fire and insert the character,
 # which is wrong.  Ditto for Return, and Tab.

 $mw->bind($class,'<Alt-KeyPress>' ,'NoOp');
 $mw->bind($class,'<Meta-KeyPress>' ,'NoOp');
 $mw->bind($class,'<Control-KeyPress>' ,'NoOp');
 $mw->bind($class,'<Return>' ,'NoOp');
 $mw->bind($class,'<KP_Enter>' ,'NoOp');
 $mw->bind($class,'<Tab>' ,'NoOp');
 if ($mw->windowingsystem =~ /^(?:classic|aqua)$/)
  {
   $mw->bind($class,'<Command-KeyPress>', 'NoOp');
  }

 # On Windows, paste is done using Shift-Insert.  Shift-Insert already
 # generates the <<Paste>> event, so we don't need to do anything here.
 if ($Tk::platform ne 'MSWin32')
  {
   $mw->bind($class,'<Insert>','InsertSelection');
  }

 if (!$Tk::strictMotif)
  {
   # Additional emacs-like bindings:
   $mw->bind($class,'<Control-a>',['SetCursor',0]);
   $mw->bind($class,'<Control-b>',['SetCursor',Ev('deltainsert',-1)]);
   $mw->bind($class,'<Control-d>',['delete','insert']);
   $mw->bind($class,'<Control-e>',['SetCursor','end']);
   $mw->bind($class,'<Control-f>',['SetCursor',Ev('deltainsert',1)]);
   $mw->bind($class,'<Control-h>','Backspace');
   $mw->bind($class,'<Control-k>',['delete','insert','end']);

   $mw->bind($class,'<Control-t>','Transpose');

   # XXX The original Tcl/Tk bindings use NextWord/PreviousWord instead
   $mw->bind($class,'<Meta-b>',['SetCursor',Ev(['wordstart'])]);
   $mw->bind($class,'<Meta-d>',['delete','insert',Ev(['wordend'])]);
   $mw->bind($class,'<Meta-f>',['SetCursor',Ev(['wordend'])]);
   $mw->bind($class,'<Meta-BackSpace>',['delete',Ev(['wordstart']),'insert']);
   $mw->bind($class,'<Meta-Delete>',['delete',Ev(['wordstart']),'insert']);

   # A few additional bindings from John Ousterhout.
# XXX conflicts with <<Copy>>:  $mw->bind($class,'<Control-w>',['delete',Ev(['wordstart']),'insert']);
   $mw->bind($class,'<2>','Button_2');
   $mw->bind($class,'<B2-Motion>','B2_Motion');
# XXX superseded by <<PasteSelection>>: $mw->bind($class,'<ButtonRelease-2>','ButtonRelease_2');
  }
 return $class;
}


sub Shift_1
{
 my $w = shift;
 my $Ev = $w->XEvent;
 $Tk::selectMode = 'char';
 $w->selectionAdjust('@' . $Ev->x)
}


sub Control_1
{
 my $w = shift;
 my $Ev = $w->XEvent;
 $w->icursor('@' . $Ev->x)
}


sub Delete
{
 my $w = shift;
 if ($w->selectionPresent)
 {
 $w->deleteSelected
 }
 else
 {
 $w->delete('insert')
 }
}


sub InsertSelection
{
 my $w = shift;
 eval {local $SIG{__DIE__}; $w->Insert($w->GetSelection)}
}


# Original is ::tk::EntryScanMark
sub Button_2
{
 my $w = shift;
 my $Ev = $w->XEvent;
 $w->scan('mark',$Ev->x);
 $Tk::x = $Ev->x;
 $Tk::y = $Ev->y;
 $Tk::mouseMoved = 0
}


# Original is ::tk::EntryScanDrag
sub B2_Motion
{
 my $w = shift;
 my $Ev = $w->XEvent;
 # Make sure these exist, as some weird situations can trigger the
 # motion binding without the initial press.  [Tcl/Tk Bug #220269]
 if (!defined $Tk::x) { $Tk::x = $Ev->x }
 if (abs(($Ev->x-$Tk::x)) > 2)
 {
 $Tk::mouseMoved = 1
 }
 $w->scan('dragto',$Ev->x)
}


# XXX Not needed anymore
sub ButtonRelease_2
{
 my $w = shift;
 my $Ev = $w->XEvent;
 if (!$Tk::mouseMoved)
 {
 eval
 {local $SIG{__DIE__};
 $w->insert('insert',$w->SelectionGet);
 $w->SeeInsert;
 }
 }
}

sub Button1Release
{
 shift->CancelRepeat;
}

# ::tk::EntryClosestGap --
# Given x and y coordinates, this procedure finds the closest boundary
# between characters to the given coordinates and returns the index
# of the character just after the boundary.
#
# Arguments:
# w -           The entry window.
# x -           X-coordinate within the window.
sub ClosestGap
{
 my($w, $x) = @_;
 my $pos = $w->index('@'.$x);
 my @bbox = $w->bbox($pos);
 if ($x - $bbox[0] < $bbox[2] / 2)
  {
   return $pos;
  }
 $pos + 1;
}

# Button1 --
# This procedure is invoked to handle button-1 presses in entry
# widgets. It moves the insertion cursor, sets the selection anchor,
# and claims the input focus.
#
# Arguments:
# w - The entry window in which the button was pressed.
# x - The x-coordinate of the button press.
sub Button1
{
 my $w = shift;
 my $x = shift;
 $Tk::selectMode = 'char';
 $Tk::mouseMoved = 0;
 $Tk::pressX = $x;
 $w->icursor($w->ClosestGap($x));
 $w->selectionFrom('insert');
 $w->selectionClear;
 if ($w->cget('-state') ne 'disabled')
  {
   $w->focus()
  }
}

sub Motion
{
 my ($w,$x,$y) = @_;
 $Tk::x = $x; # XXX ?
 $w->MouseSelect($x);
}

# MouseSelect --
# This procedure is invoked when dragging out a selection with
# the mouse. Depending on the selection mode (character, word,
# line) it selects in different-sized units. This procedure
# ignores mouse motions initially until the mouse has moved from
# one character to another or until there have been multiple clicks.
#
# Arguments:
# w - The entry window in which the button was pressed.
# x - The x-coordinate of the mouse.
sub MouseSelect
{

 my $w = shift;
 my $x = shift;
 return if UNIVERSAL::isa($w, 'Tk::Spinbox') and $w->{_element} ne 'entry';
 $Tk::selectMode = shift if (@_);
 my $cur = $w->index($w->ClosestGap($x));
 return unless defined $cur;
 my $anchor = $w->index('anchor');
 return unless defined $anchor;
 $Tk::pressX ||= $x; # XXX Better use "if !defined $Tk::pressX"?
 if (($cur != $anchor) || (abs($Tk::pressX - $x) >= 3))
  {
   $Tk::mouseMoved = 1
  }
 my $mode = $Tk::selectMode;
 return unless $mode;
 if ($mode eq 'char')
  {
   # The Tcl version uses selectionRange here XXX
   if ($Tk::mouseMoved)
    {
     if ($cur < $anchor)
      {
       $w->selectionTo($cur)
      }
     else
      {
       $w->selectionTo($cur+1)
      }
    }
  }
 elsif ($mode eq 'word')
  {
   # The Tcl version uses tcl_wordBreakBefore/After here XXX
   if ($cur < $w->index('anchor'))
    {
     $w->selectionRange($w->wordstart($cur),$w->wordend($anchor-1))
    }
   else
    {
     $w->selectionRange($w->wordstart($anchor),$w->wordend($cur))
    }
  }
 elsif ($mode eq 'line')
  {
   $w->selectionRange(0,'end')
  }
 if (@_)
  {
   my $ipos = shift;
   eval {local $SIG{__DIE__}; $w->icursor($ipos) };
  }
 $w->idletasks;
}
# ::tk::EntryPaste --
# This procedure sets the insertion cursor to the current mouse position,
# pastes the selection there, and sets the focus to the window.
#
# Arguments:
# w -           The entry window.
# x -           X position of the mouse.
sub Paste
{
 my($w, $x) = @_;
 $w->icursor($w->ClosestGap($x));
 eval { local $SIG{__DIE__};
	$w->insert("insert", $w->GetSelection);
	$w->SeeInsert; # Perl/Tk extension
      };
 if ($w->cget(-state) ne 'disabled')
  {
   $w->focus;
  }
}
# AutoScan --
# This procedure is invoked when the mouse leaves an entry window
# with button 1 down.  It scrolls the window left or right,
# depending on where the mouse is, and reschedules itself as an
# 'after' command so that the window continues to scroll until the
# mouse moves back into the window or the mouse button is released.
#
# Arguments:
# w - The entry window.
# x - The x-coordinate of the mouse when it left the window.
sub AutoScan
{
 my $w = shift;
 my $x = shift;
 return if !Tk::Exists($w);
 if ($x >= $w->width)
  {
   $w->xview('scroll',2,'units')
  }
 elsif ($x < 0)
  {
   $w->xview('scroll',-2,'units')
  }
 else
  {
   return;
  }
 $w->MouseSelect($x);
 $w->RepeatId($w->after(50,['AutoScan',$w,$x]))
}
# KeySelect
# This procedure is invoked when stroking out selections using the
# keyboard. It moves the cursor to a new position, then extends
# the selection to that position.
#
# Arguments:
# w - The entry window.
# new - A new position for the insertion cursor (the cursor hasn't
# actually been moved to this position yet).
sub KeySelect
{
 my $w = shift;
 my $new = shift;
 if (!$w->selectionPresent)
  {
   $w->selectionFrom('insert');
   $w->selectionTo($new)
  }
 else
  {
   $w->selectionAdjust($new)
  }
 $w->icursor($new);
 $w->SeeInsert;
}
# Insert --
# Insert a string into an entry at the point of the insertion cursor.
# If there is a selection in the entry, and it covers the point of the
# insertion cursor, then delete the selection before inserting.
#
# Arguments:
# w - The entry window in which to insert the string
# s - The string to insert (usually just a single character)
sub Insert
{
 my $w = shift;
 my $s = shift;
 return unless (defined $s && $s ne '');
 eval
  {local $SIG{__DIE__};
   my $insert = $w->index('insert');
   if ($w->index('sel.first') <= $insert && $w->index('sel.last') >= $insert)
    {
     $w->deleteSelected
    }
  };
 $w->insert('insert',$s);
 $w->SeeInsert
}
# Backspace --
# Backspace over the character just before the insertion cursor.
#
# Arguments:
# w - The entry window in which to backspace.
sub Backspace
{
 my $w = shift;
 if ($w->selectionPresent)
  {
   $w->deleteSelected
  }
 else
  {
   my $x = $w->index('insert')-1;
   $w->delete($x) if ($x >= 0);
   # XXX Missing repositioning part from Tcl/Tk source
  }
}
# SeeInsert
# Make sure that the insertion cursor is visible in the entry window.
# If not, adjust the view so that it is.
#
# Arguments:
# w - The entry window.
sub SeeInsert
{
 my $w = shift;
 my $c = $w->index('insert');
#
# Probably a bug in your version of tcl/tk (I've not this problem
# when I test Entry in the widget demo for tcl/tk)
# index('\@0') give always 0. Consequence :
#    if you make <Control-E> or <Control-F> view is adapted
#    but with <Control-A> or <Control-B> view is not adapted
#
 my $left = $w->index('@0');
 if ($left > $c)
  {
   $w->xview($c);
   return;
  }
 my $x = $w->width;
 while ($w->index('@' . $x) <= $c && $left < $c)
  {
   $left += 1;
   $w->xview($left)
  }
}
# SetCursor
# Move the insertion cursor to a given position in an entry. Also
# clears the selection, if there is one in the entry, and makes sure
# that the insertion cursor is visible.
#
# Arguments:
# w - The entry window.
# pos - The desired new position for the cursor in the window.
sub SetCursor
{
 my $w = shift;
 my $pos = shift;
 $w->icursor($pos);
 $w->selectionClear;
 $w->SeeInsert;
}
# Transpose
# This procedure implements the 'transpose' function for entry widgets.
# It tranposes the characters on either side of the insertion cursor,
# unless the cursor is at the end of the line.  In this case it
# transposes the two characters to the left of the cursor.  In either
# case, the cursor ends up to the right of the transposed characters.
#
# Arguments:
# w - The entry window.
sub Transpose
{
 my $w = shift;
 my $i = $w->index('insert');
 $i++ if ($i < $w->index('end'));
 my $first = $i-2;
 return if ($first < 0);
 my $str = $w->get;
 my $new = substr($str,$i-1,1) . substr($str,$first,1);
 $w->delete($first,$i);
 $w->insert('insert',$new);
 $w->SeeInsert;
}

sub tabFocus
{
 my $w = shift;
 $w->selectionRange(0,'end');
 $w->icursor('end');
 $w->SUPER::tabFocus;
}

# ::tk::EntryGetSelection --
#
# Returns the selected text of the entry with respect to the -show option.
#
# Arguments:
# w -         The entry window from which the text to get
sub getSelected
{
 my $w = shift;
 return undef unless $w->selectionPresent;
 my $str = $w->get;
 my $show = $w->cget('-show');
 $str = $show x length($str) if (defined $show);
 my $s = $w->index('sel.first');
 my $e = $w->index('sel.last');
 return substr($str,$s,$e-$s);
}


1;

__END__
