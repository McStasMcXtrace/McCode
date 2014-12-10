# Converted from menu.tcl --
#
# This file defines the default bindings for Tk menus and menubuttons.
# It also implements keyboard traversal of menus and implements a few
# other utility procedures related to menus.
#
# @(#) menu.tcl 1.34 94/12/19 17:09:09
#
# Copyright (c) 1992-1994 The Regents of the University of California.
# Copyright (c) 1994 Sun Microsystems, Inc.
#
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.

package Tk::Menu;
require Tk;
require Tk::Widget;
require Tk::Wm;
require Tk::Derived;
require Tk::Menu::Item;


use vars qw($VERSION);
$VERSION = '4.023'; # was: sprintf '4.%03d', q$Revision: #21 $ =~ /\D(\d+)\s*$/;

use strict;

use base  qw(Tk::Wm Tk::Derived Tk::Widget);

Construct Tk::Widget 'Menu';

sub Tk_cmd { \&Tk::_menu }

Tk::Methods('activate','add','clone','delete','entrycget','entryconfigure',
            'index','insert','invoke','post','postcascade','type',
            'unpost','yposition');

import Tk qw(Ev);

sub CreateArgs
{
 my ($package,$parent,$args) = @_;
 # Remove from hash %$args any configure-like
 # options which only apply at create time (e.g. -class for Frame)
 # return these as a list of -key => value pairs
 my @result = ();
 my $opt;
 foreach $opt (qw(-type -screen -visual -colormap))
  {
   my $val = delete $args->{$opt};
   push(@result, $opt => $val) if (defined $val);
  }
 return @result;
}

sub InitObject
{
 my ($menu,$args) = @_;
 my $menuitems = delete $args->{-menuitems};
 $menu->SUPER::InitObject($args);
 $menu->ConfigSpecs(-foreground => ['SELF']);
 if (defined $menuitems)
  {
   # If any other args do configure now
   if (%$args)
    {
     $menu->configure(%$args);
     %$args = ();
    }
   $menu->AddItems(@$menuitems)
  }
}

sub AddItems
{
 my $menu = shift;
 ITEM:
 while (@_)
  {
   my $item = shift;
   if (!ref($item))
    {
     $menu->separator;  # A separator
    }
   else
    {
     my ($kind,$name,%minfo) = ( @$item );
     my $invoke = delete $minfo{'-invoke'};
     if (defined $name)
      {
       $minfo{-label} = $name unless defined($minfo{-label});
       $menu->$kind(%minfo);
      }
     else
      {
       $menu->BackTrace("Don't recognize " . join(' ',@$item));
      }
    }  # A non-separator
  }
}

#
#-------------------------------------------------------------------------
# Elements of tkPriv that are used in this file:
#
# cursor - Saves the -cursor option for the posted menubutton.
# focus - Saves the focus during a menu selection operation.
# Focus gets restored here when the menu is unposted.
# inMenubutton - The name of the menubutton widget containing
# the mouse, or an empty string if the mouse is
# not over any menubutton.
# popup - If a menu has been popped up via tk_popup, this
# gives the name of the menu. Otherwise this
# value is empty.
# postedMb - Name of the menubutton whose menu is currently
# posted, or an empty string if nothing is posted
# A grab is set on this widget.
# relief - Used to save the original relief of the current
# menubutton.
# window - When the mouse is over a menu, this holds the
# name of the menu; it's cleared when the mouse
# leaves the menu.
#-------------------------------------------------------------------------
#-------------------------------------------------------------------------
# Overall note:
# This file is tricky because there are four different ways that menus
# can be used:
#
# 1. As a pulldown from a menubutton. This is the most common usage.
# In this style, the variable tkPriv(postedMb) identifies the posted
# menubutton.
# 2. As a torn-off menu copied from some other menu. In this style
# tkPriv(postedMb) is empty, and the top-level menu is no
# override-redirect.
# 3. As an option menu, triggered from an option menubutton. In thi
# style tkPriv(postedMb) identifies the posted menubutton.
# 4. As a popup menu. In this style tkPriv(postedMb) is empty and
# the top-level menu is override-redirect.
#
# The various binding procedures use the state described above to
# distinguish the various cases and take different actions in each
# case.
#-------------------------------------------------------------------------
# Bind --
# This procedure is invoked the first time the mouse enters a menubutton
# widget or a menubutton widget receives the input focus. It creates
# all of the class bindings for both menubuttons and menus.
#
# Arguments:
# w - The widget that was just entered or just received
# the input focus.
# event - Indicates which event caused the procedure to be invoked
# (Enter or FocusIn). It is used so that we can carry out
# the functions of that event in addition to setting up
# bindings.
sub ClassInit
{
 my ($class,$mw) = @_;
 # Must set focus when mouse enters a menu, in order to allow
 # mixed-mode processing using both the mouse and the keyboard.
 $mw->bind($class,'<FocusIn>', 'NoOp');
 $mw->bind($class,'<Enter>', 'Enter');
 $mw->bind($class,'<Leave>', ['Leave',Ev('X'),Ev('Y'),Ev('s')]);
 $mw->bind($class,'<Motion>', ['Motion',Ev('x'),Ev('y'),Ev('s')]);
 $mw->bind($class,'<ButtonPress>','ButtonDown');
 $mw->bind($class,'<ButtonRelease>',['Invoke',1]);
 $mw->bind($class,'<space>',['Invoke',0]);
 $mw->bind($class,'<Return>',['Invoke',0]);
 $mw->bind($class,'<Escape>','Escape');
 $mw->bind($class,'<Left>','LeftArrow');
 $mw->bind($class,'<Right>','RightArrow');
 $mw->bind($class,'<Up>','UpArrow');
 $mw->bind($class,'<Down>','DownArrow');
 $mw->bind($class,'<KeyPress>', ['TraverseWithinMenu',Ev('K')]);
 $mw->bind($class,'<Alt-KeyPress>', ['TraverseWithinMenu',Ev('K')]);
 return $class;
}

sub UpArrow
{
 my $menu = shift;
 if ($menu->cget('-type') eq 'menubar')
  {
   $menu->NextMenu('left');
  }
 else
  {
   $menu->NextEntry(-1);
  }
}

sub DownArrow
{
 my $menu = shift;
 if ($menu->cget('-type') eq 'menubar')
  {
   $menu->NextMenu('right');
  }
 else
  {
   $menu->NextEntry(1);
  }
}

sub LeftArrow
{
 my $menu = shift;
 if ($menu->cget('-type') eq 'menubar')
  {
   $menu->NextEntry(-1);
  }
 else
  {
   $menu->NextMenu('left');
  }
}

sub RightArrow
{
 my $menu = shift;
 if ($menu->cget('-type') eq 'menubar')
  {
   $menu->NextEntry(1);
  }
 else
  {
   $menu->NextMenu('right');
  }
}



# Unpost --
# This procedure unposts a given menu, plus all of its ancestors up
# to (and including) a menubutton, if any. It also restores various
# values to what they were before the menu was posted, and releases
# a grab if there's a menubutton involved. Special notes:
# 1. It's important to unpost all menus before releasing the grab, so
# that any Enter-Leave events (e.g. from menu back to main
# application) have mode NotifyGrab.
# 2. Be sure to enclose various groups of commands in "catch" so that
# the procedure will complete even if the menubutton or the menu
# or the grab window has been deleted.
#
# Arguments:
# menu - Name of a menu to unpost. Ignored if there
# is a posted menubutton.
sub Unpost
{
 my $menu = shift;
 my $mb = $Tk::postedMb;

 # Restore focus right away (otherwise X will take focus away when
 # the menu is unmapped and under some window managers (e.g. olvwm)
 # we'll lose the focus completely).

 eval {local $SIG{__DIE__}; $Tk::focus->focus() } if (defined $Tk::focus);
 undef $Tk::focus;

 # Unpost menu(s) and restore some stuff that's dependent on
 # what was posted.
 eval {local $SIG{__DIE__};
   if (defined $mb)
     {
      $menu = $mb->cget('-menu');
      $menu->unpost();
      $Tk::postedMb = undef;
      $mb->configure('-cursor',$Tk::cursor);
      $mb->configure('-relief',$Tk::relief)
     }
    elsif (defined $Tk::popup)
     {
      $Tk::popup->unpost();
      my $grab = $Tk::popup->grabCurrent;
      $grab->grabRelease if (defined $grab);

      undef $Tk::popup;
     }
    elsif (defined $menu && ref $menu &&
           $menu->cget('-type') ne 'menubar' &&
           $menu->cget('-type') ne 'tearoff'
          )
     {
      # We're in a cascaded sub-menu from a torn-off menu or popup.
      # Unpost all the menus up to the toplevel one (but not
      # including the top-level torn-off one) and deactivate the
      # top-level torn off menu if there is one.
      while (1)
       {
        my $parent = $menu->parent;
        last if (!$parent->IsMenu || !$parent->ismapped);
        $parent->postcascade('none');
        $parent->GenerateMenuSelect;
        $parent->activate('none');
        my $type = $parent->cget('-type');
        last if ($type eq 'menubar' || $type eq 'tearoff');
        $menu = $parent
       }
      $menu->unpost() if ($menu->cget('-type') ne 'menubar');
     }
  };
 warn "$@" if ($@);
 if ($Tk::tearoff || $Tk::menubar)
  {
   # Release grab, if any.
   if (defined $menu && ref $menu)
    {
     my $grab = $menu->grabCurrent;
     $grab->grabRelease if (defined $grab);
    }
   RestoreOldGrab();
   if ($Tk::menubar)
    {
     $Tk::menubar->configure(-cursor => $Tk::cursor);
     undef $Tk::menubar;
    }
   if ($Tk::platform ne 'unix')
    {
     undef $Tk::tearoff;
    }
  }
}

sub RestoreOldGrab
{
 if (defined $Tk::oldGrab)
  {
   eval
    {
     local $SIG{__DIE__};
     if ($Tk::grabStatus eq 'global')
      {
       $Tk::oldGrab->grabGlobal;
      }
     else
      {
       $Tk::oldGrab->grab;
      }
    };
   undef $Tk::oldGrab;
  }
}

sub typeIS
{my $w = shift;
 my $type = $w->type(shift);
 return defined $type && $type eq shift;
}

# Motion --
# This procedure is called to handle mouse motion events for menus.
# It does two things. First, it resets the active element in the
# menu, if the mouse is over the menu.  Second, if a mouse button
# is down, it posts and unposts cascade entries to match the mouse
# position.
#
# Arguments:
# menu - The menu window.
# y - The y position of the mouse.
# state - Modifier state (tells whether buttons are down).
sub Motion
{
 my $menu = shift;
 my $x = shift;
 my $y = shift;
 my $state = shift;
 my $t     = $menu->cget('-type');

 if ($menu->IS($Tk::window))
  {
   if ($menu->cget('-type') eq 'menubar')
    {
#    if (defined($Tk::focus) && $Tk::focus != $menu)
      {
       $menu->activate("\@$x,$y");
       $menu->GenerateMenuSelect;
      }
    }
   else
    {
     $menu->activate("\@$x,$y");
     $menu->GenerateMenuSelect;
    }
  }
 if (($state & 0x1f00) != 0)
  {
   $menu->postcascade('active')
  }
}
# ButtonDown --
# Handles button presses in menus. There are a couple of tricky things
# here:
# 1. Change the posted cascade entry (if any) to match the mouse position.
# 2. If there is a posted menubutton, must grab to the menubutton so
#    that it can track mouse motions over other menubuttons and change
#    the posted menu.
# 3. If there's no posted menubutton (e.g. because we're a torn-off menu
#    or one of its descendants) must grab to the top-level menu so that
#    we can track mouse motions across the entire menu hierarchy.

#
# Arguments:
# menu - The menu window.
sub ButtonDown
{
 my $menu = shift;
 return if (!$menu->viewable);
 $menu->postcascade('active');
 if (defined $Tk::postedMb && $Tk::postedMb->viewable)
  {
   $Tk::postedMb->grabGlobal
  }
 else
  {
   while ($menu->cget('-type') eq 'normal'
          && $menu->parent->IsMenu
          && $menu->parent->ismapped
         )
    {
     $menu = $menu->parent;
    }

   if (!defined $Tk::menuBar)
    {
     $Tk::menuBar = $menu;
     $Tk::cursor = $menu->cget('-cursor');
     $menu->configure(-cursor => 'arrow');
    }

   # Don't update grab information if the grab window isn't changing.
   # Otherwise, we'll get an error when we unpost the menus and
   # restore the grab, since the old grab window will not be viewable
   # anymore.

   $menu->SaveGrabInfo unless ($menu->IS($menu->grabCurrent));

   # Must re-grab even if the grab window hasn't changed, in order
   # to release the implicit grab from the button press.

   $menu->grabGlobal if ($Tk::platform eq 'unix');
  }
}

sub Enter
{
 my $w = shift;
 my $ev = $w->XEvent;
 $Tk::window = $w;
 if ($w->cget('-type') eq 'tearoff')
  {
   if ($ev->m ne 'NotifyUngrab')
    {
     $w->SetFocus if ($Tk::platform eq 'unix');
    }
  }
 $w->Motion($ev->x, $ev->y, $ev->s);
}

# Leave --
# This procedure is invoked to handle Leave events for a menu. It
# deactivates everything unless the active element is a cascade element
# and the mouse is now over the submenu.
#
# Arguments:
# menu - The menu window.
# rootx, rooty - Root coordinates of mouse.
# state - Modifier state.
sub Leave
{
 my $menu = shift;
 my $rootx = shift;
 my $rooty = shift;
 my $state = shift;
 undef $Tk::window;
 return if ($menu->index('active') eq 'none');
 if ($menu->typeIS('active','cascade'))
  {
   my $c = $menu->Containing($rootx,$rooty);
   return if (defined $c && $menu->entrycget('active','-menu')->IS($c));
  }
 $menu->activate('none');
 $menu->GenerateMenuSelect;
}

# Invoke --
# This procedure is invoked when button 1 is released over a menu.
# It invokes the appropriate menu action and unposts the menu if
# it came from a menubutton.
#
# Arguments:
# w - Name of the menu widget.
sub Invoke
{
 my $w = shift;
 my $release = shift;

 if ($release && !defined($Tk::window))
  {
   # Mouse was pressed over a menu without a menu button, then
   # dragged off the menu (possibly with a cascade posted) and
   # released.  Unpost everything and quit.

   $w->postcascade('none');
   $w->activate('none');
   $w->eventGenerate('<<MenuSelect>>');
   $w->Unpost;
   return;
  }

 my $type = $w->type('active');
 if ($w->typeIS('active','cascade'))
  {
   $w->postcascade('active');
   my $menu = $w->entrycget('active','-menu');
   $menu->FirstEntry() if (defined $menu);
  }
 elsif ($w->typeIS('active','tearoff'))
  {
   $w->tearOffMenu();
   $w->Unpost();
  }
 elsif ($w->typeIS('active','menubar'))
  {
   $w->postcascade('none');
   $w->activate('none');
   $w->eventGenerate('<<MenuSelect>>');
   $w->Unpost;
  }
 else
  {
   $w->Unpost();
   $w->invoke('active')
  }
}
# Escape --
# This procedure is invoked for the Cancel (or Escape) key. It unposts
# the given menu and, if it is the top-level menu for a menu button,
# unposts the menu button as well.
#
# Arguments:
# menu - Name of the menu window.
sub Escape
{
 my $menu = shift;
 my $parent = $menu->parent;
 if (!$parent->IsMenu)
  {
   $menu->Unpost()
  }
 elsif ($parent->cget('-type') eq 'menubar')
  {
   $menu->Unpost;
   RestoreOldGrab();
  }
 else
  {
   $menu->NextMenu(-1)
  }
}
# LeftRight --
# This procedure is invoked to handle "left" and "right" traversal
# motions in menus. It traverses to the next menu in a menu bar,
# or into or out of a cascaded menu.
#
# Arguments:
# menu - The menu that received the keyboard
# event.
# direction - Direction in which to move: "left" or "right"
sub NextMenu
{
 my $menu = shift;
 my $direction = shift;
 # First handle traversals into and out of cascaded menus.
 my $count;
 if ($direction eq 'right')
  {
   $count = 1;
   if ($menu->typeIS('active','cascade'))
    {
     $menu->postcascade('active');
     my $m2 = $menu->entrycget('active','-menu');
     $m2->FirstEntry if (defined $m2);
     return;
    }
   else
    {
     my $parent = $menu->parent;
     while ($parent->PathName ne '.')
      {
       if ($parent->IsMenu && $parent->cget('-type') eq 'menubar')
        {
         $parent->SetFocus;
         $parent->NextEntry(1);
         return;
        }
       $parent = $parent->parent;
      }
    }
  }
 else
  {
   $count = -1;
   my $m2 = $menu->parent;
   if ($m2->IsMenu)
    {
     $menu->activate('none');
     $menu->GenerateMenuSelect;
     $m2->SetFocus;

     $m2->postcascade('none');

     if ($m2->cget('-type') ne 'menubar')
      {
       return;
      }
    }
  }
 # Can't traverse into or out of a cascaded menu. Go to the next
 # or previous menubutton, if that makes sense.

 my $m2 = $menu->parent;
 if ($m2->IsMenu)
  {
   if ($m2->cget('-type') eq 'menubar')
    {
     $m2->SetFocus;
     $m2->NextEntry(-1);
     return;
    }
  }

 my $w = $Tk::postedMb;
 return unless defined $w;
 my @buttons = $w->parent->children;
 my $length = @buttons;
 my $i = Tk::lsearch(\@buttons,$w)+$count;
 my $mb;
 while (1)
  {
   while ($i < 0)
    {
     $i += $length
    }
   while ($i >= $length)
    {
     $i += -$length
    }
   $mb = $buttons[$i];
   last if ($mb->IsMenubutton && $mb->cget('-state') ne 'disabled'
            && defined($mb->cget('-menu'))
            && $mb->cget('-menu')->index('last') ne 'none'
           );
   return if ($mb == $w);
   $i += $count
  }
 $mb->PostFirst();
}
# NextEntry --
# Activate the next higher or lower entry in the posted menu,
# wrapping around at the ends. Disabled entries are skipped.
#
# Arguments:
# menu - Menu window that received the keystroke.
# count - 1 means go to the next lower entry,
# -1 means go to the next higher entry.
sub NextEntry
{
 my $menu = shift;
 my $count = shift;
 if ($menu->index('last') eq 'none')
  {
   return;
  }
 my $length = $menu->index('last')+1;
 my $quitAfter = $length;
 my $active = $menu->index('active');
 my $i = ($active eq 'none') ? 0 : $active+$count;
 while (1)
  {
   return if ($quitAfter <= 0);
   while ($i < 0)
    {
     $i += $length
    }
   while ($i >= $length)
    {
     $i += -$length
    }
   my $state = eval {local $SIG{__DIE__};  $menu->entrycget($i,'-state') };
   last if (defined($state) && $state ne 'disabled');
   return if ($i == $active);
   $i += $count;
   $quitAfter -= 1;
  }
 $menu->activate($i);
 $menu->GenerateMenuSelect;
 if ($menu->cget('-type') eq 'menubar' && $menu->type($i) eq 'cascade')
  {
   my $cascade = $menu->entrycget($i, '-menu');
   $menu->postcascade($i);
   $cascade->FirstEntry if (defined $cascade);
  }
}


# tkTraverseWithinMenu
# This procedure implements keyboard traversal within a menu. It
# searches for an entry in the menu that has "char" underlined. If
# such an entry is found, it is invoked and the menu is unposted.
#
# Arguments:
# w - The name of the menu widget.
# char - The character to look for; case is
# ignored. If the string is empty then
# nothing happens.
sub TraverseWithinMenu
{
 my $w = shift;
 my $char = shift;
 return unless (defined $char);
 $char = "\L$char";
 my $last = $w->index('last');
 return if ($last eq 'none');
 for (my $i = 0;$i <= $last;$i += 1)
  {
   my $label = eval {local $SIG{__DIE__};  $w->entrycget($i,'-label') };
   next unless defined($label);
   my $ul = $w->entrycget($i,'-underline');
   if (defined $ul && $ul >= 0)
    {
     $label = substr("\L$label",$ul,1);
     if (defined($label) && $label eq $char)
      {
       if ($w->type($i) eq 'cascade')
        {
         $w->postcascade($i);
         $w->activate($i);
         my $m2 = $w->entrycget($i,'-menu');
         $m2->FirstEntry if (defined $m2);
        }
       else
        {
         $w->Unpost();
         $w->invoke($i);
        }
       return;
      }
    }
  }
}

sub FindMenu
{
 my ($menu,$char) = @_;
 if ($menu->cget('-type') eq 'menubar')
  {
   if (!defined($char) || $char eq '')
    {
     $menu->FirstEntry;
    }
   else
    {
     $menu->TraverseWithinMenu($char);
    }
   return $menu;
  }
 return undef;
}


# FirstEntry --
# Given a menu, this procedure finds the first entry that isn't
# disabled or a tear-off or separator, and activates that entry.
# However, if there is already an active entry in the menu (e.g.,
# because of a previous call to tkPostOverPoint) then the active
# entry isn't changed. This procedure also sets the input focus
# to the menu.
#
# Arguments:
# menu - Name of the menu window (possibly empty).
sub FirstEntry
{
 my $menu = shift;
 return if (!defined($menu) || $menu eq '' || !ref($menu));
 $menu->SetFocus;
 return if ($menu->index('active') ne 'none');
 my $last = $menu->index('last');
 return if ($last eq 'none');
 for (my $i = 0;$i <= $last;$i += 1)
  {
   my $state = eval {local $SIG{__DIE__};  $menu->entrycget($i,'-state') };
   if (defined $state && $state ne 'disabled' && !$menu->typeIS($i,'tearoff'))
    {
     $menu->activate($i);
     $menu->GenerateMenuSelect;
     if ($menu->type($i) eq 'cascade')
      {
       my $cascade = $menu->entrycget($i,'-menu');
       if (0 && defined $cascade)
        {
         $menu->postcascade($i);
         $cascade->FirstEntry;
        }
      }
     return;
    }
  }
}

# FindName --
# Given a menu and a text string, return the index of the menu entry
# that displays the string as its label. If there is no such entry,
# return an empty string. This procedure is tricky because some names
# like "active" have a special meaning in menu commands, so we can't
# always use the "index" widget command.
#
# Arguments:
# menu - Name of the menu widget.
# s - String to look for.
sub FindName
{
 my $menu = shift;
 my $s = shift;
 my $i = undef;
 if ($s !~ /^active$|^last$|^none$|^[0-9]|^@/)
  {
   $i = eval {local $SIG{__DIE__};  $menu->index($s) };
   return $i;
  }
 my $last = $menu->index('last');
 return if ($last eq 'none');
 for ($i = 0;$i <= $last;$i += 1)
  {
   my $label = eval {local $SIG{__DIE__};  $menu->entrycget($i,'-label') };
   return $i if (defined $label && $label eq $s);
  }
 return undef;
}
# PostOverPoint --
# This procedure posts a given menu such that a given entry in the
# menu is centered over a given point in the root window. It also
# activates the given entry.
#
# Arguments:
# menu - Menu to post.
# x, y - Root coordinates of point.
# entry - Index of entry within menu to center over (x,y).
# If omitted or specified as {}, then the menu's
# upper-left corner goes at (x,y).
sub PostOverPoint
{
 my $menu = shift;
 my $x = shift;
 my $y = shift;
 my $entry = shift;
 if (defined $entry)
  {
   if ($entry == $menu->index('last'))
    {
     $y -= ($menu->yposition($entry)+$menu->height)/2;
    }
   else
    {
     $y -= ($menu->yposition($entry)+$menu->yposition($entry+1))/2;
    }
   $x -= $menu->reqwidth/2;
  }
 $menu->post($x,$y);
 if (defined($entry) && $menu->entrycget($entry,'-state') ne 'disabled')
  {
   $menu->activate($entry);
   $menu->GenerateMenuSelect;
  }
}
# tk_popup --
# This procedure pops up a menu and sets things up for traversing
# the menu and its submenus.
#
# Arguments:
# menu - Name of the menu to be popped up.
# x, y - Root coordinates at which to pop up the
# menu.
# entry - Index of a menu entry to center over (x,y).
# If omitted or specified as {}, then menu's
# upper-left corner goes at (x,y).
sub Post
{
 my $menu = shift;
 return unless (defined $menu);
 my $x = shift;
 my $y = shift;
 my $entry = shift;
 Unpost(undef) if (defined($Tk::popup) || defined($Tk::postedMb));
 $menu->PostOverPoint($x,$y,$entry);
 if ($Tk::platform eq 'unix' && $menu->viewable)
  {
   $menu->grabGlobal;
   $Tk::popup = $menu;
   $Tk::focus = $menu->focusCurrent;
   $menu->focus();
  }
}

sub SetFocus
{
 my $menu = shift;
 $Tk::focus = $menu->focusCurrent if (!defined($Tk::focus));
 $menu->focus;
}

sub GenerateMenuSelect
{
 my $menu = shift;
 $Tk::activeMenu = $menu;
 $Tk::activeItem = $menu->index('active');
 $menu->eventGenerate('<<MenuSelect>>');  # FIXME
}

# Converted from tearoff.tcl --
#
# This file contains procedures that implement tear-off menus.
#
# @(#) tearoff.tcl 1.3 94/12/17 16:05:25
#
# Copyright (c) 1994 The Regents of the University of California.
# Copyright (c) 1994 Sun Microsystems, Inc.
#
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#
# tkTearoffMenu --
# Given the name of a menu, this procedure creates a torn-off menu
# that is identical to the given menu (including nested submenus).
# The new torn-off menu exists as a toplevel window managed by the
# window manager. The return value is the name of the new menu.
#
# Arguments:
# w - The menu to be torn-off (duplicated).
sub tearOffMenu
{
 my $w = shift;
 my $x = (@_) ? shift : 0;
 my $y = (@_) ? shift : 0;

 $x = $w->rootx if $x == 0;
 $y = $w->rooty if $y == 0;

 # Find a unique name to use for the torn-off menu. Find the first
 # ancestor of w that is a toplevel but not a menu, and use this as
 # the parent of the new menu. This guarantees that the torn off
 # menu will be on the same screen as the original menu. By making
 # it a child of the ancestor, rather than a child of the menu, it
 # can continue to live even if the menu is deleted; it will go
 # away when the toplevel goes away.

 my $parent = $w->parent;
 while ($parent->toplevel != $parent || $parent->IsMenu)
  {
   $parent = $parent->parent;
  }
 my $menu = $w->clone($parent->PathName,'tearoff');

 # Pick a title for the new menu by looking at the parent of the
 # original: if the parent is a menu, then use the text of the active
 # entry. If it's a menubutton then use its text.
 my $title = $w->cget('-title');
 # print ref($w),' ',$w->PathName," ",$menu->PathName," $w\n";
 unless (defined $title && length($title))
  {
   $parent = $w->parent;
   if ($parent)
    {
     if ($parent->IsMenubutton)
      {
       $title = $parent->cget('-text');
      }
     elsif ($parent->IsMenu)
      {
       $title = $parent->entrycget('active','-label');
      }
    }
  }
 $menu->title($title) if (defined $title && length($title));
 $menu->post($x,$y);

 if (!Tk::Exists($menu))
  {
   return;
  }

 # Set tkPriv(focus) on entry: otherwise the focus will get lost
 # after keyboard invocation of a sub-menu (it will stay on the
 # submenu).


 # This seems to conflict with <Enter> class binding above
 # if this fires before the class binding the wrong thing
 # will get saved in $Tk::focus
 # $menu->bind('<Enter>','EnterFocus');
 $menu->Callback('-tearoffcommand');

 # Strangely tear-off menus do not work in tkpod and Tk804.027.
 # Explicitely setting normal state helps here - why?
 $menu->state("normal");

 return $menu;
}

# tkMenuDup --
# Given a menu (hierarchy), create a duplicate menu (hierarchy)
# in a given window.
#
# Arguments:
# src - Source window. Must be a menu. It and its
# menu descendants will be duplicated at path.
# path - Name to use for topmost menu in duplicate
# hierarchy.

sub tkMenuDup
{
 my ($src,$path,$type) = @_;
 my ($pname,$name) = $path =~ /^(.*)\.([^\.]*)$/;
 ($name) = $src->PathName =~ /^.*\.([^\.]*)$/ unless $name;
 my $parent = ($pname) ? $src->Widget($pname) : $src->MainWindow;
 my %args  = (Name => $name, -type => $type);
 foreach my $option ($src->configure())
  {
   next if (@$option == 2);
   $args{$$option[0]} = $$option[4] unless exists $args{$$option[0]};
  }
 my $dst = ref($src)->new($parent,%args);
 # print "MenuDup $src $path $name $type ->",$dst->PathName,"\n";
 $_[1] = $dst;
 if ($type eq 'tearoff')
  {
   $dst->transient($parent->toplevel);
  }
 my $last = $src->index('last');
 if ($last ne 'none')
  {
   for (my $i = $src->cget('-tearoff'); $i <= $last; $i++)
    {
     my $type = $src->type($i);
     if (defined $type)
      {
       my @args = ();
       foreach my $option ($src->entryconfigure($i))
        {
         next if (@$option == 2);
         push(@args,$$option[0],$$option[4]) if (defined $$option[4]);
        }
       $dst->add($type,@args);
      }
    }
  }
 # Duplicate the binding tags and bindings from the source menu.
 my @bindtags = $src->bindtags;
 $path = $src->PathName;
 foreach (@bindtags)
  {
   $_ = $dst if ($_ eq $path);
  }
 $dst->bindtags([@bindtags]);
 foreach my $event ($src->bind)
  {
   my $cb = $src->bind($event);
#   print "$event => $cb\n";
   $dst->bind($event,$cb->Substitute($src,$dst));
  }
 return $dst;
}



# Some convenience methods

sub separator   { require Tk::Menu::Item; shift->Separator(@_);   }
sub cascade     { require Tk::Menu::Item; shift->Cascade(@_);     }
sub checkbutton { require Tk::Menu::Item; shift->Checkbutton(@_); }
sub radiobutton { require Tk::Menu::Item; shift->Radiobutton(@_); }

sub command
{
 my ($menu,%args) = @_;
 require Tk::Menu::Item;
 if (exists $args{-button})
  {
   # Backward compatible stuff from 'Menubar'
   my $button = delete $args{-button};
   $button = ['Misc', -underline => 0 ] unless (defined $button);
   my @bargs = ();
   ($button,@bargs) = @$button if (ref($button) && ref $button eq 'ARRAY');
   $menu = $menu->Menubutton(-label => $button, @bargs);
  }
 $menu->Command(%args);
}

sub Menubutton
{
 my ($menu,%args) = @_;
 my $name = delete($args{'-text'}) || $args{'-label'};;
 $args{'-label'} = $name if (defined $name);
 my $items = delete $args{'-menuitems'};
 foreach my $opt (qw(-pack -after -before -side -padx -ipadx -pady -ipady -fill))
  {
   delete $args{$opt};
  }
 if (defined($name) && !defined($args{-underline}))
  {
   my $underline = ($name =~ s/^(.*)~/$1/) ? length($1): undef;
   if (defined($underline) && ($underline >= 0))
    {
     $args{-underline} = $underline;
     $args{-label} = $name;
    }
  }
 my $hash = $menu->TkHash('MenuButtons');
 my $mb = $hash->{$name};
 if (defined $mb)
  {
   delete $args{'-tearoff'}; # too late!
   $mb->configure(%args) if %args;
  }
 else
  {
   $mb = $menu->cascade(%args);
   $hash->{$name} = $mb;
  }
 $mb->menu->AddItems(@$items) if defined($items) && @$items;
 return $mb;
}

sub BalloonInfo
{
 my ($menu,$balloon,$X,$Y,@opt) = @_;
 my $i = $menu->index('active');
 if ($i eq 'none')
  {
   my $y = $Y - $menu->rooty;
   $i = $menu->index("\@$y");
  }
 foreach my $opt (@opt)
  {
   my $info = $balloon->GetOption($opt,$menu);
   if ($opt =~ /^-(statusmsg|balloonmsg)$/ && UNIVERSAL::isa($info,'ARRAY'))
    {
     $balloon->Subclient($i);
     return '' if $i eq 'none';
     return ${$info}[$i] || '';
    }
   return $info;
  }
}

sub MasterMenu
{
 my ($menu) = @_;
 my $pathname = $menu->PathName;
 my $master_menu;
 if ($pathname =~ m{#})
  {
   my $master_pathname = (split m{\.}, $pathname)[-1];
   $master_pathname =~ s{#}{.}g;
   $master_menu = $menu->Widget($master_pathname);
   if (0 && !Tk::Exists($master_menu))
    {
     warn "Cannot find master menu <$master_pathname>";
    }
  }
 $master_menu;
}


# ::tk::AmpMenuArgs --
# Processes arguments for a menu entry, turning -label option into
# -label and -underline options, returned by ::tk::UnderlineAmpersand.
#
sub AmpArgs
{
 my ($w, $add, $type, %args) = @_;
 my @options;
 while(my($opt,$val) = each %args)
  {
   if ($opt eq "-label")
    {
     my ($newtext,$under) = $w->UnderlineAmpersand($val);
     push @options, -label => $newtext, -underline => $under;
    }
   else
    {
     push @options, $opt, $val;
    }
  }
 $w->$type(@options);
}

1;

__END__


