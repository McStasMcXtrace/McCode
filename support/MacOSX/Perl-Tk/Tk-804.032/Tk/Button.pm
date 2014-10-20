package Tk::Button;
# Conversion from Tk4.0 button.tcl competed.
#
# Copyright (c) 1992-1994 The Regents of the University of California.
# Copyright (c) 1994 Sun Microsystems, Inc.
# Copyright (c) 1995-2003 Nick Ing-Simmons. All rights reserved.
# This program is free software; you can redistribute it and/or

use vars qw($VERSION);
$VERSION = '4.010'; # $Id: //depot/Tkutf8/Tk/Button.pm#8 $

# modify it under the same terms as Perl itself, subject
# to additional disclaimer in license.terms due to partial
# derivation from Tk4.0 sources.

use strict;

require Tk::Widget;
use base  qw(Tk::Widget);

use vars qw($buttonWindow $afterId $repeated);

Tk::Methods('deselect','flash','invoke','select','toggle');

sub Tk_cmd { \&Tk::button }

Construct Tk::Widget 'Button';

sub ClassInit
{
 my ($class,$mw) = @_;
 $mw->bind($class,'<Enter>', 'Enter');
 $mw->bind($class,'<Leave>', 'Leave');
 $mw->bind($class,'<1>', 'butDown');
 $mw->bind($class,'<ButtonRelease-1>', 'butUp');
 $mw->bind($class,'<space>', 'Invoke');
 $mw->bind($class,'<Return>', 'Invoke');
 return $class;
}

# tkButtonEnter --
# The procedure below is invoked when the mouse pointer enters a
# button widget.  It records the button we're in and changes the
# state of the button to active unless the button is disabled.
#
# Arguments:
# w -		The name of the widget.

sub Enter
{
 my $w = shift;
 my $E = shift;
 if ($w->cget('-state') ne 'disabled')
  {
   # On unix the state is active just with mouse-over
   $w->configure(-state => 'active');

   # If the mouse button is down, set the relief to sunken on entry.
   # Overwise, if there's an -overrelief value, set the relief to that.
   $w->{__relief__} = $w->cget('-relief');
   if (defined $buttonWindow && $w == $buttonWindow)
    {
     $w->configure(-relief => 'sunken');
     $w->{__prelief__} = 'sunken';
    }
   elsif ((my $over = $w->cget('-overrelief')) ne '')
    {
     $w->configure(-relief => $over);
     $w->{__prelief__} = $over;
    }
  }
 $Tk::window = $w;
}

# tkButtonLeave --
# The procedure below is invoked when the mouse pointer leaves a
# button widget.  It changes the state of the button back to
# inactive.  If we're leaving the button window with a mouse button
# pressed (tkPriv(buttonWindow) == $w), restore the relief of the
# button too.
#
# Arguments:
# w -		The name of the widget.
sub Leave
{
 my $w = shift;
 $w->configure('-state'=>'normal') if ($w->cget('-state') ne 'disabled');
 # Restore the original button relief if it was changed by Tk.
 # That is signaled by the existence of Priv($w,prelief).
 if (exists $w->{__relief__})
  {
   if (exists $w->{__prelief__} &&
       $w->{__prelief__} eq $w->cget('-relief'))
    {
     $w->configure(-relief => $w->{__relief__});
    }
   delete $w->{__relief__};
   delete $w->{__prelief__};
  }
 undef $Tk::window;
}

# tkButtonDown --
# The procedure below is invoked when the mouse button is pressed in
# a button widget.  It records the fact that the mouse is in the button,
# saves the button's relief so it can be restored later, and changes
# the relief to sunken.
#
# Arguments:
# w -		The name of the widget.
sub butDown
{
 my $w = shift;

 # Only save the button's relief if it does not yet exist.  If there
 # is an overrelief setting, Priv($w,relief) will already have been set,
 # and the current value of the -relief option will be incorrect.

 if (!exists $w->{__relief__})
  {
   $w->{__relief__} = $w->cget('-relief');
  }

 if ($w->cget('-state') ne 'disabled')
  {
   $buttonWindow = $w;
   $w->configure('-relief' => 'sunken', '-state' => 'active');
   $w->{__prelief__} = 'sunken';

   # If this button has a repeatdelay set up, get it going with an after
   $w->afterCancel($afterId);
   my $delay = $w->cget('-repeatdelay');
   $repeated = 0;
   if ($delay > 0)
    {
     $afterId = $w->after($delay, [$w, 'AutoInvoke']);
    }
  }
}

# tkButtonUp --
# The procedure below is invoked when the mouse button is released
# in a button widget.  It restores the button's relief and invokes
# the command as long as the mouse hasn't left the button.
#
# Arguments:
# w -		The name of the widget.
sub butUp
{
 my $w = shift;
 if (defined($buttonWindow) && $buttonWindow == $w)
  {
   undef $buttonWindow;

   # Restore the button's relief if it was cached.
   if (exists $w->{__relief__})
    {
     if (exists $w->{__prelief__} &&
	 $w->{__prelief__} eq $w->cget('-relief'))
      {
       $w->configure(-relief => $w->{__relief__});
      }
     delete $w->{__relief__};
     delete $w->{__prelief__};
    }

   # Clean up the after event from the auto-repeater
   $w->afterCancel($afterId);

   if ($w->IS($Tk::window) && $w->cget('-state') ne 'disabled')
    {
     $w->configure(-state => 'normal');
     # Only invoke the command if it wasn't already invoked by the
     # auto-repeater functionality
     if ($repeated == 0)
      {
       $w->invoke;
      }
    }
  }
}

# tkButtonInvoke --
# The procedure below is called when a button is invoked through
# the keyboard.  It simulate a press of the button via the mouse.
#
# Arguments:
# w -		The name of the widget.
sub Invoke
{
 my $w = shift;
 if ($w->cget('-state') ne 'disabled')
  {
   my $oldRelief = $w->cget('-relief');
   my $oldState  = $w->cget('-state');
   $w->configure('-state' => 'active', '-relief' => 'sunken');
   $w->idletasks;
   $w->after(100);
   $w->configure('-state' => $oldState, '-relief' => $oldRelief);
   $w->invoke;
  }
}

# ::tk::ButtonAutoInvoke --
#
#      Invoke an auto-repeating button, and set it up to continue to repeat.
#
# Arguments:
#      w       button to invoke.
#
# Results:
#      None.
#
# Side effects:
#      May create an after event to call ::tk::ButtonAutoInvoke.
sub AutoInvoke
{
 my $w = shift;
 $w->afterCancel($afterId);
 my $delay = $w->cget('-repeatinterval');
 if ($w->IS($Tk::window))
  {
   $repeated++;
   $w->invoke;
  }
 if ($delay > 0)
  {
   $afterId = $w->after($delay, [$w, 'AutoInvoke']);
  }
}

# Used for Tk::Widget::AmpWidget
sub AmpWidgetPostHook
{
 my $w = shift;
 $w->bind('<<AltUnderlined>>' => ['invoke']);
}


1;

__END__





