# Conversion from Tk4.0 scrollbar.tcl competed.
package Tk::Scrollbar;

use vars qw($VERSION);
$VERSION = '4.010'; # $Id: //depot/Tkutf8/Scrollbar/Scrollbar.pm#10 $

use Tk qw($XS_VERSION Ev);
use AutoLoader;

use base  qw(Tk::Widget);

#use strict;
#use vars qw($pressX $pressY @initValues $initPos $activeBg);

Construct Tk::Widget 'Scrollbar';

bootstrap Tk::Scrollbar;

sub Tk_cmd { \&Tk::scrollbar }

Tk::Methods('activate','delta','fraction','get','identify','set');

sub Needed
{
 my ($sb) = @_;
 my @val = $sb->get;
 return 1 unless (@val == 2);
 return 1 if $val[0] != 0.0;
 return 1 if $val[1] != 1.0;
 return 0;
}


sub ClassInit
{
 my ($class,$mw) = @_;
 $mw->bind($class, '<Enter>', 'Enter');
 $mw->bind($class, '<Motion>', 'Motion');
 $mw->bind($class, '<Leave>', 'Leave');

 $mw->bind($class, '<1>', 'ButtonDown');
 $mw->bind($class, '<B1-Motion>', ['Drag', Ev('x'), Ev('y')]);
 $mw->bind($class, '<ButtonRelease-1>', 'ButtonUp');
 $mw->bind($class, '<B1-Leave>', 'NoOp'); # prevent generic <Leave>
 $mw->bind($class, '<B1-Enter>', 'NoOp'); # prevent generic <Enter>
 $mw->bind($class, '<Control-1>', 'ScrlTopBottom');

 $mw->bind($class, '<2>', 'ButtonDown');
 $mw->bind($class, '<B2-Motion>', ['Drag', Ev('x'), Ev('y')]);
 $mw->bind($class, '<ButtonRelease-2>', 'ButtonUp');
 $mw->bind($class, '<B2-Leave>', 'NoOp'); # prevent generic <Leave>
 $mw->bind($class, '<B2-Enter>', 'NoOp'); # prevent generic <Enter>
 $mw->bind($class, '<Control-2>', 'ScrlTopBottom');

 $mw->bind($class, '<Up>',            ['ScrlByUnits','v',-1]);
 $mw->bind($class, '<Down>',          ['ScrlByUnits','v', 1]);
 $mw->bind($class, '<Control-Up>',    ['ScrlByPages','v',-1]);
 $mw->bind($class, '<Control-Down>',  ['ScrlByPages','v', 1]);

 $mw->bind($class, '<Left>',          ['ScrlByUnits','h',-1]);
 $mw->bind($class, '<Right>',         ['ScrlByUnits','h', 1]);
 $mw->bind($class, '<Control-Left>',  ['ScrlByPages','h',-1]);
 $mw->bind($class, '<Control-Right>', ['ScrlByPages','h', 1]);

 $mw->bind($class, '<Prior>',         ['ScrlByPages','hv',-1]);
 $mw->bind($class, '<Next>',          ['ScrlByPages','hv', 1]);

 # X11 mousewheel - honour for horizontal too.
 $mw->bind($class, '<4>',             ['ScrlByUnits','hv',-5]);
 $mw->bind($class, '<5>',             ['ScrlByUnits','hv', 5]);

 $mw->bind($class, '<Home>',          ['ScrlToPos', 0]);
 $mw->bind($class, '<End>',           ['ScrlToPos', 1]);

 $mw->bind($class, '<4>',             ['ScrlByUnits','v',-3]);
 $mw->bind($class, '<5>',             ['ScrlByUnits','v', 3]);

 return $class;

}

1;

__END__

sub Enter
{
 my $w = shift;
 my $e = $w->XEvent;
 if ($Tk::strictMotif)
  {
   my $bg = $w->cget('-background');
   $activeBg = $w->cget('-activebackground');
   $w->configure('-activebackground' => $bg);
  }
 $w->activate($w->identify($e->x,$e->y));
}

sub Leave
{
 my $w = shift;
 if ($Tk::strictMotif)
  {
   $w->configure('-activebackground' => $activeBg) if (defined $activeBg) ;
  }
 $w->activate('');
}

sub Motion
{
 my $w = shift;
 my $e = $w->XEvent;
 $w->activate($w->identify($e->x,$e->y));
}

# tkScrollButtonDown --
# This procedure is invoked when a button is pressed in a scrollbar.
# It changes the way the scrollbar is displayed and takes actions
# depending on where the mouse is.
#
# Arguments:
# w -		The scrollbar widget.
# x, y -	Mouse coordinates.

sub ButtonDown
{my $w = shift;
 my $e = $w->XEvent;
 my $element = $w->identify($e->x,$e->y);
 $w->configure('-activerelief' => 'sunken');
 if ($e->b == 1 and
     (defined($element) && $element eq 'slider'))
  {
   $w->StartDrag($e->x,$e->y);
  }
 elsif ($e->b == 2 and
	(defined($element) && $element =~ /^(trough[12]|slider)$/o))
  {
	my $pos = $w->fraction($e->x, $e->y);
	my($head, $tail) = $w->get;
	my $len = $tail - $head;

	$head = $pos - $len/2;
	$tail = $pos + $len/2;
	if ($head < 0) {
		$head = 0;
		$tail = $len;
	}
	elsif ($tail > 1) {
		$head = 1 - $len;
		$tail = 1;
	}
	$w->ScrlToPos($head);
	$w->set($head, $tail);

	$w->StartDrag($e->x,$e->y);
   }
 else
  {
   $w->Select($element,'initial');
  }
}

# tkScrollButtonUp --
# This procedure is invoked when a button is released in a scrollbar.
# It cancels scans and auto-repeats that were in progress, and restores
# the way the active element is displayed.
#
# Arguments:
# w -		The scrollbar widget.
# x, y -	Mouse coordinates.

sub ButtonUp
{my $w = shift;
 my $e = $w->XEvent;
 $w->CancelRepeat;
 $w->configure('-activerelief' => 'raised');
 $w->EndDrag($e->x,$e->y);
 $w->activate($w->identify($e->x,$e->y));
}

# tkScrollSelect --
# This procedure is invoked when button 1 is pressed over the scrollbar.
# It invokes one of several scrolling actions depending on where in
# the scrollbar the button was pressed.
#
# Arguments:
# w -		The scrollbar widget.
# element -	The element of the scrollbar that was selected, such
#		as "arrow1" or "trough2".  Shouldn't be "slider".
# repeat -	Whether and how to auto-repeat the action:  "noRepeat"
#		means don't auto-repeat, "initial" means this is the
#		first action in an auto-repeat sequence, and "again"
#		means this is the second repetition or later.

sub Select
{
 my $w = shift;
 my $element = shift;
 my $repeat  = shift;
 return unless defined ($element);
 if ($element eq 'arrow1')
  {
   $w->ScrlByUnits('hv',-1);
  }
 elsif ($element eq 'trough1')
  {
   $w->ScrlByPages('hv',-1);
  }
 elsif ($element eq 'trough2')
  {
   $w->ScrlByPages('hv', 1);
  }
 elsif ($element eq 'arrow2')
  {
   $w->ScrlByUnits('hv', 1);
  }
 else
  {
   return;
  }

 if ($repeat eq 'again')
  {
   $w->RepeatId($w->after($w->cget('-repeatinterval'),['Select',$w,$element,'again']));
  }
 elsif ($repeat eq 'initial')
  {
   $w->RepeatId($w->after($w->cget('-repeatdelay'),['Select',$w,$element,'again']));
  }
}

# tkScrollStartDrag --
# This procedure is called to initiate a drag of the slider.  It just
# remembers the starting position of the slider.
#
# Arguments:
# w -		The scrollbar widget.
# x, y -	The mouse position at the start of the drag operation.

sub StartDrag
{
 my($w,$x,$y) = @_;
 return unless (defined ($w->cget('-command')));
 $pressX = $x;
 $pressY = $y;
 @initValues = $w->get;
 my $iv0 = $initValues[0];
 if (@initValues == 2)
  {
   $initPos = $iv0;
  }
 elsif ($iv0 == 0)
  {
   $initPos = 0;
  }
 else
  {
   $initPos = $initValues[2]/$initValues[0];
  }
}

# tkScrollDrag --
# This procedure is called for each mouse motion even when the slider
# is being dragged.  It notifies the associated widget if we're not
# jump scrolling, and it just updates the scrollbar if we are jump
# scrolling.
#
# Arguments:
# w -		The scrollbar widget.
# x, y -	The current mouse position.

sub Drag
{
 my($w,$x,$y) = @_;
 return if !defined $initPos;
 my $delta = $w->delta($x-$pressX, $y-$pressY);
 if ($w->cget('-jump'))
  {
   if (@initValues == 2)
    {
     $w->set($initValues[0]+$delta, $initValues[1]+$delta);
    }
   else
    {
     $delta = sprintf "%d", $delta * $initValues[0]; # round()
     $initValues[2] += $delta;
     $initValues[3] += $delta;
     $w->set(@initValues[2,3]);
    }
  }
 else
  {
   $w->ScrlToPos($initPos+$delta);
  }
}

# tkScrollEndDrag --
# This procedure is called to end an interactive drag of the slider.
# It scrolls the window if we're in jump mode, otherwise it does nothing.
#
# Arguments:
# w -		The scrollbar widget.
# x, y -	The mouse position at the end of the drag operation.

sub EndDrag
{
 my($w,$x,$y) = @_;
 return if (!defined $initPos);
 if ($w->cget('-jump'))
  {
   my $delta = $w->delta($x-$pressX, $y-$pressY);
   $w->ScrlToPos($initPos+$delta);
  }
 undef $initPos;
}

# tkScrlByUnits --
# This procedure tells the scrollbar's associated widget to scroll up
# or down by a given number of units.  It notifies the associated widget
# in different ways for old and new command syntaxes.
#
# Arguments:
# w -		The scrollbar widget.
# orient -	Which kinds of scrollbars this applies to:  "h" for
#		horizontal, "v" for vertical, "hv" for both.
# amount -	How many units to scroll:  typically 1 or -1.

sub ScrlByUnits
{my $w = shift;
 my $orient = shift;
 my $amount = shift;
 my $cmd    = $w->cget('-command');
 return unless (defined $cmd);
 return if (index($orient,substr($w->cget('-orient'),0,1)) < 0);
 my @info = $w->get;
 if (@info == 2)
  {
   $cmd->Call('scroll',$amount,'units');
  }
 else
  {
   $cmd->Call($info[2]+$amount);
  }
}

# tkScrlByPages --
# This procedure tells the scrollbar's associated widget to scroll up
# or down by a given number of screenfuls.  It notifies the associated
# widget in different ways for old and new command syntaxes.
#
# Arguments:
# w -		The scrollbar widget.
# orient -	Which kinds of scrollbars this applies to:  "h" for
#		horizontal, "v" for vertical, "hv" for both.
# amount -	How many screens to scroll:  typically 1 or -1.

sub ScrlByPages
{
 my $w = shift;
 my $orient = shift;
 my $amount = shift;
 my $cmd    = $w->cget('-command');
 return unless (defined $cmd);
 return if (index($orient,substr($w->cget('-orient'),0,1)) < 0);
 my @info = $w->get;
 if (@info == 2)
  {
   $cmd->Call('scroll',$amount,'pages');
  }
 else
  {
   $cmd->Call($info[2]+$amount*($info[1]-1));
  }
}

# tkScrlToPos --
# This procedure tells the scrollbar's associated widget to scroll to
# a particular location, given by a fraction between 0 and 1.  It notifies
# the associated widget in different ways for old and new command syntaxes.
#
# Arguments:
# w -		The scrollbar widget.
# pos -		A fraction between 0 and 1 indicating a desired position
#		in the document.

sub ScrlToPos
{
 my $w = shift;
 my $pos = shift;
 my $cmd = $w->cget('-command');
 return unless (defined $cmd);
 my @info = $w->get;
 if (@info == 2)
  {
   $cmd->Call('moveto',$pos);
  }
 else
  {
   $cmd->Call(int($info[0]*$pos));
  }
}

# tkScrlTopBottom
# Scroll to the top or bottom of the document, depending on the mouse
# position.
#
# Arguments:
# w -		The scrollbar widget.
# x, y -	Mouse coordinates within the widget.

sub ScrlTopBottom
{
 my $w = shift;
 my $e = $w->XEvent;
 my $element = $w->identify($e->x,$e->y);
 return unless ($element);
 if ($element =~ /1$/)
  {
   $w->ScrlToPos(0);
  }
 elsif ($element =~ /2$/)
  {
   $w->ScrlToPos(1);
  }
}




