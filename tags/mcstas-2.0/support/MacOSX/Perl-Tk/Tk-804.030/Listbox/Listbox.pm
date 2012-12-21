# Converted from listbox.tcl --
#
# This file defines the default bindings for Tk listbox widgets.
#
# @(#) listbox.tcl 1.7 94/12/17 16:05:18
#
# Copyright (c) 1994 The Regents of the University of California.
# Copyright (c) 1994 Sun Microsystems, Inc.
#
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.

# Modifications from standard Listbox.pm
# --------------------------------------
# 27-JAN-2001 Alasdair Allan
#    Modified for local use by adding tied scalar and arrays
#    Implemented TIESCALAR, TIEARRAY, FETCH, FETCHSIZE, STORE, CLEAR & EXTEND
# 31-JAN-2001 Alasdair Allan
#    Made changes suggested by Tim Jenness
# 03-FEB-2001 Alasdair Allan
#    Modified STORE for tied scalars to clear and select elements
# 06-FEB-2001 Alasdair Allan
#    Added POD documentation for tied listbox
# 13-FEB-2001 Alasdair Allan
#    Implemented EXISTS, DELETE, PUSH, POP, SHIFT & UNSHIFT for tied arrays
# 14-FEB-2001 Alasdair Allan
#    Implemented SPLICE for tied arrays, all tied functionality in place
# 16-FEB-2001 Alasdair Allan
#    Tweak to STORE interface for tied scalars
# 23-FEB-2001 Alasdair Allan
#    Added flag to FETCH for tied scalars, modified to return hashes
# 24-FEB-2001 Alasdair Allan
#    Updated Pod documentation
#

package Tk::Listbox;

use vars qw($VERSION @Selection $Prev);
use strict;
$VERSION = '4.015'; # sprintf '4.%03d', q$Revision: #14 $ =~ /\D(\d+)\s*$/;

use Tk qw(Ev $XS_VERSION);
use Tk::Clipboard ();
use AutoLoader;

use base  qw(Tk::Clipboard Tk::Widget);

Construct Tk::Widget 'Listbox';

bootstrap Tk::Listbox;

sub Tk_cmd { \&Tk::listbox }

Tk::Methods('activate','bbox','curselection','delete','get','index',
            'insert','itemcget','itemconfigure','nearest','scan','see',
            'selection','size','xview','yview');

use Tk::Submethods ( 'selection' => [qw(anchor clear includes set)],
		     'scan'      => [qw(mark dragto)],
		     'xview'     => [qw(moveto scroll)],
		     'yview'     => [qw(moveto scroll)],
		     );

*Getselected = \&getSelected;

sub clipEvents
{
 return qw[Copy];
}

sub BalloonInfo
{
 my ($listbox,$balloon,$X,$Y,@opt) = @_;
 my $e = $listbox->XEvent;
 return if !$e;
 my $index = $listbox->index('@' . $e->x . ',' . $e->y);
 foreach my $opt (@opt)
  {
   my $info = $balloon->GetOption($opt,$listbox);
   if ($opt =~ /^-(statusmsg|balloonmsg)$/ && UNIVERSAL::isa($info,'ARRAY'))
    {
     $balloon->Subclient($index);
     if (defined $info->[$index])
      {
       return $info->[$index];
      }
     return '';
    }
   return $info;
  }
}

sub ClassInit
{
 my ($class,$mw) = @_;
 $class->SUPER::ClassInit($mw);
 # Standard Motif bindings:
 $mw->bind($class,'<1>',[sub {
			   my $w = shift;
			   if (Tk::Exists($w)) {
			     $w->BeginSelect(@_);
			   }
			 }, Ev('index',Ev('@'))]);
 $mw->bind($class, '<Double-1>' => \&Tk::NoOp);
 $mw->bind($class,'<B1-Motion>',['Motion',Ev('index',Ev('@'))]);
 $mw->bind($class,'<ButtonRelease-1>','ButtonRelease_1');
 ;
 $mw->bind($class,'<Shift-1>',['BeginExtend',Ev('index',Ev('@'))]);
 $mw->bind($class,'<Control-1>',['BeginToggle',Ev('index',Ev('@'))]);

 $mw->bind($class,'<B1-Leave>',['AutoScan',Ev('x'),Ev('y')]);
 $mw->bind($class,'<B1-Enter>','CancelRepeat');
 $mw->bind($class,'<Up>',['UpDown',-1]);
 $mw->bind($class,'<Shift-Up>',['ExtendUpDown',-1]);
 $mw->bind($class,'<Down>',['UpDown',1]);
 $mw->bind($class,'<Shift-Down>',['ExtendUpDown',1]);

 $mw->XscrollBind($class);
 $mw->bind($class,'<Prior>', sub {
	       my $w = shift;
	       $w->yview('scroll',-1,'pages');
	       $w->activate('@0,0');
	   });
 $mw->bind($class,'<Next>',  sub {
	       my $w = shift;
	       $w->yview('scroll',1,'pages');
	       $w->activate('@0,0');
	   });
 $mw->bind($class,'<Control-Prior>', ['xview', 'scroll', -1, 'pages']);
 $mw->bind($class,'<Control-Next>',  ['xview', 'scroll',  1, 'pages']);
 # <Home> and <End> defined in XscrollBind
 $mw->bind($class,'<Control-Home>','Cntrl_Home');
 ;
 $mw->bind($class,'<Shift-Control-Home>',['DataExtend',0]);
 $mw->bind($class,'<Control-End>','Cntrl_End');
 ;
 $mw->bind($class,'<Shift-Control-End>',['DataExtend','end']);
 # XXX What about <<Copy>>? Already handled in Tk::Clipboard?
 # $class->clipboardOperations($mw,'Copy');
 $mw->bind($class,'<space>',['BeginSelect',Ev('index','active')]);
 $mw->bind($class,'<Select>',['BeginSelect',Ev('index','active')]);
 $mw->bind($class,'<Control-Shift-space>',['BeginExtend',Ev('index','active')]);
 $mw->bind($class,'<Shift-Select>',['BeginExtend',Ev('index','active')]);
 $mw->bind($class,'<Escape>','Cancel');
 $mw->bind($class,'<Control-slash>','SelectAll');
 $mw->bind($class,'<Control-backslash>','Cntrl_backslash');
 ;
 # Additional Tk bindings that aren't part of the Motif look and feel:
 $mw->bind($class,'<2>',['scan','mark',Ev('x'),Ev('y')]);
 $mw->bind($class,'<B2-Motion>',['scan','dragto',Ev('x'),Ev('y')]);

 $mw->MouseWheelBind($class); # XXX Both needed?
 $mw->YMouseWheelBind($class);
 return $class;
}

1;
__END__

sub TIEARRAY {
  my ( $class, $obj, %options ) = @_;
  return bless {
	    OBJECT => \$obj,
	    OPTION => \%options }, $class;
}



sub TIESCALAR {
  my ( $class, $obj, %options ) = @_;
  return bless {
	    OBJECT => \$obj,
	    OPTION => \%options }, $class;
}

# FETCH
# -----
# Return either the full contents or only the selected items in the
# box depending on whether we tied it to an array or scalar respectively
sub FETCH {
  my $class = shift;

  my $self = ${$class->{OBJECT}};
  my %options = %{$class->{OPTION}} if defined $class->{OPTION};;

  # Define the return variable
  my $result;

  # Check whether we are have a tied array or scalar quantity
  if ( @_ ) {
     my $i = shift;
     # The Tk:: Listbox has been tied to an array, we are returning
     # an array list of the current items in the Listbox
     $result = $self->get($i);
  } else {
     # The Tk::Listbox has been tied to a scalar, we are returning a
     # reference to an array or hash containing the currently selected items
     my ( @array, %hash );

     if ( defined $options{ReturnType} ) {

        # THREE-WAY SWITCH
        if ( $options{ReturnType} eq "index" ) {
           $result = [$self->curselection];
        } elsif ( $options{ReturnType} eq "element" ) {
	   foreach my $selection ( $self->curselection ) {
              push(@array,$self->get($selection)); }
           $result = \@array;
	} elsif ( $options{ReturnType} eq "both" ) {
	   foreach my $selection ( $self->curselection ) {
              %hash = ( %hash, $selection => $self->get($selection)); }
           $result = \%hash;
	}
     } else {
        # return elements (default)
        foreach my $selection ( $self->curselection ) {
           push(@array,$self->get($selection)); }
        $result = \@array;
     }
  }
  return $result;
}

# FETCHSIZE
# ---------
# Return the number of elements in the Listbox when tied to an array
sub FETCHSIZE {
  my $class = shift;
  return ${$class->{OBJECT}}->size();
}

# STORE
# -----
# If tied to an array we will modify the Listbox contents, while if tied
# to a scalar we will select and clear elements.
sub STORE {

  if ( scalar(@_) == 2 ) {
     # we have a tied scalar
     my ( $class, $selected ) = @_;
     my $self = ${$class->{OBJECT}};
     my %options = %{$class->{OPTION}} if defined $class->{OPTION};;

     # clear currently selected elements
     $self->selectionClear(0,'end');

     # set selected elements
     if ( defined $options{ReturnType} ) {

        # THREE-WAY SWITCH
        if ( $options{ReturnType} eq "index" ) {
           for ( my $i=0; $i < scalar(@$selected) ; $i++ ) {
              for ( my $j=0; $j < $self->size() ; $j++ ) {
                  if( $j == $$selected[$i] ) {
	             $self->selectionSet($j); last; }
              }
           }
        } elsif ( $options{ReturnType} eq "element" ) {
           for ( my $k=0; $k < scalar(@$selected) ; $k++ ) {
              for ( my $l=0; $l < $self->size() ; $l++ ) {
                 if( $self->get($l) eq $$selected[$k] ) {
	            $self->selectionSet($l); last; }
              }
           }
	} elsif ( $options{ReturnType} eq "both" ) {
           foreach my $key ( keys %$selected ) {
              $self->selectionSet($key)
	              if $$selected{$key} eq $self->get($key);
	   }
	}
     } else {
        # return elements (default)
        for ( my $k=0; $k < scalar(@$selected) ; $k++ ) {
           for ( my $l=0; $l < $self->size() ; $l++ ) {
              if( $self->get($l) eq $$selected[$k] ) {
	         $self->selectionSet($l); last; }
           }
        }
     }

  } else {
     # we have a tied array
     my ( $class, $index, $value ) = @_;
     my $self = ${$class->{OBJECT}};

     # check size of current contents list
     my $sizeof = $self->size();

     if ( $index <= $sizeof ) {
        # Change a current listbox entry
        $self->delete($index);
        $self->insert($index, $value);
     } else {
        # Add a new value
        if ( defined $index ) {
           $self->insert($index, $value);
        } else {
           $self->insert("end", $value);
        }
     }
   }
}

# CLEAR
# -----
# Empty the Listbox of contents if tied to an array
sub CLEAR {
  my $class = shift;
  ${$class->{OBJECT}}->delete(0, 'end');
}

# EXTEND
# ------
# Do nothing and be happy about it
sub EXTEND { }

# PUSH
# ----
# Append elements onto the Listbox contents
sub PUSH {
  my ( $class, @list ) = @_;
  ${$class->{OBJECT}}->insert('end', @list);
}

# POP
# ---
# Remove last element of the array and return it
sub POP {
   my $class = shift;

   my $value = ${$class->{OBJECT}}->get('end');
   ${$class->{OBJECT}}->delete('end');
   return $value;
}

# SHIFT
# -----
# Removes the first element and returns it
sub SHIFT {
   my $class = shift;

   my $value = ${$class->{OBJECT}}->get(0);
   ${$class->{OBJECT}}->delete(0);
   return $value
}

# UNSHIFT
# -------
# Insert elements at the beginning of the Listbox
sub UNSHIFT {
   my ( $class, @list ) = @_;
   ${$class->{OBJECT}}->insert(0, @list);
}

# DELETE
# ------
# Delete element at specified index
sub DELETE {
   my ( $class, @list ) = @_;

   my $value = ${$class->{OBJECT}}->get(@list);
   ${$class->{OBJECT}}->delete(@list);
   return $value;
}

# EXISTS
# ------
# Returns true if the index exist, and undef if not
sub EXISTS {
   my ( $class, $index ) = @_;
   return undef unless ${$class->{OBJECT}}->get($index);
}

# SPLICE
# ------
# Performs equivalent of splice on the listbox contents
sub SPLICE {
   my $class = shift;

   my $self = ${$class->{OBJECT}};

   # check for arguments
   my @elements;
   if ( scalar(@_) == 0 ) {
      # none
      @elements = $self->get(0,'end');
      $self->delete(0,'end');
      return wantarray ? @elements : $elements[scalar(@elements)-1];;

   } elsif ( scalar(@_) == 1 ) {
      # $offset
      my ( $offset ) = @_;
      if ( $offset < 0 ) {
         my $start = $self->size() + $offset;
         if ( $start > 0 ) {
	    @elements = $self->get($start,'end');
            $self->delete($start,'end');
	    return wantarray ? @elements : $elements[scalar(@elements)-1];
         } else {
            return undef;
	 }
      } else {
	 @elements = $self->get($offset,'end');
         $self->delete($offset,'end');
         return wantarray ? @elements : $elements[scalar(@elements)-1];
      }

   } elsif ( scalar(@_) == 2 ) {
      # $offset and $length
      my ( $offset, $length ) = @_;
      if ( $offset < 0 ) {
         my $start = $self->size() + $offset;
         my $end = $self->size() + $offset + $length - 1;
	 if ( $start > 0 ) {
	    @elements = $self->get($start,$end);
            $self->delete($start,$end);
	    return wantarray ? @elements : $elements[scalar(@elements)-1];
         } else {
            return undef;
	 }
      } else {
	 @elements = $self->get($offset,$offset+$length-1);
         $self->delete($offset,$offset+$length-1);
         return wantarray ? @elements : $elements[scalar(@elements)-1];
      }

   } else {
      # $offset, $length and @list
      my ( $offset, $length, @list ) = @_;
      if ( $offset < 0 ) {
         my $start = $self->size() + $offset;
         my $end = $self->size() + $offset + $length - 1;
	 if ( $start > 0 ) {
	    @elements = $self->get($start,$end);
            $self->delete($start,$end);
	    $self->insert($start,@list);
	    return wantarray ? @elements : $elements[scalar(@elements)-1];
         } else {
            return undef;
	 }
      } else {
	 @elements = $self->get($offset,$offset+$length-1);
         $self->delete($offset,$offset+$length-1);
	 $self->insert($offset,@list);
         return wantarray ? @elements : $elements[scalar(@elements)-1];
      }
   }
}

# ----

#
# Bind --
# This procedure is invoked the first time the mouse enters a listbox
# widget or a listbox widget receives the input focus. It creates
# all of the class bindings for listboxes.
#
# Arguments:
# event - Indicates which event caused the procedure to be invoked
# (Enter or FocusIn). It is used so that we can carry out
# the functions of that event in addition to setting up
# bindings.

sub xyIndex
{
 my $w = shift;
 my $Ev = $w->XEvent;
 return $w->index($Ev->xy);
}

sub ButtonRelease_1
{
 my $w = shift;
 my $Ev = $w->XEvent;
 $w->CancelRepeat;
 $w->activate($Ev->xy);
}


sub Cntrl_Home
{
 my $w = shift;
 my $Ev = $w->XEvent;
 $w->activate(0);
 $w->see(0);
 $w->selectionClear(0,'end');
 $w->selectionSet(0);
 $w->eventGenerate("<<ListboxSelect>>");
}


sub Cntrl_End
{
 my $w = shift;
 my $Ev = $w->XEvent;
 $w->activate('end');
 $w->see('end');
 $w->selectionClear(0,'end');
 $w->selectionSet('end');
 $w->eventGenerate("<<ListboxSelect>>");
}


sub Cntrl_backslash
{
 my $w = shift;
 my $Ev = $w->XEvent;
 if ($w->cget('-selectmode') ne 'browse')
 {
  $w->selectionClear(0,'end');
  $w->eventGenerate("<<ListboxSelect>>");
 }
}

# BeginSelect --
#
# This procedure is typically invoked on button-1 presses. It begins
# the process of making a selection in the listbox. Its exact behavior
# depends on the selection mode currently in effect for the listbox;
# see the Motif documentation for details.
#
# Arguments:
# w - The listbox widget.
# el - The element for the selection operation (typically the
# one under the pointer). Must be in numerical form.
sub BeginSelect
{
 my $w = shift;
 my $el = shift;
 if ($w->cget('-selectmode') eq 'multiple')
  {
   if ($w->selectionIncludes($el))
    {
     $w->selectionClear($el)
    }
   else
    {
     $w->selectionSet($el)
    }
  }
 else
  {
   $w->selectionClear(0,'end');
   $w->selectionSet($el);
   $w->selectionAnchor($el);
   @Selection = ();
   $Prev = $el
  }
 $w->focus if ($w->cget('-takefocus'));
 $w->eventGenerate("<<ListboxSelect>>");
}
# Motion --
#
# This procedure is called to process mouse motion events while
# button 1 is down. It may move or extend the selection, depending
# on the listbox's selection mode.
#
# Arguments:
# w - The listbox widget.
# el - The element under the pointer (must be a number).
sub Motion
{
 my $w = shift;
 my $el = shift;
 if (defined($Prev) && $el == $Prev)
  {
   return;
  }
 my $anchor = $w->index('anchor');
 my $mode = $w->cget('-selectmode');
 if ($mode eq 'browse')
  {
   $w->selectionClear(0,'end');
   $w->selectionSet($el);
   $Prev = $el;
   $w->eventGenerate("<<ListboxSelect>>");
  }
 elsif ($mode eq 'extended')
  {
   my $i = $Prev;
   if (!defined $i || $i eq '')
    {
     $i = $el;
     $w->selectionSet($el);
    }
   if ($w->selectionIncludes('anchor'))
    {
     $w->selectionClear($i,$el);
     $w->selectionSet('anchor',$el)
    }
   else
    {
     $w->selectionClear($i,$el);
     $w->selectionClear('anchor',$el)
    }
   if (!@Selection)
    {
     @Selection = $w->curselection;
    }
   while ($i < $el && $i < $anchor)
    {
     if (Tk::lsearch(\@Selection,$i) >= 0)
      {
       $w->selectionSet($i)
      }
     $i++
    }
   while ($i > $el && $i > $anchor)
    {
     if (Tk::lsearch(\@Selection,$i) >= 0)
      {
       $w->selectionSet($i)
      }
     $i--
    }
   $Prev = $el;
   $w->eventGenerate("<<ListboxSelect>>");
  }
}
# BeginExtend --
#
# This procedure is typically invoked on shift-button-1 presses. It
# begins the process of extending a selection in the listbox. Its
# exact behavior depends on the selection mode currently in effect
# for the listbox; see the Motif documentation for details.
#
# Arguments:
# w - The listbox widget.
# el - The element for the selection operation (typically the
# one under the pointer). Must be in numerical form.
sub BeginExtend
{
 my $w = shift;
 my $el = shift;
 if ($w->cget('-selectmode') eq 'extended' && $w->selectionIncludes('anchor'))
  {
   $w->Motion($el)
  }
 else
  {
   # No selection yet; simulate the begin-select operation.
   $w->BeginSelect($el);
  }
}
# BeginToggle --
#
# This procedure is typically invoked on control-button-1 presses. It
# begins the process of toggling a selection in the listbox. Its
# exact behavior depends on the selection mode currently in effect
# for the listbox; see the Motif documentation for details.
#
# Arguments:
# w - The listbox widget.
# el - The element for the selection operation (typically the
# one under the pointer). Must be in numerical form.
sub BeginToggle
{
 my $w = shift;
 my $el = shift;
 if ($w->cget('-selectmode') eq 'extended')
  {
   @Selection = $w->curselection();
   $Prev = $el;
   $w->selectionAnchor($el);
   if ($w->selectionIncludes($el))
    {
     $w->selectionClear($el)
    }
   else
    {
     $w->selectionSet($el)
    }
   $w->eventGenerate("<<ListboxSelect>>");
  }
}
# AutoScan --
# This procedure is invoked when the mouse leaves an entry window
# with button 1 down. It scrolls the window up, down, left, or
# right, depending on where the mouse left the window, and reschedules
# itself as an "after" command so that the window continues to scroll until
# the mouse moves back into the window or the mouse button is released.
#
# Arguments:
# w - The entry window.
# x - The x-coordinate of the mouse when it left the window.
# y - The y-coordinate of the mouse when it left the window.
sub AutoScan
{
 my $w = shift;
 return if !Tk::Exists($w);
 my $x = shift;
 my $y = shift;
 if ($y >= $w->height)
  {
   $w->yview('scroll',1,'units')
  }
 elsif ($y < 0)
  {
   $w->yview('scroll',-1,'units')
  }
 elsif ($x >= $w->width)
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
 $w->Motion($w->index("@" . $x . ',' . $y));
 $w->RepeatId($w->after(50,'AutoScan',$w,$x,$y));
}
# UpDown --
#
# Moves the location cursor (active element) up or down by one element,
# and changes the selection if we're in browse or extended selection
# mode.
#
# Arguments:
# w - The listbox widget.
# amount - +1 to move down one item, -1 to move back one item.
sub UpDown
{
 my $w = shift;
 my $amount = shift;
 $w->activate($w->index('active')+$amount);
 $w->see('active');
 my $mode = $w->cget('-selectmode');
 if ($mode eq 'browse')
  {
   $w->selectionClear(0,'end');
   $w->selectionSet('active');
   $w->eventGenerate("<<ListboxSelect>>");
  }
 elsif ($mode eq 'extended')
  {
   $w->selectionClear(0,'end');
   $w->selectionSet('active');
   $w->selectionAnchor('active');
   $Prev = $w->index('active');
   @Selection = ();
   $w->eventGenerate("<<ListboxSelect>>");
  }
}
# ExtendUpDown --
#
# Does nothing unless we're in extended selection mode; in this
# case it moves the location cursor (active element) up or down by
# one element, and extends the selection to that point.
#
# Arguments:
# w - The listbox widget.
# amount - +1 to move down one item, -1 to move back one item.
sub ExtendUpDown
{
 my $w = shift;
 my $amount = shift;
 if ($w->cget('-selectmode') ne 'extended')
  {
   return;
  }
 my $active = $w->index('active');
 if (!@Selection)
  {
   $w->selectionSet($active);
   @Selection = $w->curselection;
  }
 $w->activate($active + $amount);
 $w->see('active');
 $w->Motion($w->index('active'))
}
# DataExtend
#
# This procedure is called for key-presses such as Shift-KEndData.
# If the selection mode isn't multiple or extend then it does nothing.
# Otherwise it moves the active element to el and, if we're in
# extended mode, extends the selection to that point.
#
# Arguments:
# w - The listbox widget.
# el - An integer element number.
sub DataExtend
{
 my $w = shift;
 my $el = shift;
 my $mode = $w->cget('-selectmode');
 if ($mode eq 'extended')
  {
   $w->activate($el);
   $w->see($el);
   if ($w->selectionIncludes('anchor'))
    {
     $w->Motion($el)
    }
  }
 elsif ($mode eq 'multiple')
  {
   $w->activate($el);
   $w->see($el)
  }
}
# Cancel
#
# This procedure is invoked to cancel an extended selection in
# progress. If there is an extended selection in progress, it
# restores all of the items between the active one and the anchor
# to their previous selection state.
#
# Arguments:
# w - The listbox widget.
sub Cancel
{
 my $w = shift;
 if ($w->cget('-selectmode') ne 'extended' || !defined $Prev)
  {
   return;
  }
 my $first = $w->index('anchor');
 my $last = $Prev;
 if ($first > $last)
  {
   ($first, $last) = ($last, $first);
  }
 $w->selectionClear($first,$last);
 while ($first <= $last)
  {
   if (Tk::lsearch(\@Selection,$first) >= 0)
    {
     $w->selectionSet($first)
    }
   $first++
  }
 $w->eventGenerate("<<ListboxSelect>>");
}
# SelectAll
#
# This procedure is invoked to handle the "select all" operation.
# For single and browse mode, it just selects the active element.
# Otherwise it selects everything in the widget.
#
# Arguments:
# w - The listbox widget.
sub SelectAll
{
 my $w = shift;
 my $mode = $w->cget('-selectmode');
 if ($mode eq 'single' || $mode eq 'browse')
  {
   $w->selectionClear(0,'end');
   $w->selectionSet('active')
  }
 else
  {
   $w->selectionSet(0,'end')
  }
 $w->eventGenerate("<<ListboxSelect>>");
}

# Perl/Tk extensions:
sub SetList
{
 my $w = shift;
 $w->delete(0,'end');
 $w->insert('end',@_);
}

sub deleteSelected
{
 my $w = shift;
 my $i;
 foreach $i (reverse $w->curselection)
  {
   $w->delete($i);
  }
}

sub clipboardPaste
{
 my $w = shift;
 my $index = $w->index('active') || $w->index($w->XEvent->xy);
 my $str;
 eval {local $SIG{__DIE__}; $str = $w->clipboardGet };
 return if $@;
 foreach (split("\n",$str))
  {
   $w->insert($index++,$_);
  }
}

sub getSelected
{
 my ($w) = @_;
 my $i;
 my (@result) = ();
 foreach $i ($w->curselection)
  {
   push(@result,$w->get($i));
  }
 return (wantarray) ? @result : $result[0];
}



1;
__END__


