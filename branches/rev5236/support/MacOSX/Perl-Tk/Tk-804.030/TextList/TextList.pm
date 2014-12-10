# Copyright (c) 1999 Greg London. All rights reserved.
# This program is free software; you can redistribute it and/or
# modify it under the same terms as Perl itself.

# code for bindings taken from Listbox.pm

# comments specifying method functionality taken from
# "Perl/Tk Pocket Reference" by Stephen Lidie.

#######################################################################
# this module uses a text module as its base class to create a list box.
# this will allow list box functionality to also have all the functionality
# of the Text widget.
#
# note that most methods use an element number to indicate which
# element in the list to work on.
# the exception to this is the tag and mark methods which
# are dual natured. These methods may accept either the
# normal element number, or they will also take a element.char index,
# which would be useful for applying tags to part of a line in the list.
#
#######################################################################

package Tk::TextList;

use strict;
use vars qw($VERSION);
$VERSION = '4.006'; # $Id: //depot/Tkutf8/TextList/TextList.pm#5 $

use base qw(Tk::Derived Tk::ReindexedROText );

use Tk qw (Ev);

Construct Tk::Widget 'TextList';

#######################################################################
# the following line causes Populate to get called
# @ISA = qw(Tk::Derived ... );
#######################################################################
sub Populate
{
 my ($w,$args)=@_;
 my $option=delete $args->{'-selectmode'};
 $w->SUPER::Populate($args);
 $w->ConfigSpecs( -selectmode  => ['PASSIVE','selectMode','SelectMode','browse'],
		  -takefocus   => ['PASSIVE','takeFocus','TakeFocus',1],
		  -spacing3    => ['SELF', undef, undef, 3],
		  -insertwidth => ['SELF', undef, undef, 0],
		);

}

#######################################################################
#######################################################################
sub ClassInit
{
 my ($class,$mw) = @_;

 # Standard Motif bindings:
 $mw->bind($class,'<1>',['BeginSelect',Ev('index',Ev('@'))]);
 $mw->bind($class,'<B1-Motion>',['Motion',Ev('index',Ev('@'))]);
 $mw->bind($class,'<ButtonRelease-1>','ButtonRelease_1');

 $mw->bind($class,'<Shift-1>',['BeginExtend',Ev('index',Ev('@'))]);
 $mw->bind($class,'<Control-1>',['BeginToggle',Ev('index',Ev('@'))]);

 $mw->bind($class,'<B1-Leave>',['AutoScan',Ev('x'),Ev('y')]);
 $mw->bind($class,'<B1-Enter>','CancelRepeat');
 $mw->bind($class,'<Up>',['UpDown',-1]);
 $mw->bind($class,'<Shift-Up>',['ExtendUpDown',-1]);
 $mw->bind($class,'<Down>',['UpDown',1]);
 $mw->bind($class,'<Shift-Down>',['ExtendUpDown',1]);

 $mw->XscrollBind($class);
 $mw->PriorNextBind($class);

 $mw->bind($class,'<Control-Home>','Cntrl_Home');

 $mw->bind($class,'<Shift-Control-Home>',['DataExtend',0]);
 $mw->bind($class,'<Control-End>','Cntrl_End');

 $mw->bind($class,'<Shift-Control-End>',['DataExtend','end']);
 $class->clipboardOperations($mw,'Copy');
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

 $mw->bind($class,'<FocusIn>' , ['tagConfigure','_ACTIVE_TAG', -underline=>1]);
 $mw->bind($class,'<FocusOut>', ['tagConfigure','_ACTIVE_TAG', -underline=>0]);

 return $class;
}

#######################################################################
# set the active element to index
# "active" is a text "mark" which underlines the marked text.
#######################################################################
sub activate
{
 my($w,$element)=@_;
 $element= $w->index($element).'.0';
 $w->SUPER::tag('remove', '_ACTIVE_TAG', '1.0','end');
 $w->SUPER::tag('add', '_ACTIVE_TAG',
   $element.' linestart', $element.' lineend');
 $w->SUPER::mark('set', 'active', $element);
}


#######################################################################
# bbox returns a list (x,y,width,height) giving an approximate
# bounding box of character given by index
#######################################################################
sub bbox
{
 my($w,$element)=@_;
 $element=$w->index($element).'.0' unless ($element=~/\./);
 return $w->SUPER::bbox($element);
}

#######################################################################
# returns a list of indices of all elements currently selected
#######################################################################
sub curselection
{
 my ($w)=@_;
 my @ranges = $w->SUPER::tag('ranges', 'sel');
 my @selection_list;
 while (@ranges)
  {
   my ($first,$firstcol) = split(/\./,shift(@ranges));
   my ($last,$lastcol) = split(/\./,shift(@ranges));

   #########################################################################
   # if previous selection ended on the same line that this selection starts,
   # then fiddle the numbers so that this line number isnt included twice.
   #########################################################################
   if (defined($selection_list[-1]) and ($first == $selection_list[-1]))
    {
     $first++; # count this selection starting from the next line.
    }

   if ($lastcol==0)
    {
    $last-=1;
    }

   #########################################################################
   # if incrementing $first causes it to be greater than $last,
   # then do nothing,
   # else add (first .. last) to list
   #########################################################################
   unless ($first>$last)
    {
    push(@selection_list, $first .. $last);
    }
  }
 return @selection_list;
}


#######################################################################
# deletes range of elements from element1 to element2
# defaults to element1
#######################################################################
sub delete
{
 my ($w, $element1, $element2)=@_;
 $element1=$w->index($element1);
 $element2=$element1 unless(defined($element2));
 $element2=$w->index($element2);
 $w->SUPER::delete($element1.'.0' , $element2.'.0 lineend');
}

#######################################################################
# deletes range of characters from index1 to index2
# defaults to index1+1c
# index is line.char notation.
#######################################################################
sub deleteChar
{
 my ($w, $index1, $index2)=@_;
 $index1=$w->index($index1);
 $index2=$index1.' +1c' unless(defined($index2));
 $index2=$w->index($index2);
 $w->SUPER::delete($index1, $index2);
}

#######################################################################
# returns as a list contents of elements from $element1 to $element2
# defaults to element1.
#######################################################################
sub get
{
 my ($w, $element1, $element2)=@_;
 $element1=$w->index($element1);
 $element2=$element1 unless(defined($element2));
 $element2=$w->index($element2);
 my @getlist;
 for(my $i=$element1; $i<=$element2; $i++)
  {
  push(@getlist, $w->SUPER::get($i.'.0 linestart', $i.'.0 lineend'));
  }

 return @getlist;
}

#######################################################################
# return text between index1 and index2 which are line.char notation.
# return value is a single string. index2 defaults to index1+1c
# index is line.char notation.
######################################################################
sub getChar
{
 my $w=shift;
 return $w->SUPER::get(@_);
}

#######################################################################
# returns index in number notation
# this method returns an element number, ie the 5th element.
#######################################################################
sub index
{
 my ($w,$element)=@_;
 return undef unless(defined($element));
 $element=0 if $element<0;
 $element .= '.0' unless $element=~/\D/;
 $element = $w->SUPER::index($element);
 my($line,$col)=split(/\./,$element);
 return $line;
}

#######################################################################
# returns index in line.char notation
# this method returns an index specific to a character within an element
#######################################################################
sub indexChar
{
 my $w=shift;
 return $w->SUPER::index(@_);
}


#######################################################################
# inserts specified elements just before element at index
#######################################################################
sub insert
{
 my $w=shift;
 my $element=shift;
 $element=$w->index($element);
 my $item;
 while (@_)
  {
  $item = shift(@_);
  $item .= "\n";
  $w->SUPER::insert($element++.'.0', $item);
  }
}

#######################################################################
# inserts string just before character at index.
# index is line.char notation.
#######################################################################
sub insertChar
{
 my $w=shift;
 $w->SUPER::insert(@_);
}



#######################################################################
# returns index of element nearest to y-coordinate
#
# currently not defined
#######################################################################
#sub nearest
#{
# return undef;
#}

#######################################################################
# Sets the selection anchor to element at index
#######################################################################
sub selectionAnchor
{
 my ($w, $element)=@_;
 $element=$w->index($element);
 $w->SUPER::mark('set', 'anchor', $element.'.0');
}

#######################################################################
#  deselects elements between index1 and index2, inclusive
#######################################################################
sub selectionClear
{
 my ($w, $element1, $element2)=@_;
 $element1=$w->index($element1);
 $element2=$element1 unless(defined($element2));
 $element2=$w->index($element2);
 $w->SUPER::tag('remove', 'sel', $element1.'.0', $element2.'.0 lineend +1c');
}

#######################################################################
# returns 1 if element at index is selected, 0 otherwise.
#######################################################################
sub selectionIncludes
{
 my ($w, $element)=@_;
 $element=$w->index($element);
 my @list = $w->curselection;
 my $line;
 foreach $line (@list)
  {
  if ($line == $element) {return 1;}
  }
 return 0;
}

#######################################################################
# adds all elements between element1 and element2 inclusive to selection
#######################################################################
sub selectionSet
{
 my ($w, $element1, $element2)=@_;
 $element1=$w->index($element1);
 $element2=$element1 unless(defined($element2));
 $element2=$w->index($element2);
 $w->SUPER::tag('add', 'sel', $element1.'.0', $element2.'.0 lineend +1c');
}

#######################################################################
# for ->selection(option,args) calling convention
#######################################################################
sub selection
{
# my ($w,$sub)=(shift,"selection".ucfirst(shift));
# no strict 'refs';
# # can't use $w->$sub, since it might call overridden method-- bleh
# &($sub)($w,@_);
}


#######################################################################
# adjusts the view in window so element at index is completely visible
#######################################################################
sub see
{
 my ($w, $element)=@_;
 $element=$w->index($element);
 $w->SUPER::see($element.'.0');
}

#######################################################################
# returns number of elements in listbox
#######################################################################
sub size
{
 my ($w)=@_;
 my $element = $w->index('end');
 # theres a weird thing with the 'end' mark sometimes being on a line
 # with text, and sometimes being on a line all by itself
 my ($text) = $w->get($element);
 if (length($text) == 0)
  {$element -= 1;}
 return $element;
}



#######################################################################
# add a tag based on element numbers
#######################################################################
sub tagAdd
{
 my ($w, $tagName, $element1, $element2)=@_;
 $element1=$w->index($element1);
 $element1.='.0';

 $element2=$element1.' lineend' unless(defined($element2));
 $element2=$w->index($element2);
 $element2.='.0 lineend +1c';

 $w->SUPER::tag('add', $tagName, $element1, $element2);
}

#######################################################################
# add a tag based on line.char indexes
#######################################################################
sub tagAddChar
{
 my $w=shift;
 $w->SUPER::tag('add',@_);
}


#######################################################################
# remove a tag based on element numbers
#######################################################################
sub tagRemove
{
 my ($w, $tagName, $element1, $element2)=@_;
 $element1=$w->index($element1);
 $element1.='.0';

 $element2=$element1.' lineend' unless(defined($element2));
 $element2=$w->index($element2);
 $element2.='.0 lineend +1c';

 $w->SUPER::tag('remove', 'sel', $element1, $element2);
}

#######################################################################
# remove a tag based on line.char indexes
#######################################################################
sub tagRemoveChar
{
 my $w=shift;
 $w->SUPER::tag('remove', @_);
}




#######################################################################
# perform tagNextRange based on element numbers
#######################################################################
sub tagNextRange
{
 my ($w, $tagName, $element1, $element2)=@_;
 $element1=$w->index($element1);
 $element1.='.0';

 $element2=$element1 unless(defined($element2));
 $element2=$w->index($element2);
 $element2.='.0 lineend +1c';

 my $index = $w->SUPER::tag('nextrange', 'sel', $element1, $element2);
 my ($line,$col)=split(/\./,$index);
 return $line;
}

#######################################################################
# perform tagNextRange based on line.char indexes
#######################################################################
sub tagNextRangeChar
{
 my $w=shift;
 $w->SUPER::tag('nextrange', @_);
}

#######################################################################
# perform tagPrevRange based on element numbers
#######################################################################
sub tagPrevRange
{
 my ($w, $tagName, $element1, $element2)=@_;
 $element1=$w->index($element1);
 $element1.='.0';

 $element2=$element1 unless(defined($element2));
 $element2=$w->index($element2);
 $element2.='.0 lineend +1c';

 my $index = $w->SUPER::tag('prevrange', 'sel', $element1, $element2);
 my ($line,$col)=split(/\./,$index);
 return $line;
}

#######################################################################
# perform tagPrevRange based on line.char indexes
#######################################################################
sub tagPrevRangeChar
{
 my $w=shift;
 $w->SUPER::tag('prevrange', @_);
}



#######################################################################
# perform markSet based on element numbers
#######################################################################
sub markSet
{
 my ($w,$mark,$element1)=@_;
 $element1=$w->index($element1);
 $element1.='.0';
 $w->SUPER::mark('set', $element1,$mark);
}

#######################################################################
# perform markSet based on line.char indexes
#######################################################################
sub markSetChar
{
 my $w=shift;
 $w->SUPER::mark('set', @_);
}

#######################################################################
# perform markNext based on element numbers
#######################################################################
sub markNext
{
 my ($w,$element1)=@_;
 $element1=$w->index($element1);
 $element1.='.0';
 return $w->SUPER::mark('next', $element1);
}

#######################################################################
# perform markNext based on line.char indexes
#######################################################################
sub markNextChar
{
 my $w=shift;
 $w->SUPER::mark('next', @_);
}


#######################################################################
# perform markPrevious based on element numbers
#######################################################################
sub markPrevious
{
 my ($w,$element1)=@_;
 $element1=$w->index($element1);
 $element1.='.0';
 return $w->SUPER::mark('previous', $element1);
}

#######################################################################
# perform markPrevious based on line.char indexes
#######################################################################
sub markPreviousChar
{
 my $w=shift;
 $w->SUPER::mark('previous', @_);
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
 $w->selectionSet(0)
}


sub Cntrl_End
{
 my $w = shift;
 my $Ev = $w->XEvent;
 $w->activate('end');
 $w->see('end');
 $w->selectionClear(0,'end');
 $w->selectionSet('end')
}


sub Cntrl_backslash
{
 my $w = shift;
 my $Ev = $w->XEvent;
 if ($w->cget('-selectmode') ne 'browse')
 {
 $w->selectionClear(0,'end');
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
   my @list = ();
   $w->{'SELECTION_LIST_REF'} = \@list;
   $w->{'PREVIOUS_ELEMENT'} = $el
  }
 $w->focus if ($w->cget('-takefocus'));
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
 if (defined($w->{'PREVIOUS_ELEMENT'}) && $el == $w->{'PREVIOUS_ELEMENT'})
  {
   return;
  }

 # if no selections, select current
 if($w->curselection==0)
  {
  $w->activate($el);
  $w->selectionSet($el);
  $w->selectionAnchor($el);
  $w->{'PREVIOUS_ELEMENT'}=$el;
  return;
  }

 my $anchor = $w->index('anchor');
 my $mode = $w->cget('-selectmode');
 if ($mode eq 'browse')
  {
   $w->selectionClear(0,'end');
   $w->selectionSet($el);
   $w->{'PREVIOUS_ELEMENT'} = $el;
  }
 elsif ($mode eq 'extended')
  {
   my $i = $w->{'PREVIOUS_ELEMENT'};
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
   while ($i < $el && $i < $anchor)
    {
     if (Tk::lsearch($w->{'SELECTION_LIST_REF'},$i) >= 0)
      {
       $w->selectionSet($i)
      }
     $i += 1
    }
   while ($i > $el && $i > $anchor)
    {
     if (Tk::lsearch($w->{'SELECTION_LIST_REF'},$i) >= 0)
      {
       $w->selectionSet($i)
      }
     $i += -1
    }
   $w->{'PREVIOUS_ELEMENT'} = $el
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

 # if no selections, select current
 if($w->curselection==0)
  {
  $w->activate($el);
  $w->selectionSet($el);
  $w->selectionAnchor($el);
  $w->{'PREVIOUS_ELEMENT'}=$el;
  return;
  }

 if ($w->cget('-selectmode') eq 'extended' && $w->selectionIncludes('anchor'))
  {
   $w->Motion($el)
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
   my @list = $w->curselection();
   $w->{'SELECTION_LIST_REF'} = \@list;
   $w->{'PREVIOUS_ELEMENT'} = $el;
   $w->selectionAnchor($el);
   if ($w->selectionIncludes($el))
    {
     $w->selectionClear($el)
    }
   else
    {
     $w->selectionSet($el)
    }
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
 my $selectmode = $w->cget('-selectmode');
 if ($selectmode eq 'browse')
  {
   $w->selectionClear(0,'end');
   $w->selectionSet('active')
  }
 elsif ($selectmode eq 'extended')
  {
   $w->selectionClear(0,'end');
   $w->selectionSet('active');
   $w->selectionAnchor('active');
   $w->{'PREVIOUS_ELEMENT'} = $w->index('active');
   my @list = ();
   $w->{'SELECTION_LIST_REF'}=\@list;
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
 $w->activate($w->index('active')+$amount);
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
 if ($w->cget('-selectmode') ne 'extended' || !defined $w->{'PREVIOUS_ELEMENT'})
  {
   return;
  }
 my $first = $w->index('anchor');
 my $last = $w->{'PREVIOUS_ELEMENT'};
 if ($first > $last)
  {
  ($first,$last)=($last,$first);
  }
 $w->selectionClear($first,$last);
 while ($first <= $last)
  {
   if (Tk::lsearch($w->{'SELECTION_LIST_REF'},$first) >= 0)
    {
     $w->selectionSet($first)
    }
   $first += 1
  }
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
}

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
 my $element = $w->index('active') || $w->index($w->XEvent->xy);
 my $str;
 eval {local $SIG{__DIE__}; $str = $w->clipboardGet };
 return if $@;
 foreach (split("\n",$str))
  {
   $w->insert($element++,$_);
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
