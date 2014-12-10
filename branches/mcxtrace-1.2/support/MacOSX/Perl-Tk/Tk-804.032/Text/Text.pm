# text.tcl --
#
# This file defines the default bindings for Tk text widgets.
#
# @(#) text.tcl 1.18 94/12/17 16:05:26
#
# Copyright (c) 1992-1994 The Regents of the University of California.
# Copyright (c) 1994 Sun Microsystems, Inc.
# perl/Tk version:
# Copyright (c) 1995-2004 Nick Ing-Simmons
# Copyright (c) 1999 Greg London
#
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.
package Tk::Text;
use AutoLoader;
use Carp;
use strict;

use Text::Tabs;

use vars qw($VERSION);
#$VERSION = sprintf '4.%03d', q$Revision: #24 $ =~ /\D(\d+)\s*$/;
$VERSION = '4.029';

use Tk qw(Ev $XS_VERSION);
use base  qw(Tk::Clipboard Tk::Widget);

Construct Tk::Widget 'Text';

bootstrap Tk::Text;

sub Tk_cmd { \&Tk::text }

sub Tk::Widget::ScrlText { shift->Scrolled('Text' => @_) }

Tk::Methods('bbox','compare','debug','delete','dlineinfo','dump','edit',
            'get','image','index','insert','mark','scan','search',
            'see','tag','window','xview','yview');

use Tk::Submethods ( 'mark'   => [qw(gravity names next previous set unset)],
		     'scan'   => [qw(mark dragto)],
		     'tag'    => [qw(add bind cget configure delete lower
				     names nextrange prevrange raise ranges remove)],
		     'window' => [qw(cget configure create names)],
		     'image'  => [qw(cget configure create names)],
		     'xview'  => [qw(moveto scroll)],
		     'yview'  => [qw(moveto scroll)],
                     'edit'   => [qw(modified redo reset separator undo)],
		     );

sub Tag;
sub Tags;

sub bindRdOnly
{

 my ($class,$mw) = @_;

 # Standard Motif bindings:
 $mw->bind($class,'<Meta-B1-Motion>','NoOp');
 $mw->bind($class,'<Meta-1>','NoOp');
 $mw->bind($class,'<Alt-KeyPress>','NoOp');
 $mw->bind($class,'<Escape>','unselectAll');

 $mw->bind($class,'<1>',['Button1',Ev('x'),Ev('y')]);
 $mw->bind($class,'<B1-Motion>','B1_Motion' ) ;
 $mw->bind($class,'<B1-Leave>','B1_Leave' ) ;
 $mw->bind($class,'<B1-Enter>','CancelRepeat');
 $mw->bind($class,'<ButtonRelease-1>','CancelRepeat');
 $mw->bind($class,'<Control-1>',['markSet','insert',Ev('@')]);

 $mw->bind($class,'<Double-1>','selectWord' ) ;
 $mw->bind($class,'<Triple-1>','selectLine' ) ;
 $mw->bind($class,'<Shift-1>','adjustSelect' ) ;
 $mw->bind($class,'<Double-Shift-1>',['SelectTo',Ev('@'),'word']);
 $mw->bind($class,'<Triple-Shift-1>',['SelectTo',Ev('@'),'line']);

 $mw->bind($class,'<Left>',['SetCursor',Ev('index','insert-1c')]);
 $mw->bind($class,'<Shift-Left>',['KeySelect',Ev('index','insert-1c')]);
 $mw->bind($class,'<Control-Left>',['SetCursor',Ev('index','insert-1c wordstart')]);
 $mw->bind($class,'<Shift-Control-Left>',['KeySelect',Ev('index','insert-1c wordstart')]);

 $mw->bind($class,'<Right>',['SetCursor',Ev('index','insert+1c')]);
 $mw->bind($class,'<Shift-Right>',['KeySelect',Ev('index','insert+1c')]);
 $mw->bind($class,'<Control-Right>',['SetCursor',Ev('index','insert+1c wordend')]);
 $mw->bind($class,'<Shift-Control-Right>',['KeySelect',Ev('index','insert wordend')]);

 $mw->bind($class,'<Up>',['SetCursor',Ev('UpDownLine',-1)]);
 $mw->bind($class,'<Shift-Up>',['KeySelect',Ev('UpDownLine',-1)]);
 $mw->bind($class,'<Control-Up>',['SetCursor',Ev('PrevPara','insert')]);
 $mw->bind($class,'<Shift-Control-Up>',['KeySelect',Ev('PrevPara','insert')]);

 $mw->bind($class,'<Down>',['SetCursor',Ev('UpDownLine',1)]);
 $mw->bind($class,'<Shift-Down>',['KeySelect',Ev('UpDownLine',1)]);
 $mw->bind($class,'<Control-Down>',['SetCursor',Ev('NextPara','insert')]);
 $mw->bind($class,'<Shift-Control-Down>',['KeySelect',Ev('NextPara','insert')]);

 $mw->bind($class,'<Home>',['SetCursor','insert linestart']);
 $mw->bind($class,'<Shift-Home>',['KeySelect','insert linestart']);
 $mw->bind($class,'<Control-Home>',['SetCursor','1.0']);
 $mw->bind($class,'<Control-Shift-Home>',['KeySelect','1.0']);

 $mw->bind($class,'<End>',['SetCursor','insert lineend']);
 $mw->bind($class,'<Shift-End>',['KeySelect','insert lineend']);
 $mw->bind($class,'<Control-End>',['SetCursor','end-1char']);
 $mw->bind($class,'<Control-Shift-End>',['KeySelect','end-1char']);

 $mw->bind($class,'<Prior>',['SetCursor',Ev('ScrollPages',-1)]);
 $mw->bind($class,'<Shift-Prior>',['KeySelect',Ev('ScrollPages',-1)]);
 $mw->bind($class,'<Control-Prior>',['xview','scroll',-1,'page']);

 $mw->bind($class,'<Next>',['SetCursor',Ev('ScrollPages',1)]);
 $mw->bind($class,'<Shift-Next>',['KeySelect',Ev('ScrollPages',1)]);
 $mw->bind($class,'<Control-Next>',['xview','scroll',1,'page']);

 $mw->bind($class,'<Shift-Tab>', 'NoOp'); # Needed only to keep <Tab> binding from triggering; does not have to actually do anything.
 $mw->bind($class,'<Control-Tab>','focusNext');
 $mw->bind($class,'<Control-Shift-Tab>','focusPrev');

 $mw->bind($class,'<Control-space>',['markSet','anchor','insert']);
 $mw->bind($class,'<Select>',['markSet','anchor','insert']);
 $mw->bind($class,'<Control-Shift-space>',['SelectTo','insert','char']);
 $mw->bind($class,'<Shift-Select>',['SelectTo','insert','char']);
 $mw->bind($class,'<Control-slash>','selectAll');
 $mw->bind($class,'<Control-backslash>','unselectAll');

 if (!$Tk::strictMotif)
  {
   $mw->bind($class,'<Control-a>',    ['SetCursor','insert linestart']);
   $mw->bind($class,'<Control-b>',    ['SetCursor','insert-1c']);
   $mw->bind($class,'<Control-e>',    ['SetCursor','insert lineend']);
   $mw->bind($class,'<Control-f>',    ['SetCursor','insert+1c']);
   $mw->bind($class,'<Meta-b>',       ['SetCursor','insert-1c wordstart']);
   $mw->bind($class,'<Meta-f>',       ['SetCursor','insert wordend']);
   $mw->bind($class,'<Meta-less>',    ['SetCursor','1.0']);
   $mw->bind($class,'<Meta-greater>', ['SetCursor','end-1c']);

   $mw->bind($class,'<Control-n>',    ['SetCursor',Ev('UpDownLine',1)]);
   $mw->bind($class,'<Control-p>',    ['SetCursor',Ev('UpDownLine',-1)]);

   $mw->bind($class,'<2>',['Button2',Ev('x'),Ev('y')]);
   $mw->bind($class,'<B2-Motion>',['Motion2',Ev('x'),Ev('y')]);
  }
 $mw->bind($class,'<Destroy>','Destroy');
 $mw->bind($class, '<3>', ['PostPopupMenu', Ev('X'), Ev('Y')]  );
 $mw->YMouseWheelBind($class);
 $mw->XMouseWheelBind($class);

 $mw->MouseWheelBind($class);

 return $class;
}

sub selectAll
{
 my ($w) = @_;
 $w->tagAdd('sel','1.0','end');
}

sub unselectAll
{
 my ($w) = @_;
 $w->tagRemove('sel','1.0','end');
}

sub adjustSelect
{
 my ($w) = @_;
 my $Ev = $w->XEvent;
 $w->ResetAnchor($Ev->xy);
 $w->SelectTo($Ev->xy,'char')
}

sub selectLine
{
 my ($w) = @_;
 my $Ev = $w->XEvent;
 $w->SelectTo($Ev->xy,'line');
 Tk::catch { $w->markSet('insert','sel.first') };
}

sub selectWord
{
 my ($w) = @_;
 my $Ev = $w->XEvent;
 $w->SelectTo($Ev->xy,'word');
 Tk::catch { $w->markSet('insert','sel.first') }
}

sub ClassInit
{
 my ($class,$mw) = @_;
 $class->SUPER::ClassInit($mw);

 $class->bindRdOnly($mw);

 $mw->bind($class,'<Tab>', 'insertTab');
 $mw->bind($class,'<Control-i>', ['Insert',"\t"]);
 $mw->bind($class,'<Return>', ['Insert',"\n"]);
 $mw->bind($class,'<Delete>','Delete');
 $mw->bind($class,'<BackSpace>','Backspace');
 $mw->bind($class,'<Insert>', \&ToggleInsertMode ) ;
 $mw->bind($class,'<KeyPress>',['InsertKeypress',Ev('A')]);

 $mw->bind($class,'<F1>', 'clipboardColumnCopy');
 $mw->bind($class,'<F2>', 'clipboardColumnCut');
 $mw->bind($class,'<F3>', 'clipboardColumnPaste');

 # Additional emacs-like bindings:

 if (!$Tk::strictMotif)
  {
   $mw->bind($class,'<Control-d>',['delete','insert']);
   $mw->bind($class,'<Control-k>','deleteToEndofLine') ;
   $mw->bind($class,'<Control-o>','openLine');
   $mw->bind($class,'<Control-t>','Transpose');
   $mw->bind($class,'<Meta-d>',['delete','insert','insert wordend']);
   $mw->bind($class,'<Meta-BackSpace>',['delete','insert-1c wordstart','insert']);

   # A few additional bindings of my own.
   $mw->bind($class,'<Control-h>','deleteBefore');
   $mw->bind($class,'<ButtonRelease-2>','ButtonRelease2');
  }
#JD# $Tk::prevPos = undef;
 return $class;
}

sub insertTab
{
 my ($w) = @_;
 $w->Insert("\t");
 $w->focus;
 $w->break
}

sub deleteToEndofLine
{
 my ($w) = @_;
 if ($w->compare('insert','==','insert lineend'))
  {
   $w->delete('insert')
  }
 else
  {
   $w->delete('insert','insert lineend')
  }
}

sub openLine
{
 my ($w) = @_;
 $w->insert('insert',"\n");
 $w->markSet('insert','insert-1c')
}

sub Button2
{
 my ($w,$x,$y) = @_;
 $w->scan('mark',$x,$y);
 $Tk::x = $x;
 $Tk::y = $y;
 $Tk::mouseMoved = 0;
}

sub Motion2
{
 my ($w,$x,$y) = @_;
 $Tk::mouseMoved = 1 if ($x != $Tk::x || $y != $Tk::y);
 $w->scan('dragto',$x,$y) if ($Tk::mouseMoved);
}

sub ButtonRelease2
{
 my ($w) = @_;
 my $Ev = $w->XEvent;
 if (!$Tk::mouseMoved)
  {
   Tk::catch
    {
     $w->mark('set','insert',$Ev->xy);
     $w->insert($Ev->xy,$w->SelectionGet);
     $w->focus if ($w->cget('-state') eq "normal");
    }
  }
}

sub InsertSelection
{
 my ($w) = @_;
 Tk::catch { $w->Insert($w->SelectionGet) }
}

sub Backspace
{
 my ($w) = @_;
 my $sel = Tk::catch { $w->tag('nextrange','sel','1.0','end') };
 if (defined $sel)
  {
   $w->delete('sel.first','sel.last');
   return;
  }
 $w->deleteBefore;
}

sub deleteBefore
{
 my ($w) = @_;
 if ($w->compare('insert','!=','1.0'))
  {
   $w->delete('insert-1c');
   $w->see('insert')
  }
}

sub Delete
{
 my ($w) = @_;
 my $sel = Tk::catch { $w->tag('nextrange','sel','1.0','end') };
 if (defined $sel)
  {
   $w->delete('sel.first','sel.last')
  }
 else
  {
   $w->delete('insert');
   $w->see('insert')
  }
}

# Button1 --
# This procedure is invoked to handle button-1 presses in text
# widgets. It moves the insertion cursor, sets the selection anchor,
# and claims the input focus.
#
# Arguments:
# w - The text window in which the button was pressed.
# x - The x-coordinate of the button press.
# y - The x-coordinate of the button press.
sub Button1
{
 my ($w,$x,$y) = @_;
 $Tk::selectMode = 'char';
 $Tk::mouseMoved = 0;
 $w->SetCursor('@'.$x.','.$y);
 $w->markSet('anchor','insert');
 $w->focus() if ($w->cget('-state') eq 'normal');
}

sub B1_Motion
{
 my ($w) = @_;
 return unless defined $Tk::mouseMoved;
 my $Ev = $w->XEvent;
 $Tk::x = $Ev->x;
 $Tk::y = $Ev->y;
 $w->SelectTo($Ev->xy)
}

sub B1_Leave
{
 my ($w) = @_;
 my $Ev = $w->XEvent;
 $Tk::x = $Ev->x;
 $Tk::y = $Ev->y;
 $w->AutoScan;
}

# SelectTo --
# This procedure is invoked to extend the selection, typically when
# dragging it with the mouse. Depending on the selection mode (character,
# word, line) it selects in different-sized units. This procedure
# ignores mouse motions initially until the mouse has moved from
# one character to another or until there have been multiple clicks.
#
# Arguments:
# w - The text window in which the button was pressed.
# index - Index of character at which the mouse button was pressed.
sub SelectTo
{
 my ($w, $index, $mode)= @_;
 $Tk::selectMode = $mode if defined ($mode);
 my $cur = $w->index($index);
 my $anchor = Tk::catch { $w->index('anchor') };
 if (!defined $anchor)
  {
   $w->markSet('anchor',$anchor = $cur);
   $Tk::mouseMoved = 0;
  }
 elsif ($w->compare($cur,'!=',$anchor))
  {
   $Tk::mouseMoved = 1;
  }
 $Tk::selectMode = 'char' unless (defined $Tk::selectMode);
 $mode = $Tk::selectMode;
 my ($first,$last);
 if ($mode eq 'char')
  {
   if ($w->compare($cur,'<','anchor'))
    {
     $first = $cur;
     $last = 'anchor';
    }
   else
    {
     $first = 'anchor';
     $last = $cur
    }
  }
 elsif ($mode eq 'word')
  {
   if ($w->compare($cur,'<','anchor'))
    {
     $first = $w->index("$cur wordstart");
     $last = $w->index('anchor - 1c wordend')
    }
   else
    {
     $first = $w->index('anchor wordstart');
     $last = $w->index("$cur wordend")
    }
  }
 elsif ($mode eq 'line')
  {
   if ($w->compare($cur,'<','anchor'))
    {
     $first = $w->index("$cur linestart");
     $last = $w->index('anchor - 1c lineend + 1c')
    }
   else
    {
     $first = $w->index('anchor linestart');
     $last = $w->index("$cur lineend + 1c")
    }
  }
 if ($Tk::mouseMoved || $Tk::selectMode ne 'char')
  {
   $w->tagRemove('sel','1.0',$first);
   $w->tagAdd('sel',$first,$last);
   $w->tagRemove('sel',$last,'end');
   $w->idletasks;
  }
}
# AutoScan --
# This procedure is invoked when the mouse leaves a text window
# with button 1 down. It scrolls the window up, down, left, or right,
# depending on where the mouse is (this information was saved in
# tkPriv(x) and tkPriv(y)), and reschedules itself as an 'after'
# command so that the window continues to scroll until the mouse
# moves back into the window or the mouse button is released.
#
# Arguments:
# w - The text window.
sub AutoScan
{
 my ($w) = @_;
 if ($Tk::y >= $w->height)
  {
   $w->yview('scroll',2,'units')
  }
 elsif ($Tk::y < 0)
  {
   $w->yview('scroll',-2,'units')
  }
 elsif ($Tk::x >= $w->width)
  {
   $w->xview('scroll',2,'units')
  }
 elsif ($Tk::x < 0)
  {
   $w->xview('scroll',-2,'units')
  }
 else
  {
   return;
  }
 $w->SelectTo('@' . $Tk::x . ','. $Tk::y);
 $w->RepeatId($w->after(50,['AutoScan',$w]));
}
# SetCursor
# Move the insertion cursor to a given position in a text. Also
# clears the selection, if there is one in the text, and makes sure
# that the insertion cursor is visible.
#
# Arguments:
# w - The text window.
# pos - The desired new position for the cursor in the window.
sub SetCursor
{
 my ($w,$pos) = @_;
 $pos = 'end - 1 chars' if $w->compare($pos,'==','end');
 $w->markSet('insert',$pos);
 $w->unselectAll;
 $w->see('insert');
}
# KeySelect
# This procedure is invoked when stroking out selections using the
# keyboard. It moves the cursor to a new position, then extends
# the selection to that position.
#
# Arguments:
# w - The text window.
# new - A new position for the insertion cursor (the cursor has not
# actually been moved to this position yet).
sub KeySelect
{
 my ($w,$new) = @_;
 my ($first,$last);
 if (!defined $w->tag('ranges','sel'))
  {
   # No selection yet
   $w->markSet('anchor','insert');
   if ($w->compare($new,'<','insert'))
    {
     $w->tagAdd('sel',$new,'insert')
    }
   else
    {
     $w->tagAdd('sel','insert',$new)
    }
  }
 else
  {
   # Selection exists
   if ($w->compare($new,'<','anchor'))
    {
     $first = $new;
     $last = 'anchor'
    }
   else
    {
     $first = 'anchor';
     $last = $new
    }
   $w->tagRemove('sel','1.0',$first);
   $w->tagAdd('sel',$first,$last);
   $w->tagRemove('sel',$last,'end')
  }
 $w->markSet('insert',$new);
 $w->see('insert');
 $w->idletasks;
}
# ResetAnchor --
# Set the selection anchor to whichever end is farthest from the
# index argument. One special trick: if the selection has two or
# fewer characters, just leave the anchor where it is. In this
# case it does not matter which point gets chosen for the anchor,
# and for the things like Shift-Left and Shift-Right this produces
# better behavior when the cursor moves back and forth across the
# anchor.
#
# Arguments:
# w - The text widget.
# index - Position at which mouse button was pressed, which determines
# which end of selection should be used as anchor point.
sub ResetAnchor
{
 my ($w,$index) = @_;
 if (!defined $w->tag('ranges','sel'))
  {
   $w->markSet('anchor',$index);
   return;
  }
 my $a = $w->index($index);
 my $b = $w->index('sel.first');
 my $c = $w->index('sel.last');
 if ($w->compare($a,'<',$b))
  {
   $w->markSet('anchor','sel.last');
   return;
  }
 if ($w->compare($a,'>',$c))
  {
   $w->markSet('anchor','sel.first');
   return;
  }
 my ($lineA,$chA) = split(/\./,$a);
 my ($lineB,$chB) = split(/\./,$b);
 my ($lineC,$chC) = split(/\./,$c);
 if ($lineB < $lineC+2)
  {
   my $total = length($w->get($b,$c));
   if ($total <= 2)
    {
     return;
    }
   if (length($w->get($b,$a)) < $total/2)
    {
     $w->markSet('anchor','sel.last')
    }
   else
    {
     $w->markSet('anchor','sel.first')
    }
   return;
  }
 if ($lineA-$lineB < $lineC-$lineA)
  {
   $w->markSet('anchor','sel.last')
  }
 else
  {
   $w->markSet('anchor','sel.first')
  }
}

########################################################################
sub markExists
{
 my ($w, $markname)=@_;
 my $mark_exists=0;
 my @markNames_list = $w->markNames;
 foreach my $mark (@markNames_list)
  { if ($markname eq $mark) {$mark_exists=1;last;} }
 return $mark_exists;
}

########################################################################
sub OverstrikeMode
{
 my ($w,$mode) = @_;

 $w->{'OVERSTRIKE_MODE'} =0 unless exists($w->{'OVERSTRIKE_MODE'});

 $w->{'OVERSTRIKE_MODE'}=$mode if (@_ > 1);

 return $w->{'OVERSTRIKE_MODE'};
}

########################################################################
# pressed the <Insert> key, just above 'Del' key.
# this toggles between insert mode and overstrike mode.
sub ToggleInsertMode
{
 my ($w)=@_;
 $w->OverstrikeMode(!$w->OverstrikeMode);
}

########################################################################
sub InsertKeypress
{
 my ($w,$char)=@_;
 return unless length($char);
 if ($w->OverstrikeMode)
  {
   my $current=$w->get('insert');
   $w->delete('insert') unless($current eq "\n");
  }
 $w->Insert($char);
}

########################################################################
sub GotoLineNumber
{
 my ($w,$line_number) = @_;
 $line_number=~ s/^\s+|\s+$//g;
 return if $line_number =~ m/\D/;
 my ($last_line,$junk)  = split(/\./, $w->index('end'));
 if ($line_number > $last_line) {$line_number = $last_line; }
 $w->{'LAST_GOTO_LINE'} = $line_number;
 $w->markSet('insert', $line_number.'.0');
 $w->see('insert');
}

########################################################################
sub GotoLineNumberPopUp
{
 my ($w)=@_;
 my $popup = $w->{'GOTO_LINE_NUMBER_POPUP'};

 unless (defined($w->{'LAST_GOTO_LINE'}))
  {
   my ($line,$col) =  split(/\./, $w->index('insert'));
   $w->{'LAST_GOTO_LINE'} = $line;
  }

 ## if anything is selected when bring up the pop-up, put it in entry window.
 my $selected;
 eval { $selected = $w->SelectionGet(-selection => "PRIMARY"); };
 unless ($@)
  {
   if (defined($selected) and length($selected))
    {
     unless ($selected =~ /\D/)
      {
       $w->{'LAST_GOTO_LINE'} = $selected;
      }
    }
  }
 unless (defined($popup))
  {
   require Tk::DialogBox;
   $popup = $w->DialogBox(-buttons => [qw[Ok Cancel]],-title => "Goto Line Number", -popover => $w,
                          -command => sub { $w->GotoLineNumber($w->{'LAST_GOTO_LINE'}) if $_[0] eq 'Ok'});
   $w->{'GOTO_LINE_NUMBER_POPUP'}=$popup;
   $popup->resizable('no','no');
   my $frame = $popup->Frame->pack(-fill => 'x');
   $frame->Label(-text=>'Enter line number: ')->pack(-side => 'left');
   my $entry = $frame->Entry(-background=>'white', -width=>25,
                             -textvariable => \$w->{'LAST_GOTO_LINE'})->pack(-side =>'left',-fill => 'x');
   $popup->Advertise(entry => $entry);
  }
 $popup->Popup;
 $popup->Subwidget('entry')->focus;
 $popup->Wait;
}

########################################################################

sub getSelected
{
 shift->GetTextTaggedWith('sel');
}

sub deleteSelected
{
 shift->DeleteTextTaggedWith('sel');
}

sub GetTextTaggedWith
{
 my ($w,$tag) = @_;

 my @ranges = $w->tagRanges($tag);
 my $range_total = @ranges;
 my $return_text='';

 # if nothing selected, then ignore
 if ($range_total == 0) {return $return_text;}

 # for every range-pair, get selected text
 while(@ranges)
  {
  my $first = shift(@ranges);
  my $last = shift(@ranges);
  my $text = $w->get($first , $last);
  if(defined($text))
   {$return_text = $return_text . $text;}
  # if there is more tagged text, separate with an end of line  character
  if(@ranges)
   {$return_text = $return_text . "\n";}
  }
 return $return_text;
}

########################################################################
sub DeleteTextTaggedWith
{
 my ($w,$tag) = @_;
 my @ranges = $w->tagRanges($tag);
 my $range_total = @ranges;

 # if nothing tagged with that tag, then ignore
 if ($range_total == 0) {return;}

 # insert marks where selections are located
 # marks will move with text even as text is inserted and deleted
 # in a previous selection.
 for (my $i=0; $i<$range_total; $i++)
  { $w->markSet('mark_tag_'.$i => $ranges[$i]); }

 # for every selected mark pair, insert new text and delete old text
 for (my $i=0; $i<$range_total; $i=$i+2)
  {
  my $first = $w->index('mark_tag_'.$i);
  my $last = $w->index('mark_tag_'.($i+1));

  my $text = $w->delete($first , $last);
  }

 # delete the marks
 for (my $i=0; $i<$range_total; $i++)
  { $w->markUnset('mark_tag_'.$i); }
}


########################################################################
sub FindAll
{
 my ($w,$mode, $case, $pattern ) = @_;
 ### 'sel' tags accumulate, need to remove any previous existing
 $w->unselectAll;

 my $match_length=0;
 my $start_index;
 my $end_index = '1.0';

 while(defined($end_index))
  {
  if ($case eq '-nocase')
   {
   $start_index = $w->search(
    $mode,
    $case,
    -count => \$match_length,
    "--",
    $pattern ,
    $end_index,
    'end');
   }
  else
   {
   $start_index = $w->search(
    $mode,
    -count => \$match_length,
    "--",
    $pattern ,
    $end_index,
    'end');
   }

  unless(defined($start_index) && $start_index) {last;}

  my ($line,$col) = split(/\./, $start_index);
  $col = $col + $match_length;
  $end_index = $line.'.'.$col;
  $w->tagAdd('sel', $start_index, $end_index);
  }
}

########################################################################
# get current selected text and search for the next occurrence
sub FindSelectionNext
{
 my ($w) = @_;
 my $selected;
 eval {$selected = $w->SelectionGet(-selection => "PRIMARY"); };
 return if($@);
 return unless (defined($selected) and length($selected));

 $w->FindNext('-forward', '-exact', '-case', $selected);
}

########################################################################
# get current selected text and search for the previous occurrence
sub FindSelectionPrevious
{
 my ($w) = @_;
 my $selected;
 eval {$selected = $w->SelectionGet(-selection => "PRIMARY"); };
 return if($@);
 return unless (defined($selected) and length($selected));

 $w->FindNext('-backward', '-exact', '-case', $selected);
}



########################################################################
sub FindNext
{
 my ($w,$direction, $mode, $case, $pattern ) = @_;

 ## if searching forward, start search at end of selected block
 ## if backward, start search from start of selected block.
 ## dont want search to find currently selected text.
 ## tag 'sel' may not be defined, use eval loop to trap error
 my $is_forward = $direction =~ m{^-f} && $direction eq substr("-forwards", 0, length($direction));
 eval {
  if ($is_forward)
   {
   $w->markSet('insert', 'sel.last');
   $w->markSet('current', 'sel.last');
   }
  else
   {
   $w->markSet('insert', 'sel.first');
   $w->markSet('current', 'sel.first');
   }
 };

 my $saved_index=$w->index('insert');

 # remove any previous existing tags
 $w->unselectAll;

 my $match_length=0;
 my $start_index;

 if ($case eq '-nocase')
  {
  $start_index = $w->search(
   $direction,
   $mode,
   $case,
   -count => \$match_length,
   "--",
   $pattern ,
   'insert');
  }
 else
  {
  $start_index = $w->search(
   $direction,
   $mode,
   -count => \$match_length,
   "--",
   $pattern ,
   'insert');
  }

 unless(defined($start_index)) { return 0; }
 if(length($start_index) == 0) { return 0; }

 my ($line,$col) = split(/\./, $start_index);
 $col = $col + $match_length;
 my $end_index = $line.'.'.$col;
 $w->tagAdd('sel', $start_index, $end_index);

 $w->see($start_index);

 if ($is_forward)
  {
  $w->markSet('insert', $end_index);
  $w->markSet('current', $end_index);
  }
 else
  {
  $w->markSet('insert', $start_index);
  $w->markSet('current', $start_index);
  }

 my $compared_index = $w->index('insert');

 my $ret_val;
 if ($compared_index eq $saved_index)
  {$ret_val=0;}
 else
  {$ret_val=1;}
 return $ret_val;
}

########################################################################
sub FindAndReplaceAll
{
 my ($w,$mode, $case, $find, $replace ) = @_;
 $w->markSet('insert', '1.0');
 $w->unselectAll;
 while($w->FindNext('-forward', $mode, $case, $find))
  {
  $w->ReplaceSelectionsWith($replace);
  }
}

########################################################################
sub ReplaceSelectionsWith
{
 my ($w,$new_text ) = @_;

 my @ranges = $w->tagRanges('sel');
 my $range_total = @ranges;

 # if nothing selected, then ignore
 if ($range_total == 0) {return};

 # insert marks where selections are located
 # marks will move with text even as text is inserted and deleted
 # in a previous selection.
 for (my $i=0; $i<$range_total; $i++)
  {$w->markSet('mark_sel_'.$i => $ranges[$i]); }

 # for every selected mark pair, insert new text and delete old text
 my ($first, $last);
 for (my $i=0; $i<$range_total; $i=$i+2)
  {
  $first = $w->index('mark_sel_'.$i);
  $last = $w->index('mark_sel_'.($i+1));

  ##########################################################################
  # eventually, want to be able to get selected text,
  # support regular expression matching, determine replace_text
  # $replace_text = $selected_text=~m/$new_text/  (or whatever would work)
  # will have to pass in mode and case flags.
  # this would allow a regular expression search and replace to be performed
  # example, look for "line (\d+):" and replace with "$1 >" or similar
  ##########################################################################

  $w->insert($last, $new_text);
  $w->delete($first, $last);

  }
 ############################################################
 # set the insert cursor to the end of the last insertion mark
 $w->markSet('insert',$w->index('mark_sel_'.($range_total-1)));

 # delete the marks
 for (my $i=0; $i<$range_total; $i++)
  { $w->markUnset('mark_sel_'.$i); }
}
########################################################################
sub FindAndReplacePopUp
{
 my ($w)=@_;
 $w->findandreplacepopup(0);
}

########################################################################
sub FindPopUp
{
 my ($w)=@_;
 $w->findandreplacepopup(1);
}

########################################################################

sub findandreplacepopup
{
 my ($w,$find_only)=@_;

 my $pop = $w->Toplevel;
 $pop->transient($w->toplevel);
 if ($find_only)
  { $pop->title("Find"); }
 else
  { $pop->title("Find and/or Replace"); }
 my $frame =  $pop->Frame->pack(-anchor=>'nw');

 $frame->Label(-text=>"Direction:")
  ->grid(-row=> 1, -column=>1, -padx=> 20, -sticky => 'nw');
 my $direction = '-forward';
 $frame->Radiobutton(
  -variable => \$direction,
  -text => 'forward',-value => '-forward' )
  ->grid(-row=> 2, -column=>1, -padx=> 20, -sticky => 'nw');
 $frame->Radiobutton(
  -variable => \$direction,
  -text => 'backward',-value => '-backward' )
  ->grid(-row=> 3, -column=>1, -padx=> 20, -sticky => 'nw');

 $frame->Label(-text=>"Mode:")
  ->grid(-row=> 1, -column=>2, -padx=> 20, -sticky => 'nw');
 my $mode = '-exact';
 $frame->Radiobutton(
  -variable => \$mode, -text => 'exact',-value => '-exact' )
  ->grid(-row=> 2, -column=>2, -padx=> 20, -sticky => 'nw');
 $frame->Radiobutton(
  -variable => \$mode, -text => 'regexp',-value => '-regexp' )
  ->grid(-row=> 3, -column=>2, -padx=> 20, -sticky => 'nw');

 $frame->Label(-text=>"Case:")
  ->grid(-row=> 1, -column=>3, -padx=> 20, -sticky => 'nw');
 my $case = '-case';
 $frame->Radiobutton(
  -variable => \$case, -text => 'case',-value => '-case' )
  ->grid(-row=> 2, -column=>3, -padx=> 20, -sticky => 'nw');
 $frame->Radiobutton(
  -variable => \$case, -text => 'nocase',-value => '-nocase' )
  ->grid(-row=> 3, -column=>3, -padx=> 20, -sticky => 'nw');

 ######################################################
 my $find_entry = $pop->Entry(-width=>25);
 $find_entry->focus;

 my $donext = sub {$w->FindNext ($direction,$mode,$case,$find_entry->get())};

 $find_entry -> pack(-anchor=>'nw', '-expand' => 'yes' , -fill => 'x'); # autosizing

 ######  if any $w text is selected, put it in the find entry
 ######  could be more than one text block selected, get first selection
 my @ranges = $w->tagRanges('sel');
 if (@ranges)
  {
  my $first = shift(@ranges);
  my $last = shift(@ranges);

  # limit to one line
  my ($first_line, $first_col) = split(/\./,$first);
  my ($last_line, $last_col) = split(/\./,$last);
  unless($first_line == $last_line)
   {$last = $first. ' lineend';}

  $find_entry->insert('insert', $w->get($first , $last));
  }
 else
  {
  my $selected;
  eval {$selected=$w->SelectionGet(-selection => "PRIMARY"); };
  if($@) {}
  elsif (defined($selected))
   {$find_entry->insert('insert', $selected);}
  }

 $find_entry->icursor(0);

 my ($replace_entry,$button_replace,$button_replace_all);
 unless ($find_only)
  {
   $replace_entry = $pop->Entry(-width=>25);

  $replace_entry -> pack(-anchor=>'nw', '-expand' => 'yes' , -fill => 'x');
  }


 my $button_find = $pop->Button(-text=>'Find', -command => $donext, -default => 'active')
  -> pack(-side => 'left');

 my $button_find_all = $pop->Button(-text=>'Find All',
  -command => sub {$w->FindAll($mode,$case,$find_entry->get());} )
  ->pack(-side => 'left');

 unless ($find_only)
  {
   $button_replace = $pop->Button(-text=>'Replace', -default => 'normal',
   -command => sub {$w->ReplaceSelectionsWith($replace_entry->get());} )
   -> pack(-side =>'left');
   $button_replace_all = $pop->Button(-text=>'Replace All',
   -command => sub {$w->FindAndReplaceAll
    ($mode,$case,$find_entry->get(),$replace_entry->get());} )
   ->pack(-side => 'left');
  }


  my $button_cancel = $pop->Button(-text=>'Cancel',
  -command => sub {$pop->destroy()} )
  ->pack(-side => 'left');

  $find_entry->bind("<Return>" => [$button_find, 'invoke']);
  $find_entry->bind("<Escape>" => [$button_cancel, 'invoke']);

 $find_entry->bind("<Return>" => [$button_find, 'invoke']);
 $find_entry->bind("<Escape>" => [$button_cancel, 'invoke']);

 $pop->resizable('yes','no');
 return $pop;
}

# paste clipboard into current location
sub clipboardPaste
{
 my ($w) = @_;
 local $@;
 Tk::catch { $w->Insert($w->clipboardGet) };
}

########################################################################
# Insert --
# Insert a string into a text at the point of the insertion cursor.
# If there is a selection in the text, and it covers the point of the
# insertion cursor, then delete the selection before inserting.
#
# Arguments:
# w - The text window in which to insert the string
# string - The string to insert (usually just a single character)
sub Insert
{
 my ($w,$string) = @_;
 return unless (defined $string && $string ne '');
 #figure out if cursor is inside a selection
 my @ranges = $w->tagRanges('sel');
 if (@ranges)
  {
   while (@ranges)
    {
     my ($first,$last) = splice(@ranges,0,2);
     if ($w->compare($first,'<=','insert') && $w->compare($last,'>=','insert'))
      {
       $w->ReplaceSelectionsWith($string);
       return;
      }
    }
  }
 # paste it at the current cursor location
 $w->insert('insert',$string);
 $w->see('insert');
}

# UpDownLine --
# Returns the index of the character one *display* line above or below the
# insertion cursor. There are two tricky things here. First,
# we want to maintain the original column across repeated operations,
# even though some lines that will get passed through do not have
# enough characters to cover the original column. Second, do not
# try to scroll past the beginning or end of the text.
#
# This may have some weirdness associated with a proportional font. Ie.
# the insertion cursor will zigzag up or down according to the width of
# the character at destination.
#
# Arguments:
# w - The text window in which the cursor is to move.
# n - The number of lines to move: -1 for up one line,
# +1 for down one line.
sub UpDownLine
{
 my ($w,$n) = @_;
 $w->see('insert');
 my $i = $w->index('insert');

 my ($line,$char) = split(/\./,$i);

 my $testX; #used to check the "new" position
 my $testY; #used to check the "new" position

 (my $bx, my $by, my $bw, my $bh) = $w->bbox($i);
 (my $lx, my $ly, my $lw, my $lh) = $w->dlineinfo($i);

 if ( ($n == -1) and ($by <= $bh) )
  {
   #On first display line.. so scroll up and recalculate..
   $w->yview('scroll', -1, 'units');
   unless (($w->yview)[0]) {
     #first line of entire text - keep same position.
     return $i;
   }
   ($bx, $by, $bw, $bh) = $w->bbox($i);
   ($lx, $ly, $lw, $lh) = $w->dlineinfo($i);
  }
 elsif ( ($n == 1) and
         ($ly + $lh) > ( $w->height - 2*$w->cget(-bd) - 2*$w->cget(-highlightthickness) - $lh + 1) )
  {
   #On last display line.. so scroll down and recalculate..
   $w->yview('scroll', 1, 'units');
   ($bx, $by, $bw, $bh) = $w->bbox($i);
   ($lx, $ly, $lw, $lh) = $w->dlineinfo($i);
  }

 # Calculate the vertical position of the next display line
 my $Yoffset = 0;
 $Yoffset = $by - $ly + 1 if ($n== -1);
 $Yoffset = $ly + $lh + 1 - $by if ($n == 1);
 $Yoffset*=$n;
 $testY = $by + $Yoffset;

 # Save the original 'x' position of the insert cursor if:
 # 1. This is the first time through -- or --
 # 2. The insert cursor position has changed from the previous
 #    time the up or down key was pressed -- or --
 # 3. The cursor has reached the beginning or end of the widget.

 {
  no warnings 'uninitialized';
  if (not defined $w->{'origx'} or ($w->{'lastindex'} != $i) )
   {
    $w->{'origx'} = $bx;
   }
 }

 # Try to keep the same column if possible
 $testX = $w->{'origx'};

 # Get the coordinates of the possible new position
 my $testindex = $w->index('@'.$testX.','.$testY );
 $w->see($testindex);
 my ($nx,$ny,$nw,$nh) = $w->bbox($testindex);

 # Which side of the character should we position the cursor -
 # mainly for a proportional font
 if ($testX > $nx+$nw/2)
  {
   $testX = $nx+$nw+1;
  }

 my $newindex = $w->index('@'.$testX.','.$testY );

 if ( $w->compare($newindex,'==','end - 1 char') and ($ny == $ly ) )
  {
    # Then we are trying to the 'end' of the text from
    # the same display line - don't do that
    return $i;
  }

 $w->{'lastindex'} = $newindex;
 $w->see($newindex);
 return $newindex;
}

# PrevPara --
# Returns the index of the beginning of the paragraph just before a given
# position in the text (the beginning of a paragraph is the first non-blank
# character after a blank line).
#
# Arguments:
# w - The text window in which the cursor is to move.
# pos - Position at which to start search.
sub PrevPara
{
 my ($w,$pos) = @_;
 $pos = $w->index("$pos linestart");
 while (1)
  {
   if ($w->get("$pos - 1 line") eq "\n" && $w->get($pos) ne "\n" || $pos eq '1.0' )
    {
     my $string = $w->get($pos,"$pos lineend");
     if ($string =~ /^(\s)+/)
      {
       my $off = length($1);
       $pos = $w->index("$pos + $off chars")
      }
     if ($w->compare($pos,'!=','insert') || $pos eq '1.0')
      {
       return $pos;
      }
    }
   $pos = $w->index("$pos - 1 line")
  }
}
# NextPara --
# Returns the index of the beginning of the paragraph just after a given
# position in the text (the beginning of a paragraph is the first non-blank
# character after a blank line).
#
# Arguments:
# w - The text window in which the cursor is to move.
# start - Position at which to start search.
sub NextPara
{
 my ($w,$start) = @_;
 my $pos = $w->index("$start linestart + 1 line");
 while ($w->get($pos) ne "\n")
  {
   if ($w->compare($pos,'==','end'))
    {
     return $w->index('end - 1c');
    }
   $pos = $w->index("$pos + 1 line")
  }
 while ($w->get($pos) eq "\n" )
  {
   $pos = $w->index("$pos + 1 line");
   if ($w->compare($pos,'==','end'))
    {
     return $w->index('end - 1c');
    }
  }
 my $string = $w->get($pos,"$pos lineend");
 if ($string =~ /^(\s+)/)
  {
   my $off = length($1);
   return $w->index("$pos + $off chars");
  }
 return $pos;
}
# ScrollPages --
# This is a utility procedure used in bindings for moving up and down
# pages and possibly extending the selection along the way. It scrolls
# the view in the widget by the number of pages, and it returns the
# index of the character that is at the same position in the new view
# as the insertion cursor used to be in the old view.
#
# Arguments:
# w - The text window in which the cursor is to move.
# count - Number of pages forward to scroll; may be negative
# to scroll backwards.
sub ScrollPages
{
 my ($w,$count) = @_;
 my @bbox = $w->bbox('insert');
 $w->yview('scroll',$count,'pages');
 if (!@bbox)
  {
   return $w->index('@' . int($w->height/2) . ',' . 0);
  }
 my $x = int($bbox[0]+$bbox[2]/2);
 my $y = int($bbox[1]+$bbox[3]/2);
 return $w->index('@' . $x . ',' . $y);
}

sub Contents
{
 my $w = shift;
 if (@_)
  {
   $w->delete('1.0','end');
   $w->insert('end',shift) while (@_);
  }
 else
  {
   return $w->get('1.0','end -1c');
  }
}

sub Destroy
{
 my ($w) = @_;
 delete $w->{_Tags_};
}

sub Transpose
{
 my ($w) = @_;
 my $pos = 'insert';
 $pos = $w->index("$pos + 1 char") if ($w->compare($pos,'!=',"$pos lineend"));
 return if ($w->compare("$pos - 1 char",'==','1.0'));
 my $new = $w->get("$pos - 1 char").$w->get("$pos - 2 char");
 $w->delete("$pos - 2 char",$pos);
 $w->insert('insert',$new);
 $w->see('insert');
}

sub Tag
{
 my $w = shift;
 my $name = shift;
 Carp::confess('No args') unless (ref $w and defined $name);
 $w->{_Tags_} = {} unless (exists $w->{_Tags_});
 unless (exists $w->{_Tags_}{$name})
  {
   require Tk::Text::Tag;
   $w->{_Tags_}{$name} = 'Tk::Text::Tag'->new($w,$name);
  }
 $w->{_Tags_}{$name}->configure(@_) if (@_);
 return $w->{_Tags_}{$name};
}

sub Tags
{
 my ($w,$name) = @_;
 my @result = ();
 foreach $name ($w->tagNames(@_))
  {
   push(@result,$w->Tag($name));
  }
 return @result;
}

sub TIEHANDLE
{
 my ($class,$obj) = @_;
 return $obj;
}

sub PRINT
{
 my $w = shift;
 # Find out whether 'end' is displayed at the moment
 # Retrieve the position of the bottom of the window as
 # a fraction of the entire contents of the Text widget
 my $yview = ($w->yview)[1];

 # If $yview is 1.0 this means that 'end' is visible in the window
 my $update = 0;
 $update = 1 if $yview == 1.0;

 # Loop over all input strings
 while (@_)
  {
   $w->insert('end',shift);
  }
  # Move the window to see the end of the text if required
  $w->see('end') if $update;
}

sub PRINTF
{
 my $w = shift;
 $w->PRINT(sprintf(shift,@_));
}

sub WRITE
{
 my ($w, $scalar, $length, $offset) = @_;
 unless (defined $length) { $length = length $scalar }
 unless (defined $offset) { $offset = 0 }
 $w->PRINT(substr($scalar, $offset, $length));
}

sub WhatLineNumberPopUp
{
 my ($w)=@_;
 my ($line,$col) = split(/\./,$w->index('insert'));
 $w->messageBox(-type => 'Ok', -title => "What Line Number",
                -message => "The cursor is on line $line (column is $col)");
}

sub MenuLabels
{
 return qw[~File ~Edit ~Search ~View];
}

sub SearchMenuItems
{
 my ($w) = @_;
 return [
    ['command'=>'~Find',          -command => [$w => 'FindPopUp']],
    ['command'=>'Find ~Next',     -command => [$w => 'FindSelectionNext']],
    ['command'=>'Find ~Previous', -command => [$w => 'FindSelectionPrevious']],
    ['command'=>'~Replace',       -command => [$w => 'FindAndReplacePopUp']]
   ];
}

sub EditMenuItems
{
 my ($w) = @_;
 my @items = ();
 foreach my $op ($w->clipEvents)
  {
   push(@items,['command' => "~$op", -command => [ $w => "clipboard$op"]]);
  }
 push(@items,
    '-',
    ['command'=>'Select All', -command   => [$w => 'selectAll']],
    ['command'=>'Unselect All', -command => [$w => 'unselectAll']],
  );
 return \@items;
}

sub ViewMenuItems
{
 my ($w) = @_;
 my $v;
 tie $v,'Tk::Configure',$w,'-wrap';
 return  [
    ['command'=>'Goto ~Line...', -command => [$w => 'GotoLineNumberPopUp']],
    ['command'=>'~Which Line?',  -command =>  [$w => 'WhatLineNumberPopUp']],
    ['cascade'=> 'Wrap', -tearoff => 0, -menuitems => [
      [radiobutton => 'Word', -variable => \$v, -value => 'word'],
      [radiobutton => 'Character', -variable => \$v, -value => 'char'],
      [radiobutton => 'None', -variable => \$v, -value => 'none'],
    ]],
  ];
}

########################################################################
sub clipboardColumnCopy
{
 my ($w) = @_;
 $w->Column_Copy_or_Cut(0);
}

sub clipboardColumnCut
{
 my ($w) = @_;
 $w->Column_Copy_or_Cut(1);
}

########################################################################
sub Column_Copy_or_Cut
{
 my ($w, $cut) = @_;
 my @ranges = $w->tagRanges('sel');
 my $range_total = @ranges;
 # this only makes sense if there is one selected block
 unless ($range_total==2)
  {
  $w->bell;
  return;
  }

 my $selection_start_index = shift(@ranges);
 my $selection_end_index = shift(@ranges);

 my ($start_line, $start_column) = split(/\./, $selection_start_index);
 my ($end_line,   $end_column)   = split(/\./, $selection_end_index);

 # correct indices for tabs
 my $string;
 $string = $w->get($start_line.'.0', $start_line.'.0 lineend');
 $string = substr($string, 0, $start_column);
 $string = expand($string);
 my $tab_start_column = length($string);

 $string = $w->get($end_line.'.0', $end_line.'.0 lineend');
 $string = substr($string, 0, $end_column);
 $string = expand($string);
 my $tab_end_column = length($string);

 my $length = $tab_end_column - $tab_start_column;

 $selection_start_index = $start_line . '.' . $tab_start_column;
 $selection_end_index   = $end_line   . '.' . $tab_end_column;

 # clear the clipboard
 $w->clipboardClear;
 my ($clipstring, $startstring, $endstring);
 my $padded_string = ' 'x$tab_end_column;
 for(my $line = $start_line; $line <= $end_line; $line++)
  {
  $string = $w->get($line.'.0', $line.'.0 lineend');
  $string = expand($string) . $padded_string;
  $clipstring = substr($string, $tab_start_column, $length);
  #$clipstring = unexpand($clipstring);
  $w->clipboardAppend($clipstring."\n");

  if ($cut)
   {
   $startstring = substr($string, 0, $tab_start_column);
   $startstring = unexpand($startstring);
   $start_column = length($startstring);

   $endstring = substr($string, 0, $tab_end_column );
   $endstring = unexpand($endstring);
   $end_column = length($endstring);

   $w->delete($line.'.'.$start_column,  $line.'.'.$end_column);
   }
  }
}

########################################################################

sub clipboardColumnPaste
{
 my ($w) = @_;
 my @ranges = $w->tagRanges('sel');
 my $range_total = @ranges;
 if ($range_total)
  {
  warn " there cannot be any selections during clipboardColumnPaste. \n";
  $w->bell;
  return;
  }

 my $clipboard_text;
 eval
  {
  $clipboard_text = $w->SelectionGet(-selection => "CLIPBOARD");
  };

 return unless (defined($clipboard_text));
 return unless (length($clipboard_text));
 my $string;

 my $current_index = $w->index('insert');
 my ($current_line, $current_column) = split(/\./,$current_index);
 $string = $w->get($current_line.'.0', $current_line.'.'.$current_column);
 $string = expand($string);
 $current_column = length($string);

 my @clipboard_lines = split(/\n/,$clipboard_text);
 my $length;
 my $end_index;
 my ($delete_start_column, $delete_end_column, $insert_column_index);
 foreach my $line (@clipboard_lines)
  {
  if ($w->OverstrikeMode)
   {
   #figure out start and end indexes to delete, compensating for tabs.
   $string = $w->get($current_line.'.0', $current_line.'.0 lineend');
   $string = expand($string);
   $string = substr($string, 0, $current_column);
   $string = unexpand($string);
   $delete_start_column = length($string);

   $string = $w->get($current_line.'.0', $current_line.'.0 lineend');
   $string = expand($string);
   $string = substr($string, 0, $current_column + length($line));
   chomp($string);  # dont delete a "\n" on end of line.
   $string = unexpand($string);
   $delete_end_column = length($string);



   $w->delete(
              $current_line.'.'.$delete_start_column ,
              $current_line.'.'.$delete_end_column
             );
   }

  $string = $w->get($current_line.'.0', $current_line.'.0 lineend');
  $string = expand($string);
  $string = substr($string, 0, $current_column);
  $string = unexpand($string);
  $insert_column_index = length($string);

  $w->insert($current_line.'.'.$insert_column_index, unexpand($line));
  $current_line++;
  }

}

# Backward compatibility
sub GetMenu
{
 carp((caller(0))[3]." is deprecated") if $^W;
 shift->menu
}

1;
__END__


