# Copyright (c) 1995-2004 Nick Ing-Simmons.
# Copyright (c) 1999 Greg London.
# All rights reserved.
# This program is free software; you can redistribute it and/or
# modify it under the same terms as Perl itself.
package Tk::TextUndo;

use vars qw($VERSION $DoDebug);
$VERSION = '4.015'; # $Id: //depot/Tkutf8/Tk/TextUndo.pm#15 $
$DoDebug = 0;

use Tk qw (Ev);
use AutoLoader;

use Tk::Text ();
use base qw(Tk::Text);

Construct Tk::Widget 'TextUndo';

sub ClassInit
{
 my ($class,$mw) = @_;
 $mw->bind($class,'<<Undo>>','undo');
 $mw->bind($class,'<<Redo>>','redo');

 return $class->SUPER::ClassInit($mw);
}


####################################################################
# methods for manipulating the undo and redo stacks.
# no one should directly access the stacks except for these methods.
# everyone else must access the stacks through these methods.
####################################################################
sub ResetUndo
{
 my ($w) = @_;
 delete $w->{UNDO};
 delete $w->{REDO};
}

sub PushUndo
{
 my $w = shift;
 $w->{UNDO} = [] unless (exists $w->{UNDO});
 push(@{$w->{UNDO}},@_);
}

sub PushRedo
{
 my $w = shift;
 $w->{REDO} = [] unless (exists $w->{REDO});
 push(@{$w->{REDO}},@_);
}

sub PopUndo
{
 my ($w) = @_;
 return pop(@{$w->{UNDO}}) if defined $w->{UNDO};
 return undef;
}

sub PopRedo
{
 my ($w) = @_;
 return pop(@{$w->{REDO}}) if defined $w->{REDO};
 return undef;
}

sub ShiftRedo
{
 my ($w) = @_;
 return shift(@{$w->{REDO}}) if defined $w->{REDO};
 return undef;
}

sub numberChanges
{
 my ($w) = @_;
 return 0 unless (exists $w->{'UNDO'}) and (defined($w->{'UNDO'}));
 return scalar(@{$w->{'UNDO'}});
}

sub SizeRedo
{
 my ($w) = @_;
 return 0 unless exists $w->{'REDO'};
 return scalar(@{$w->{'REDO'}});
}

sub getUndoAtIndex
{
 my ($w,$index) = @_;
 return undef unless (exists $w->{UNDO});
 return $w->{UNDO}[$index];
}

sub getRedoAtIndex
{
 my ($w,$index) = @_;
 return undef unless (exists $w->{REDO});
 return $w->{REDO}[$index];
}

####################################################################
# type "hello there"
# hello there_
# hit UNDO
# hello_
# type "out"
# hello out_
# pressing REDO should not do anything
# pressing UNDO should make "out" disappear.
# pressing UNDO should make "there" reappear.
# pressing UNDO should make "there" disappear.
# pressing UNDO should make "hello" disappear.
#
# if there is anything in REDO stack and
# the OperationMode is normal, (i.e. not in the middle of an ->undo or ->redo)
# then before performing the current operation
# take the REDO stack, and put it on UNDO stack
# such that UNDO/REDO keystrokes will still make logical sense.
#
# call this method at the beginning of any overloaded method
# which adds operations to the undo or redo stacks.
# it will perform all the magic needed to handle the redo stack.
####################################################################
sub CheckForRedoShuffle
{
 my ($w) = @_;
 my $size_redo = $w->SizeRedo;
 return unless $size_redo && ($w->OperationMode eq 'normal');
 # local $DoDebug = 1;

 # we are about to 'do' something new, but have something in REDO stack.
 # The REDOs may conflict with new ops, but we want to preserve them.
 # So convert them to UNDOs - effectively do them and their inverses
 # so net effect on the widget is no-change.

 $w->dump_array('StartShuffle');

 $w->OperationMode('REDO_MAGIC');
 $w->MarkSelectionsSavePositions;

 my @pvtundo;

 # go through REDO array from end downto 0, i.e. pseudo pop
 # then pretend we did 'redo' get inverse, and push into UNDO array
 # and 'do' the op.
 for (my $i=$size_redo-1; $i>=0 ; $i--)
  {
   my ($op,@args) = @{$w->getRedoAtIndex($i)};
   my $op_undo = $op .'_UNDO';
   # save the inverse of the op on the UNDO array
   # do this before the re-doing the op - after a 'delete' we cannot see
   # text we deleted!
   my $undo = $w->$op_undo(@args);
   $w->PushUndo($undo);
   # We must 'do' the operation now so if this is an insert
   # the text and tags are available for inspection in delete_UNDO, and
   # indices reflect changes.
   $w->$op(@args);
   # Save the undo that will reverse what we just did - it is
   # on the undo stack but will be tricky to find
   push(@pvtundo,$undo);
  }

 # Now shift each item off REDO array until empty
 # push each item onto UNDO array - this reverses the order
 # and we are not altering buffer so we cannot look in the
 # buffer to compute inverses - which is why we saved them above

 while ($w->SizeRedo)
  {
   my $ref = $w->ShiftRedo;
   $w->PushUndo($ref);
  }

 # Finally undo whatever we did to compensate for doing it
 # and get buffer back to state it was before we started.
 while (@pvtundo)
  {
   my ($op,@args) = @{pop(@pvtundo)};
   $w->$op(@args);
  }

 $w->RestoreSelectionsMarkedSaved;
 $w->OperationMode('normal');
 $w->dump_array('EndShuffle');
}

# sets/returns undo/redo/normal operation mode
sub OperationMode
{
 my ($w,$mode) = @_;
 $w->{'OPERATION_MODE'} = $mode  if (@_ > 1);
 $w->{'OPERATION_MODE'} = 'normal' unless exists($w->{'OPERATION_MODE'});
 return $w->{'OPERATION_MODE'};
}

####################################################################
# dump the undo and redo stacks to the screen.
# used for debug purposes.
sub dump_array
{
 return unless $DoDebug;
 my ($w,$why) = @_;
 print "At $why:\n";
 foreach my $key ('UNDO','REDO')
  {
   if (defined($w->{$key}))
    {
     print " $key array is:\n";
     my $array = $w->{$key};
     foreach my $ref (@$array)
      {
       my @items;
       foreach my $item (@$ref)
        {
         my $loc = $item;
         $loc =~ tr/\n/\^/;
         push(@items,$loc);
        }
       print "  [",join(',',@items),"]\n";
      }
    }
  }
 print "\n";
}


############################################################
############################################################
# these are a group of methods used to indicate the start and end of
# several operations that are to be undo/redo 'ed in a single step.
#
# in other words, "glob" a bunch of operations together.
#
# for example, a search and replace should be undone with a single
# keystroke, rather than one keypress undoes the insert and another
# undoes the delete.
# all other methods should access the count via these methods.
# no other method should directly access the {GLOB_COUNT} value directly
#############################################################
#############################################################

sub AddOperation
{
 my ($w,@operation) = @_;
 my $mode = $w->OperationMode;

 if ($mode eq 'normal')
  {$w->PushUndo([@operation]);}
 elsif ($mode eq 'undo')
  {$w->PushRedo([@operation]);}
 elsif ($mode eq 'redo')
  {$w->PushUndo([@operation]);}
 else
  {die "invalid destination '$mode', must be one of 'normal', 'undo' or 'redo'";}
}

sub addGlobStart	# add it to end of undo list
{
 my ($w, $who) = @_;
 unless (defined($who)) {$who = (caller(1))[3];}
 $w->CheckForRedoShuffle;
 $w->dump_array('Start'.$who);
 $w->AddOperation('GlobStart', $who) ;
}

sub addGlobEnd		# add it to end of undo list
{
 my ($w, $who) = @_;
 unless (defined($who)) {$who = (caller(1))[3];}
 my $topundo = $w->getUndoAtIndex(-1);
 if ($topundo->[0] eq 'GlobStart')
  {
   $w->PopUndo;
  }
 else
  {
   my $nxtundo = $w->getUndoAtIndex(-2);
   if ($nxtundo->[0] eq 'GlobStart')
    {
     $w->PopUndo;
     $w->PopUndo;
     $w->PushUndo($topundo);
    }
   else
    {
     $w->AddOperation('GlobEnd',  $who);
    }
  }
 $w->dump_array('End'.$who);
}

sub GlobStart
{
 my ($w, $who) = @_;
 unless (defined($w->{GLOB_COUNT})) {$w->{GLOB_COUNT}=0;}
 if ($w->OperationMode eq 'normal')
  {
   $w->PushUndo($w->GlobStart_UNDO($who));
  }
 $w->{GLOB_COUNT} = $w->{GLOB_COUNT} + 1;
}

sub GlobStart_UNDO
{
 my ($w, $who) = @_;
 $who = 'GlobEnd_UNDO' unless defined($who);
 return ['GlobEnd',$who];
}

sub GlobEnd
{
 my ($w, $who) = @_;
 unless (defined($w->{GLOB_COUNT})) {$w->{GLOB_COUNT}=0;}
 if ($w->OperationMode eq 'normal')
  {
   $w->PushUndo($w->GlobStart_UNDO($who));
  }
 $w->{GLOB_COUNT} = $w->{GLOB_COUNT} - 1;
}

sub GlobEnd_UNDO
{
 my ($w, $who) = @_;
 $who = 'GlobStart_UNDO' unless defined($who);
 return ['GlobStart',$who];
}

sub GlobCount
{
 my ($w,$count) = @_;
 unless ( exists($w->{'GLOB_COUNT'}) and defined($w->{'GLOB_COUNT'}) )
  {
   $w->{'GLOB_COUNT'}=0;
  }
 if (defined($count))
  {
   $w->{'GLOB_COUNT'}=$count;
  }
 return $w->{'GLOB_COUNT'};
}

####################################################################
# two methods should be used by applications to access undo and redo
# capability, namely, $w->undo; and $w->redo; methods.
# these methods undo and redo the last operation, respectively.
####################################################################
sub undo
{
 my ($w) = @_;
 $w->dump_array('Start'.'undo');
 unless ($w->numberChanges) {$w->bell; return;} # beep and return if empty
 $w->GlobCount(0); #initialize to zero
 $w->OperationMode('undo');
 do
  {
   my ($op,@args) = @{$w->PopUndo};  # get undo operation, convert ref to array
   my $undo_op = $op .'_UNDO';
   $w->PushRedo($w->$undo_op(@args)); # find out how to undo it
   $w->$op(@args);   # do the operation
  } while($w->GlobCount and $w->numberChanges);
 $w->OperationMode('normal');
 $w->dump_array('End'.'undo');
}

sub redo
{
 my ($w) = @_;
 unless ($w->SizeRedo) {$w->bell; return;} # beep and return if empty
 $w->OperationMode('redo');
 $w->GlobCount(0); #initialize to zero
 do
  {
   my ($op,@args) = @{$w->PopRedo}; # get op from redo stack, convert to list
   my $undo_op = $op .'_UNDO';
   $w->PushUndo($w->$undo_op(@args)); # figure out how to undo operation
   $w->$op(@args); # do the operation
  } while($w->GlobCount and $w->SizeRedo);
 $w->OperationMode('normal');
}


############################################################
# override low level subroutines so that they work with UNDO/REDO capability.
# every overridden subroutine must also have a corresponding *_UNDO subroutine.
# the *_UNDO method takes the same parameters in and returns an array reference
# which is how to undo itself.
# note that the *_UNDO must receive absolute indexes.
# ->insert receives 'markname' as the starting index.
# ->insert must convert 'markname' using $absindex=$w->index('markname')
# and pass $absindex to ->insert_UNDO.
############################################################

sub insert
{
 my $w = shift;
 $w->markSet('insert', $w->index(shift) );
 while(@_)
  {
   my $index1 = $w->index('insert');
   my $string = shift;
   my $taglist_ref; $taglist_ref = shift if @_;

   if ($w->OperationMode eq 'normal')
    {
     $w->CheckForRedoShuffle;
     $w->PushUndo($w->insert_UNDO($index1,$string,$taglist_ref));
    }
   $w->markSet('notepos' => $index1);
   $w->SUPER::insert($index1,$string,$taglist_ref);
   $w->markSet('insert', $w->index('notepos'));
  }
}

sub insert_UNDO
{
 my $w = shift;
 my $index = shift;
 my $string = '';
 # This possible call: ->insert (index, string, tag, string, tag...);
 # if more than one string, keep reading strings in (discarding tags)
 # until all strings are read in and $string contains entire text inserted.
 while (@_)
  {
   $string .= shift;
   shift if (@_); # discard tag
  }
 # calculate index
 # possible things to insert:
 # carriage return
 # single character (not CR)
 # single line of characters (not ending in CR)
 # single line of characters ending with a CR
 # multi-line characters. last line does not end with CR
 # multi-line characters, last line does end with CR.
 my ($line,$col) = split(/\./,$index);
 if ($string =~ /\n(.*)$/)
  {
   $line += $string =~ tr/\n/\n/;
   $col  = length($1);
  }
 else
  {
   $col += length($string);
  }
 return ['delete', $index, $line.'.'.$col];
}

sub delete
{
 my ($w, $start, $stop) = @_;
 unless(defined($stop))
  { $stop = $start .'+1c'; }
 my $index1 = $w->index($start);
 my $index2 = $w->index($stop);
 if ($w->OperationMode eq 'normal')
  {
   $w->CheckForRedoShuffle;
   $w->PushUndo($w->delete_UNDO($index1,$index2));
  }
 $w->SUPER::delete($index1,$index2);
 # why call SetCursor - it has side effects
 # which cause a whole slew if save/restore hassles ?
 $w->SetCursor($index1);
}

sub delete_UNDO
{
 my ($w, $index1, $index2) = @_;
 my %tags;
 my @result = ( 'insert' => $index1 );
 my $str   = '';

 ###############################################################
 # get tags in range and return them in a format that
 # can be inserted.
 # $text->insert('1.0', $string1, [tag1,tag2], $string2, [tag2, tag3]);
 # note, have to break tags up into sequential order
 # in reference to _all_ tags.
 ###############################################################

 $w->dump('-text','-tag', -command => sub {
  my ($kind,$value,$posn) = @_;
  if ($kind eq 'text')
   {
    $str .= $value;
   }
  else
   {
    push(@result,$str,[keys %tags]) if (length $str);
    $str = '';
    if ($kind eq 'tagon')
     {
      $tags{$value} = 1;
     }
    elsif ($kind eq 'tagoff')
     {
      delete $tags{$value};
     }
   }
 }, $index1, $index2);
 push(@result,$str,[keys %tags]) if (length $str);
 return \@result;
}

############################################################
# override subroutines which are collections of low level
# routines executed in sequence.
# wrap a globstart and globend around the SUPER:: version of routine.
############################################################

sub ReplaceSelectionsWith
{
 my $w = shift;
 $w->addGlobStart;
 $w->SUPER::ReplaceSelectionsWith(@_);
 $w->addGlobEnd;
}

sub FindAndReplaceAll
{
 my $w = shift;
 $w->addGlobStart;
 $w->SUPER::FindAndReplaceAll(@_);
 $w->addGlobEnd;
}

sub clipboardCut
{
 my $w = shift;
 $w->addGlobStart;
 $w->SUPER::clipboardCut(@_);
 $w->addGlobEnd;
}

sub clipboardPaste
{
 my $w = shift;
 $w->addGlobStart;
 $w->SUPER::clipboardPaste(@_);
 $w->addGlobEnd;
}

sub clipboardColumnCut
{
 my $w = shift;
 $w->addGlobStart;
 $w->SUPER::clipboardColumnCut(@_);
 $w->addGlobEnd;
}

sub clipboardColumnPaste
{
 my $w = shift;
 $w->addGlobStart;
 $w->SUPER::clipboardColumnPaste(@_);
 $w->addGlobEnd;
}

# Greg: this method is more tightly coupled to the base class
# than I would prefer, but I know of no other way to do it.

sub Insert
{
 my ($w,$char)=@_;
 return if $char eq '';
 $w->addGlobStart;
 $w->SUPER::Insert($char);
 $w->addGlobEnd;
 $w->see('insert');
}


sub InsertKeypress
{
 my ($w,$char)=@_;
 return if $char eq '';
 if ($char =~ /^\S$/ and !$w->OverstrikeMode and !$w->tagRanges('sel'))
  {
   my $index = $w->index('insert');
   my $undo_item = $w->getUndoAtIndex(-1);
   if (defined($undo_item) &&
       ($undo_item->[0] eq 'delete') &&
       ($undo_item->[2] == $index)
      )
    {
     $w->SUPER::insert($index,$char);
     $undo_item->[2] = $w->index('insert');
     $w->see('insert');
     return;
    }
  }
 $w->addGlobStart;
 $w->SUPER::InsertKeypress($char);
 $w->addGlobEnd;
}

############################################################
sub TextUndoFileProgress
{
 my ($w,$action,$filename,$count,$val,$total) = @_;
 return unless(defined($filename) and defined($count));

 my $popup = $w->{'FILE_PROGRESS_POP_UP'};
 unless (defined($popup))
  {
   $w->update;
   $popup = $w->Toplevel(-title => "File Progress",-popover => $w);
   $popup->transient($w->toplevel);
   $popup->withdraw;
   $popup->resizable('no','no');
   $popup->Label(-textvariable => \$popup->{ACTION})->pack;
   $popup->Label(-textvariable => \$popup->{FILENAME})->pack;
   $popup->Label(-textvariable => \$popup->{COUNT})->pack;
   my $f = $popup->Frame(-height => 10, -border => 2, -relief => 'sunken')->pack(-fill => 'x');
   my $i = $f->Frame(-background => 'blue', -relief => 'raised', -border => 2);
   $w->{'FILE_PROGRESS_POP_UP'} = $popup;
   $popup->{PROGBAR} = $i;
  }
 $popup->{ACTION}   = $action;
 $popup->{COUNT}    = "lines: $count";
 $popup->{FILENAME} = "Filename: $filename";
 if (defined($val) && defined($total) && $total != 0)
  {
   $popup->{PROGBAR}->place('-x' => 0, '-y' => 0, -relheight => 1, -relwidth => $val/$total);
  }
 else
  {
   $popup->{PROGBAR}->placeForget;
  }

 $popup->idletasks;
 unless ($popup->viewable)
  {
   $w->idletasks;
   $w->toplevel->deiconify unless $w->viewable;
   $popup->Popup;
  }
 $popup->update;
 return $popup;
}

sub FileName
{
 my ($w,$filename) = @_;
 if (@_ > 1)
  {
   $w->{'FILENAME'}=$filename;
  }
 return $w->{'FILENAME'};
}

sub PerlIO_layers
{
 my ($w,$layers) = @_;
 $w->{PERLIO_LAYERS} = $layers if @_ > 1;
 return $w->{PERLIO_LAYERS} || '' ;
}

sub ConfirmDiscard
{
 my ($w)=@_;
 if ($w->numberChanges)
  {
   my $ans = $w->messageBox(-icon    => 'warning',
                            -type => 'YesNoCancel', -default => 'Yes',
                            -message =>
"The text has been modified without being saved.
Save edits?");
   return 0 if $ans eq 'Cancel';
   return 0 if ($ans eq 'Yes' && !$w->Save);
  }
 return 1;
}

################################################################################
# if the file has been modified since being saved, a pop up window will be
# created, asking the user to confirm whether or not to exit.
# this allows the user to return to the application and save the file.
# the code would look something like this:
#
# if ($w->user_wants_to_exit)
#  {$w->ConfirmExit;}
#
# it is also possible to trap attempts to delete the main window.
# this allows the ->ConfirmExit method to be called when the main window
# is attempted to be deleted.
#
# $mw->protocol('WM_DELETE_WINDOW'=>
#  sub{$w->ConfirmExit;});
#
# finally, it might be desirable to trap Control-C signals at the
# application level so that ->ConfirmExit is also called.
#
# $SIG{INT}= sub{$w->ConfirmExit;};
#
################################################################################

sub ConfirmExit
{
 my ($w) = @_;
 $w->toplevel->destroy if $w->ConfirmDiscard;
}

sub Save
{
 my ($w,$filename) = @_;
 $filename = $w->FileName unless defined $filename;
 return $w->FileSaveAsPopup unless defined $filename;
 my $layers = $w->PerlIO_layers;
 if (open(my $file,">$layers",$filename))
  {
   my $status;
   my $count=0;
   my $index = '1.0';
   my $progress;
   my ($lines) = $w->index('end - 1 chars') =~ /^(\d+)\./;
   while ($w->compare($index,'<','end'))
    {
#    my $end = $w->index("$index + 1024 chars");
     my $end = $w->index("$index  lineend +1c");
     print $file $w->get($index,$end);
     $index = $end;
     if (($count++%1000) == 0)
      {
       $progress = $w->TextUndoFileProgress (Saving => $filename,$count,$count,$lines);
      }
    }
   $progress->withdraw if defined $progress;
   if (close($file))
    {
     $w->ResetUndo;
     $w->FileName($filename);
     return 1;
    }
  }
 else
  {
   $w->BackTrace("Cannot open $filename:$!");
  }
 return 0;
}

sub Load
{
 my ($w,$filename) = @_;
 $filename = $w->FileName unless (defined($filename));
 return 0 unless defined $filename;
 my $layers = $w->PerlIO_layers;
 if (open(my $file,"<$layers",$filename))
  {
   $w->MainWindow->Busy;
   $w->EmptyDocument;
   my $count=1;
   my $progress;
   while (<$file>)
    {
     $w->SUPER::insert('end',$_);
     if (($count++%1000) == 0)
      {
       $progress = $w->TextUndoFileProgress (Loading => $filename,
                         $count,tell($file),-s $filename);
      }
    }
   close($file);
   $progress->withdraw if defined $progress;
   $w->markSet('insert' => '1.0');
   $w->FileName($filename);
   $w->MainWindow->Unbusy;
  }
 else
  {
   $w->BackTrace("Cannot open $filename:$!");
  }
}

sub IncludeFile
{
 my ($w,$filename) = @_;
 unless (defined($filename))
  {$w->BackTrace("filename not specified"); return;}
 my $layers = $w->PerlIO_layers;
 if (open(my $file,"<$layers",$filename))
  {
   $w->Busy;
   my $count=1;
   $w->addGlobStart;
   my $progress;
   while (<$file>)
    {
     $w->insert('insert',$_);
     if (($count++%1000) == 0)
      {
       $progress = $w->TextUndoFileProgress(Including => $filename,
                        $count,tell($file),-s $filename);
      }
    }
   $progress->withdraw if defined $progress;
   $w->addGlobEnd;
   close($file);
   $w->Unbusy;
  }
 else
  {
   $w->BackTrace("Cannot open $filename:$!");
  }
}

# clear document without pushing it into UNDO array, (use SUPER::delete)
# (using plain delete(1.0,end) on a really big document fills up the undo array)
# and then clear the Undo and Redo stacks.
sub EmptyDocument
{
 my ($w) = @_;
 $w->SUPER::delete('1.0','end');
 $w->ResetUndo;
 $w->FileName(undef);
}

sub ConfirmEmptyDocument
{
 my ($w)=@_;
 $w->EmptyDocument if $w->ConfirmDiscard;
}

sub FileMenuItems
{
 my ($w) = @_;
 return [
   ["command"=>'~Open',    -command => [$w => 'FileLoadPopup']],
   ["command"=>'~Save',    -command => [$w => 'Save' ]],
   ["command"=>'Save ~As', -command => [$w => 'FileSaveAsPopup']],
   ["command"=>'~Include', -command => [$w => 'IncludeFilePopup']],
   ["command"=>'~Clear',   -command => [$w => 'ConfirmEmptyDocument']],
   "-",@{$w->SUPER::FileMenuItems}
  ]
}

sub EditMenuItems
{
 my ($w) = @_;

 return [
    ["command"=>'Undo', -command => [$w => 'undo']],
    ["command"=>'Redo', -command => [$w => 'redo']],
     "-",@{$w->SUPER::EditMenuItems}
  ];
}

sub CreateFileSelect
{
 my $w = shift;
 my $k = shift;
 my $name = $w->FileName;
 my @types = (['All Files', '*']);
 my $dir   = undef;
 if (defined $name)
  {
   require File::Basename;
   my $sfx;
   ($name,$dir,$sfx) = File::Basename::fileparse($name,'\..*');
   #
   # it should never happen where we have a file suffix and
   # no file name... but fileparse() screws this up with dotfiles.
   #
   if (length($sfx) && !length($name)) { ($name, $sfx) = ($sfx, $name) }

   if (defined($sfx) && length($sfx))
    {
     unshift(@types,['Similar Files',[$sfx]]);
     $name .= $sfx;
    }
  }
 return $w->$k(-initialdir  => $dir, -initialfile => $name,
               -filetypes => \@types, @_);
}

sub FileLoadPopup
{
 my ($w)=@_;
 my $name = $w->CreateFileSelect('getOpenFile',-title => 'File Load');
 return $w->Load($name) if defined($name) and length($name);
 return 0;
}

sub IncludeFilePopup
{
 my ($w)=@_;
 my $name = $w->CreateFileSelect('getOpenFile',-title => 'File Include');
 return $w->IncludeFile($name) if defined($name) and length($name);
 return 0;
}

sub FileSaveAsPopup
{
 my ($w)=@_;
 my $name = $w->CreateFileSelect('getSaveFile',-title => 'File Save As');
 return $w->Save($name) if defined($name) and length($name);
 return 0;
}


sub MarkSelectionsSavePositions
{
 my ($w)=@_;
 $w->markSet('MarkInsertSavePosition','insert');
 my @ranges = $w->tagRanges('sel');
 my $i = 0;
 while (@ranges)
  {
   my ($start,$end) = splice(@ranges,0,2);
   $w->markSet( 'MarkSelectionsSavePositions_'.++$i, $start);
   $w->markSet( 'MarkSelectionsSavePositions_'.++$i, $end);
   $w->tagRemove('sel',$start,$end);
  }
}

sub RestoreSelectionsMarkedSaved
{
 my ($w)=@_;
 my $i = 1;
 my %mark_hash;
 foreach my $mark ($w->markNames)
  {
   $mark_hash{$mark}=1;
  }
 while(1)
  {
   my $markstart = 'MarkSelectionsSavePositions_'.$i++;
   last unless(exists($mark_hash{$markstart}));
   my $indexstart = $w->index($markstart);
   my $markend = 'MarkSelectionsSavePositions_'.$i++;
   last unless(exists($mark_hash{$markend}));
   my $indexend = $w->index($markend);
   $w->tagAdd('sel',$indexstart, $indexend);
   $w->markUnset($markstart, $markend);
  }
 $w->markSet('insert','MarkInsertSavePosition');
}

####################################################################
# selected lines may be discontinous sequence.
sub GetMarkedSelectedLineNumbers
{
 my ($w) = @_;

 my $i = 1;
 my %mark_hash;
 my @ranges;
 foreach my $mark ($w->markNames)
  {
   $mark_hash{$mark}=1;
  }

 while(1)
  {
   my $markstart = 'MarkSelectionsSavePositions_'.$i++;
   last unless(exists($mark_hash{$markstart}));
   my $indexstart = $w->index($markstart);
   my $markend = 'MarkSelectionsSavePositions_'.$i++;
   last unless(exists($mark_hash{$markend}));
   my $indexend = $w->index($markend);

   push(@ranges, $indexstart, $indexend);
  }

 my @selection_list;
 while (@ranges)
  {
   my ($first) = split(/\./,shift(@ranges));
   my ($last) = split(/\./,shift(@ranges));
   # if previous selection ended on the same line that this selection starts,
   # then fiddle the numbers so that this line number isnt included twice.
   if (defined($selection_list[-1]) and ($first == $selection_list[-1]))
    {
     # if this selection ends on the same line its starts, then skip this sel
     next if ($first == $last);
     $first++; # count this selection starting from the next line.
    }
   push(@selection_list, $first .. $last);
  }
 return @selection_list;
}

sub insertStringAtStartOfSelectedLines
{
 my ($w,$insert_string)=@_;
 $w->addGlobStart;
 $w->MarkSelectionsSavePositions;
 foreach my $line ($w->GetMarkedSelectedLineNumbers)
  {
   $w->insert($line.'.0', $insert_string);
  }
 $w->RestoreSelectionsMarkedSaved;
 $w->addGlobEnd;
}

sub deleteStringAtStartOfSelectedLines
{
 my ($w,$insert_string)=@_;
 $w->addGlobStart;
 $w->MarkSelectionsSavePositions;
 my $length = length($insert_string);
 foreach my $line ($w->GetMarkedSelectedLineNumbers)
  {
   my $start = $line.'.0';
   my $end   = $line.'.'.$length;
   my $current_text = $w->get($start, $end);
   next unless ($current_text eq $insert_string);
   $w->delete($start, $end);
  }
 $w->RestoreSelectionsMarkedSaved;
 $w->addGlobEnd;
}


1;
__END__

