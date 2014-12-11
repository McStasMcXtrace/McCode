# Copyright (c) 1999 Greg Bartels. All rights reserved.
# This program is free software; you can redistribute it and/or
# modify it under the same terms as Perl itself.

# Special thanks to Nick Ing-Simmons for pushing a lot of
# my text edit functionality into Text.pm and TextUndo.pm
# otherwise, this module would have been monstrous.

# Andy Worhal had it wrong, its "fifteen megabytes of fame"
#	-Greg Bartels

package Tk::TextEdit;


use vars qw($VERSION);
$VERSION = '4.004'; # $Id: //depot/Tkutf8/Tk/TextEdit.pm#4 $

use Tk qw (Ev);
use AutoLoader;

use Text::Tabs;

use base qw(Tk::TextUndo);

Construct Tk::Widget 'TextEdit';

#######################################################################
#######################################################################
sub ClassInit
{
 my ($class,$mw) = @_;
 $class->SUPER::ClassInit($mw);

 $mw->bind($class,'<F5>', 'IndentSelectedLines');
 $mw->bind($class,'<F6>', 'UnindentSelectedLines');

 $mw->bind($class,'<F7>', 'CommentSelectedLines');
 $mw->bind($class,'<F8>', 'UncommentSelectedLines');

 return $class;
}

# 8 horizontal pixels in the "space" character in default font.
my $tab_multiplier = 8;

sub debug_code_f1
{
 my $w=shift;
}

sub debug_code_f2
{
 my $w=shift;
}

#######################################################################
#######################################################################
sub InitObject
{
 my ($w) = @_;
 $w->SUPER::InitObject;

 $w->{'INDENT_STRING'} = "\t";   #  Greg mode=>"\t",   Nick mode=>" "
 $w->{'LINE_COMMENT_STRING'} = "#";   #  assuming perl comments

 my %pair_descriptor_hash =
	(
	'PARENS' => [ 'multiline', '(', ')', "[()]" ],
	'CURLIES' => [ 'multiline', '{', '}', "[{}]" ],
	'BRACES' => [ 'multiline', '[', ']', "[][]" ],
	'DOUBLEQUOTE' => [ 'singleline', "\"","\"" ],
	'SINGLEQUOTE' => [ 'singleline', "'","'" ],
	);

 $w->{'HIGHLIGHT_PAIR_DESCRIPTOR_HASH_REF'}=\%pair_descriptor_hash;

 $w->tagConfigure
  ('CURSOR_HIGHLIGHT_PARENS', -foreground=>'white', -background=>'violet');
 $w->tagConfigure
  ('CURSOR_HIGHLIGHT_CURLIES', -foreground=>'white', -background=>'blue');
 $w->tagConfigure
  ('CURSOR_HIGHLIGHT_BRACES', -foreground=>'white', -background=>'purple');
 $w->tagConfigure
  ('CURSOR_HIGHLIGHT_DOUBLEQUOTE', -foreground=>'black', -background=>'green');
 $w->tagConfigure
  ('CURSOR_HIGHLIGHT_SINGLEQUOTE', -foreground=>'black', -background=>'grey');

 $w->tagConfigure('BLOCK_HIGHLIGHT_PARENS', -background=>'red');
 $w->tagConfigure('BLOCK_HIGHLIGHT_CURLIES', -background=>'orange');
 $w->tagConfigure('BLOCK_HIGHLIGHT_BRACES', -background=>'red');
 $w->tagConfigure('BLOCK_HIGHLIGHT_DOUBLEQUOTE', -background=>'red');
 $w->tagConfigure('BLOCK_HIGHLIGHT_SINGLEQUOTE', -background=>'red');

 $w->tagRaise('BLOCK_HIGHLIGHT_PARENS','CURSOR_HIGHLIGHT_PARENS');
 $w->tagRaise('BLOCK_HIGHLIGHT_CURLIES','CURSOR_HIGHLIGHT_CURLIES');
 $w->tagRaise('BLOCK_HIGHLIGHT_BRACES','CURSOR_HIGHLIGHT_BRACES');
 $w->tagRaise('BLOCK_HIGHLIGHT_DOUBLEQUOTE','CURSOR_HIGHLIGHT_DOUBLEQUOTE');
 $w->tagRaise('BLOCK_HIGHLIGHT_SINGLEQUOTE','CURSOR_HIGHLIGHT_SINGLEQUOTE');

 $w->{'UPDATE_WIDGET_PERIOD'}=300;  # how much time between each call.
 $w->{'WINDOW_PLUS_AND_MINUS_VALUE'}=80;
 $w->SetGUICallbackIndex(0);
 $w->schedule_next_callback;

}

#######################################################################

sub cancel_current_gui_callback_and_restart_from_beginning
{
 my ($w)=@_;
 if(defined($w->{'UPDATE_WIDGET_AFTER_REFERENCE'}))
  {$w->{'UPDATE_WIDGET_AFTER_REFERENCE'}->cancel();}
 $w->SetGUICallbackIndex(0);

 $w->schedule_next_callback;
}

sub schedule_next_callback
{
 my ($w)=@_;
 return if $w->NoMoreGUICallbacksToCall; #stops infinite recursive call.
 $w->{'UPDATE_WIDGET_AFTER_REFERENCE'} = $w->after
   ($w->{'UPDATE_WIDGET_PERIOD'},
    sub
    {
    $w->CallNextGUICallback;
    $w->schedule_next_callback;
    }
   );

}


#######################################################################
# use these methods to pass the TextEdit widget an anonymous array
# of code references.
# any time the widget changes that requires the display to be updated,
# then these code references will be scheduled in sequence for calling.
# splitting them up allows them to be prioritized by order,
# and prevents the widget from "freezing" too long if they were
# one large callback. scheduling them apart allows the widget time
# to respond to user inputs.
#######################################################################
sub SetGUICallbacks
{
 my ($w,$callback_array_ref) = @_;
 $w->{GUI_CALLBACK_ARRAY_REF}=$callback_array_ref;
 $w->SetGUICallbackIndex(0);
}

sub GetGUICallbacks
{
 return shift->{GUI_CALLBACK_ARRAY_REF};
}

sub SetGUICallbackIndex
{
 my ($w, $val)=@_;
 $w->{GUI_CALLBACK_ARRAY_INDEX}=$val;
}

sub GetGUICallbackIndex
{
 return shift->{GUI_CALLBACK_ARRAY_INDEX};
}

sub IncrementGUICallbackIndex
{
 shift->{GUI_CALLBACK_ARRAY_INDEX} += 1;
}

sub NoMoreGUICallbacksToCall
{
 my ($w) = @_;
 return 0 unless defined ($w->{GUI_CALLBACK_ARRAY_REF});
 return 0 unless defined ($w->{GUI_CALLBACK_ARRAY_INDEX});
 my $arr_ref = $w->{GUI_CALLBACK_ARRAY_REF};
 my $arr_ind = $w->{GUI_CALLBACK_ARRAY_INDEX};
 return $arr_ind >= @$arr_ref;
}

sub CallNextGUICallback
{
 my ($w) = @_;
 return if $w->NoMoreGUICallbacksToCall;
 my $arr_ref = $w->{GUI_CALLBACK_ARRAY_REF};
 my $arr_ind = $w->{GUI_CALLBACK_ARRAY_INDEX};
  &{$arr_ref->[$arr_ind]};
 $w->IncrementGUICallbackIndex;
}


#######################################################################
#######################################################################

sub insert
{
 my $w = shift;
 $w->SUPER::insert(@_);
 $w->cancel_current_gui_callback_and_restart_from_beginning;
}

sub delete
{
 my $w = shift;
 $w->SUPER::delete(@_);
 $w->cancel_current_gui_callback_and_restart_from_beginning;
}

sub SetCursor
{
 my $w = shift;
 $w->SUPER::SetCursor(@_);
 $w->cancel_current_gui_callback_and_restart_from_beginning;
}

sub OverstrikeMode
{
 my ($w,$mode) = @_;
 if (defined($mode))
  {
  $w->SUPER::OverstrikeMode($mode);
  $w->cancel_current_gui_callback_and_restart_from_beginning;
  }
 return $w->SUPER::OverstrikeMode;
}


#######################################################################
# use yview on scrollbar to get fractional coordinates.
# scale this by the total length of the text to find the
# approximate start line of widget and end line of widget.
#######################################################################
sub GetScreenWindowCoordinates
{
 my $w = shift;
 my ($top_frac, $bot_frac) = $w->yview;
 my $end_index = $w->index('end');
 my ($lines,$columns) = split (/\./,$end_index);
 my $window = $w->{'WINDOW_PLUS_AND_MINUS_VALUE'};
 my $top_line = int(($top_frac * $lines) - $window);
 $top_line = 0 if ($top_line < 0);
 my $bot_line = int(($bot_frac * $lines) + $window);
 $bot_line = $lines if ($bot_line > $lines);
 my $top_index = $top_line . '.0';
 my $bot_index = $bot_line . '.0';

 $_[0] = $top_index;
 $_[1] = $bot_index;
}

########################################################################
# take two indices as inputs.
# if they are on the same line or same column (accounting for tabs)
# then return 1
# else return 0
# (assume indices passed in are in line.column format)
########################################################################
sub IndicesLookGood
{
 my ($w, $start, $end, $singleline) = @_;

 return 0 unless ( (defined($start)) and (defined($end)));

 my ($start_line, $start_column) = split (/\./,$start);
 my ($end_line,   $end_column)   = split (/\./,$end);

 ##########################
 # good if on the same line
 ##########################
 return 1 if ($start_line == $end_line);

 ##########################
 # if not on same line and its a singleline, its bad
 ##########################
 return 0 if $singleline;


 # get both lines, convert the tabs to spaces, and get the new column.
 # see if they line up or not.
 my $string;
 $string = $w->get($start_line.'.0', $start_line.'.0 lineend');
 $string = substr($string, 0, $start_column+1);
 $string = expand($string);
 $start_column = length($string);

 $string = $w->get($end_line.'.0', $end_line.'.0 lineend');
 $string = substr($string, 0, $end_column +1);
 $string = expand($string);
 $end_column = length($string);

 ##########################
 # good if on the same column (adjusting for tabs)
 ##########################
 return 1 if ($start_column == $end_column);

 # otherwise its bad
 return 0;
}

########################################################################
# if searching backward, count paranthesis until find a start parenthesis
# which does not have a forward match.
#
# (<= search backward will return this index
#    ()
#      START X HERE
#   ( (  )  () )
# )<== search forward will return this index
#
# if searching forward, count paranthesis until find a end parenthesis
# which does not have a rearward match.
########################################################################
sub searchForBaseCharacterInPair
{
 my
  (
   $w, $top_index, $searchfromindex, $bot_index,
   $direction, $startchar, $endchar, $charpair
  )=@_;
 my ($plus_one_char, $search_end_index, $index_offset, $done_index);
 if ($direction eq '-forward')
  {
  $plus_one_char = $endchar;
  $search_end_index = $bot_index;
  $index_offset = ' +1c';
  $done_index = $w->index('end');
  }
 else
  {
  $plus_one_char = $startchar;
  $search_end_index = $top_index;
  $index_offset = '';
  $done_index = '1.0';
  }

 my $at_done_index = 0;
 my $count = 0;
 my $char;
 while(1)
  {
  $searchfromindex = $w->search
   ($direction, '-regexp', $charpair, $searchfromindex, $search_end_index );

  last unless(defined($searchfromindex));
  $char = $w->get($searchfromindex, $w->index($searchfromindex.' +1c'));
  if ($char eq $plus_one_char)
   {$count += 1;}
  else
   {$count -= 1;}
  last if ($count==1);
  # boundary condition exists when first char in widget is the match char
  # need to be able to determine if search tried to go past index '1.0'
  # if so, set index to undef and return.
  if ( $at_done_index )
   {
   $searchfromindex = undef;
   last;
   }
  $at_done_index = 1 if ($searchfromindex eq $done_index);
  $searchfromindex=$w->index($searchfromindex . $index_offset);
  }
 return $searchfromindex;
}

########################################################################
# highlight a character pair that most closely brackets the cursor.
# allows you to pick and choose which ones you want to do.
########################################################################

sub HighlightParenthesisAroundCursor
{
 my ($w)=@_;
 $w->HighlightSinglePairBracketingCursor
  ( '(', ')', '[()]', 'CURSOR_HIGHLIGHT_PARENS','BLOCK_HIGHLIGHT_PARENS',0);
}

sub HighlightCurlyBracesAroundCursor
{
 my ($w)=@_;
 $w->HighlightSinglePairBracketingCursor
  ( '{', '}', '[{}]', 'CURSOR_HIGHLIGHT_CURLIES','BLOCK_HIGHLIGHT_CURLIES',0);
}

sub HighlightBracesAroundCursor
{
 my ($w)=@_;
 $w->HighlightSinglePairBracketingCursor
  ( '[', ']','[][]', 'CURSOR_HIGHLIGHT_BRACES','BLOCK_HIGHLIGHT_BRACES',0);
}

sub HighlightDoubleQuotesAroundCursor
{
 my ($w)=@_;
 $w->HighlightSinglePairBracketingCursor
  ( "\"", "\"", "\"", 'CURSOR_HIGHLIGHT_DOUBLEQUOTE','BLOCK_HIGHLIGHT_DOUBLEQUOTE',1);
}

sub HighlightSingleQuotesAroundCursor
{
 my ($w)=@_;
 $w->HighlightSinglePairBracketingCursor
  ( "'", "'", "'", 'CURSOR_HIGHLIGHT_SINGLEQUOTE','BLOCK_HIGHLIGHT_SINGLEQUOTE',1);
}

########################################################################
# highlight all the character pairs that most closely bracket the cursor.
########################################################################
sub HighlightAllPairsBracketingCursor
{
 my ($w)=@_;
 $w->HighlightParenthesisAroundCursor;
 $w->HighlightCurlyBracesAroundCursor;
 $w->HighlightBracesAroundCursor;
 $w->HighlightDoubleQuotesAroundCursor;
 $w->HighlightSingleQuotesAroundCursor;
}

########################################################################
# search for a pair of matching characters that bracket the
# cursor and tag them with the given tagname.
# startchar might be '['
# endchar would then be ']'
# tagname is a name of a tag, which has already been
# configured to highlight however the user wants them to behave.
# error tagname is the tag to highlight the chars with if there
# is a problem of some kind.
# singleline indicates whether the character pairs must occur
# on a single line. quotation marks are single line characters usually.
########################################################################
sub HighlightSinglePairBracketingCursor
{
 my
  (
   $w, $startchar, $endchar, $charpair,
   $good_tagname, $bad_tagname, $single_line
  ) = @_;
 $single_line=0 unless defined($single_line);
 $w->tagRemove($good_tagname, '1.0','end');
 $w->tagRemove($bad_tagname, '1.0','end');
 my $top_index; my $bot_index;
 my $cursor = $w->index('insert');
 if ($single_line)
  {
  $top_index = $w->index($cursor.' linestart');
  $bot_index = $w->index($cursor.' lineend');
  }
 else
  {
  $w->GetScreenWindowCoordinates($top_index, $bot_index);
  }

 # search backward for the startchar
 #  $top_index, $searchfromindex, $bot_index,
 #  $direction, $startchar, $endchar, $charpair

 my $startindex = $w->searchForBaseCharacterInPair
  (
   $top_index, $cursor, $bot_index,
   '-backward', $startchar, $endchar, $charpair
  );

 # search forward for the endchar
 my $endindex = $w->searchForBaseCharacterInPair
  (
   $top_index, $cursor, $bot_index,
   '-forward', $startchar, $endchar, $charpair
  );
 return unless ((defined $startindex) and (defined $endindex));

 my $final_tag = $bad_tagname;
 if ($w->IndicesLookGood( $startindex, $endindex, $single_line))
  {
  $final_tag = $good_tagname;
  }

 $w->tagAdd($final_tag, $startindex, $w->index($startindex.'+1c') );
 $w->tagAdd($final_tag,   $endindex, $w->index(  $endindex.'+1c') );
}

####################################################################
sub IndentSelectedLines
{
 my($w)=@_;
 $w->insertStringAtStartOfSelectedLines($w->{'INDENT_STRING'});
}

sub UnindentSelectedLines
{
 my($w)=@_;
 $w->deleteStringAtStartOfSelectedLines($w->{'INDENT_STRING'});
}

sub CommentSelectedLines
{
 my($w)=@_;
 $w->insertStringAtStartOfSelectedLines($w->{'LINE_COMMENT_STRING'});
}

sub UncommentSelectedLines
{
 my($w)=@_;
 $w->deleteStringAtStartOfSelectedLines($w->{'LINE_COMMENT_STRING'});
}


1;
__END__
