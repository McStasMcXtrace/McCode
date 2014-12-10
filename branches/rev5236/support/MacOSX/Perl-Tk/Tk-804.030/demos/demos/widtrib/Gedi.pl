# Gedi master advanced text editor.

use Tk::TextEdit;

use vars qw/$TOP/;

my $TOP;
my $text_frame;
my $counter_frame;
my $textwindow;
my $current_line_label;
my $total_line_label;
my $current_column_label;
my $insert_overstrike_mode_label;
my $about_pop_up_reference;
my $menu;
my $help_menu;

sub about_pop_up
{
	my $name = ref($about_pop_up_reference);
	if (defined($about_pop_up_reference))
		{
		$about_pop_up_reference->raise;
		$about_pop_up_reference->focus;
		}
	else
		{
		my $pop = $TOP->Toplevel();
		$pop->title("About");

		$pop->Label(-text=>"Gedi (Gregs EDItor)")->pack();
		$pop->Label(-text=>"Ver. 1.0")->pack();
		$pop->Label(-text=>"Copyright 1999")->pack();
		$pop->Label(-text=>"Greg London")->pack();
		$pop->Label(-text=>"All Rights Reserved.")->pack();
		$pop->Label(-text=>"This program is free software.")->pack();
		$pop->Label(-text=>"You can redistribute it and/or")->pack();
		$pop->Label(-text=>"modify it under the same terms")->pack();
		$pop->Label(-text=>"as Perl itself.")->pack();
		$pop->Label(-text=>"Special Thanks to")->pack();
		$pop->Label(-text=>"Nick Ing-Simmons.")->pack();

		my $button_ok = $pop->Button(-text=>'OK',
			-command => sub {$pop->destroy();
			$about_pop_up_reference = undef;
			} )
			->pack();
		$pop->resizable('no','no');
		$about_pop_up_reference = $pop;
		}
}


sub update_indicators
{
	my ($line,$column)= split(/\./,$textwindow->index('insert'));
	$current_line_label->configure (-text=> "line: $line");
	$current_column_label->configure (-text=> "column: $column");

	my ($last_line,$last_col) = split(/\./,$textwindow->index('end'));
	$total_line_label->configure (-text=> "total lines: $last_line");

	my $mode = $textwindow->OverstrikeMode;
	my $overstrke_insert='Insert Mode';
	if ($mode)
		{$overstrke_insert='Overstrike Mode';}
	$insert_overstrike_mode_label->configure
		(-text=> "$overstrke_insert");

	my $filename = $textwindow->FileName;
	$filename = 'NoName' unless(defined($filename));
	my $edit_flag='';
	if($textwindow->numberChanges)
 		{$edit_flag='edited';}
	$TOP->configure(-title => "Gedi  $edit_flag $filename");
	$textwindow->idletasks;

}






sub Gedi {
    my($demo) = @_;
    $TOP = $MW->WidgetDemo(
        -name             => $demo,
        -text             => 'Gedi master advanced text editor ',
	-geometry_manager => 'grid',
        -title            => 'GEDI Text Editor',
        -iconname         => 'GEDI',
    );

$TOP->withdraw;

$text_frame = $TOP->Frame->pack
	(-anchor=>'nw', -expand=>'yes', -fill => 'both'); # autosizing
$counter_frame = $TOP->Frame->pack(-anchor=>'nw');

$textwindow = $text_frame->Scrolled(
	'TextEdit',
	exportselection => 'true',  # 'sel' tag is associated with selections
	# initial height, if it isnt 1, then autosizing fails
	# once window shrinks below height
	# and the line counters go off the screen.
	# seems to be a problem with the Tk::pack command;
#	height => 40,
	-background => 'white',
	-wrap=> 'none',
	-setgrid => 'true', # use this for autosizing
	-scrollbars =>'se')
	-> pack(-expand => 'yes' , -fill => 'both');	# autosizing

$TOP->protocol('WM_DELETE_WINDOW'=>
 sub{$textwindow->ConfirmExit;}
 );

$SIG{INT} = sub {$textwindow->ConfirmExit;};

$current_line_label = $counter_frame
	-> Label(-text=>'line: 1')
	-> grid(-row=>1,-column=>1, -sticky=>'nw' );

$total_line_label = $counter_frame
	-> Label(-text=>'total lines: 1')
	-> grid(-row=>2,-column=>1, -sticky=>'nw' );

$current_column_label = $counter_frame
	-> Label(-text=>'column: 0')
	-> grid(-row=>3,-column=>1, -sticky=>'nw' );

$insert_overstrike_mode_label = $counter_frame
	-> Label(-text=>' ')
	-> grid(-row=>5,-column=>1, -sticky=>'nw' );

$textwindow->SetGUICallbacks (
 [
  \&update_indicators,
  sub{$textwindow->HighlightAllPairsBracketingCursor}
 ]
);

$menu = $textwindow->menu;

$TOP->configure(-menu => $menu);

$help_menu = $menu->cascade(-label=>'~Help', -tearoff => 0, -menuitems => [
         [Command => 'A~bout', -command => \&about_pop_up]
         ]);


#$TOP->minsize(30,1);
#$TOP->geometry("80x24");

while(<DATA>)
	{$textwindow->insert('insert',$_);}
$textwindow->ResetUndo;

$textwindow->CallNextGUICallback;

# adjust height
$TOP->update;
my $menuheight = ($TOP->wrapper)[1];
my $TOPheight = 30 + $TOP->reqheight + $menuheight;
if ($TOP->screenheight < $TOPheight) {
    $textwindow->GeometryRequest($textwindow->reqwidth, $textwindow->reqheight - ($TOPheight - $TOP->screenheight));
}
$TOP->deiconify;

}


__DATA__

Tk800.015 contains many modifications to the
text based modules, as well as new text modules
and an application that uses them all.
Text.pm, TextUndo.pm, TextEdit.pm, and gedi
have all been updated since the release prior
to Tk800.015.

This demo contains a rundown of all the features
of the text modules, and

What is available in the text modules?
================================================

Text.pm
========

Text.pm is the base text editing module.
Beyond the core functionality of typing text,
Text.pm has built in menu support for basic
editing features such as Find/Replace text,
Copy/Cut/Paste, Goto Line Number, and What
Line Number queries.

These functions are available simply by right
clicking the mouse over the text area. Doing
so will cause a pop-up menu to appear which will
contain cascading menus to give access to all of
these new functions.

Many of these functions will create their own
pop-up windows. Find/Replace will create a pop-up
window which contains an entry for text to
find, an entry for replace text, a number of
radio buttons to control options such as
case sensitivity, and several command buttons to
perform functions such as Find, Find All,
Replace, Replace All.

All of these features have corresponding methods
built into the Text widget. This allows the basic
functions to be built into the widget, and also
allows added features to be built on the lower
level methods as needed. No one should have to
reinvent the wheel when it comes to text editing
features.

Insert and Overstrike modes are also supported
in the Text.pm module. Pressing the <Insert>
key will toggle modes back and forth.

Column based copy/cut/paste features are also
available in the Text.pm module. They are bound
to the following keys:

<F1> clipboardColumnCopy
<F2> clipboardColumnCut
<F3> clipboardColumnPaste

Currently, column based operations are beta versions.
They compensate for tabs, but they will not behave
properly unless the text is all the same font, and
is the same width per character.

Hopefully some future version of Text.pm will correct
for this deficiency.

Column paste should work with overstrike mode.


TextUndo.pm
=============

TextUndo.pm is the second level module, being
derived from the Text.pm module. As it's name
implies, TextUndo supports "UNDO" capability.
It now also supports "REDO" capability.

Undo/redo works on user typed commands and
also programmatically, so that any application
that causes text to be inserted or deleted
can be undone/redone, whether it was directly
typed by the user or indirectly through another
method.

The undo/redo functions support tags, so that
if you delete text with tags, undo will re-insert
the text and re-tag it as well. This will eventually
allow the text modules to support more sophisticated
word processing type features. Such functionality
should be available in a future release of the
text modules.

The TextUndo.pm module also has several added
features to support file based operations.
File based methods include ->Save, ->Load, and
->Include. All methods take a filename as a
parameter. These methods will create a progress
widget to indicate the progress of the operation.

The other feature of the TextUndo.pm module
is the ConfirmDiscard method. This method checks to
see if the text has been modified since it was
last saved. If it has been modified, and the
it will create a pop-up menu asking the user
if they want to save the text to a file before
exiting. This method can easily be tied into
the exit routines, and signal handlers, to provide
a consistent "save before exit?" feel.

TextEdit.pm
=============

The TextEdit.pm is a new module in prototype version
which adds further features to the text modules.
TextEdit is based off of the TextUndo module,
and so has all of the features of TextUndo and
Text.

Features of the TextEdit.pm module include
parenthesis matching. The module looks at the
current cursor position and then tries to find
the parenthesis that bracket the cursor.
Character pairs that are searched for are:
() {} [] "" ''

It also checks the position of the pairs to
try to highlight bad positions. The module
assumes that if the pairs are not on the same
line or not on the same column, then there
might be a missing parenthesis somewhere.
Characters that appear to not align are
highlighted in red.

(quotations must start and end on the same line)


PARENTHISIS MATCHING DEMO:
move the cursor to the x between the quotes
on the line below:


{
		(  )
	(	{  	}
		[
	'	">> x <<"	'
	[]	]
	)

}

PARENTHESIS MISMATCHING DEMO:
move the cursor to the x between the quotes
on the line below:


{
		(  )
	 ( <<RED possible error		{  	}
		[
	'	">> x <<"	'
	[]	]
	) <<RED possible error

}



Another feature of the TextEdit module is support
for application level indicators which reflect
the status of certain internals.  The line and
column position of the cursor, the total length
of the file, whether the widget is in insert or
overstrike mode.  Anytime anything occurs that could
affect these values, a user supplied callback
is invoked. This callback is supplied by the
application so that the application can update
whatever indicators it uses, however it implements
them.

One other feature of the TextEdit.pm module is
block level text indention and block level text
commenting. If a block of text is selected,
that text can be indented or unindented wiht
a single keystroke. It can also be commented
out or uncommented as well. The keystroke bindings
that support this are:

<F5> IndentSelectedLines
<F6> UnindentSelectedLines

<F7> CommentSelectedLines
<F8> UncommentSelectedLines

These bindings only operate on the currently
selected text. The indent string and the comment
string can be programmed to be anything, but
defaults to "\t" (tab) for indent and "#" for
comments.

(currently the widget hash is used to store these values.
$w->{'INDENT_STRING'} and $w->{'LINE_COMMENT_STRING'}
At some point in the future, this will be changed to
use configure options to set these values.
any application that changes these values should do
so in such a way that when the TextEdit module changes,
the application can be easily changed to handle this)



gedi application
=====================
gedi is short for Greg's EDItor.
The "g" is soft, pronounced like a "j".

The gedi application uses all of the features of
the text modules, Text, TextUndo, and TextEdit.
It supplies TextEdit with a callback to update
the indicator displays. This information includes
the current cursor position, insert/overstrike
mode, length of the file, filename, and whether
the file has been edited or not.

The bottom of this display contains
line number
column number
total lines
insert/overstrike mode.

The title bar contains
filename
and if the file has been edited, the word "edited".

Where gedi is installed depends on your system,
but it is part of the tarkit for Tk800.015 and above.

gedi was created to be put a perl editor in with the
perl tar kit.

NOTE: THIS IS NOT THE ACTUAL GEDI APPLICATION, BUT
A DEMO SET UP TO BE SIMILAR IN NATURE TO THE GEDI
APPLICATION. THE ACTUAL GEDI APPLICATION IS PART OF
THE TK800.015 TARKIT. WHERE IT IS LOCATED ON YOUR
SYSTEM WILL VARY DEPENDING ON YOUR SYSTEM. ONCE
YOU LOCATE THE GEDI APPLICATION, PUT IT IN YOUR
EXECUTABLE PATH, AND YOU WILL BE ABLE TO USE IT AS
A TEXT EDITOR.






