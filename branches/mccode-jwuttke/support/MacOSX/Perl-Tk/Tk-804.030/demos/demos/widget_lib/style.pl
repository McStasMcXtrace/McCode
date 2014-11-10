# style.pl

use vars qw/$TOP/;

sub style {

    # Create a top-level window with a text widget that demonstrates
    # the various display styles that are available in texts.

    my($demo) = @_;
    $TOP = $MW->WidgetDemo(
        -name     => $demo,
        -text     =>'',
        -title    => 'Text Demonstration - Display Styles',
        -iconname => 'style',
    );

    eval { # eval, in case fonts already exist
	$TOP->fontCreate(qw/C_small  -family courier   -size 10/);
	$TOP->fontCreate(qw/C_big    -family courier   -size 14 -weight bold/);
	$TOP->fontCreate(qw/C_vbig   -family helvetica -size 24 -weight bold/);
	$TOP->fontCreate(qw/C_bold   -family courier   -size 12 -weight bold
			 -slant italic/);
    };

    my $t = $TOP->Scrolled(qw/Text -setgrid true -width  70 -height 32
			   -font normal -wrap word -scrollbars e/);
    $t->pack(qw/-expand yes -fill both/);

    # Set up display styles.

    $t->tag(qw/configure bold    -font C_bold/);
    $t->tag(qw/configure big     -font C_big/);
    $t->tag(qw/configure verybig -font C_vbig/);
    if ($TOP->depth > 1) {
	$t->tag(qw/configure color1 -background/ => '#a0b7ce');
	$t->tag(qw/configure color2 -foreground red/);
	$t->tag(qw/configure raised -relief raised -borderwidth 1/);
	$t->tag(qw/configure sunken -relief sunken -borderwidth 1/);
    } else {
	$t->tag(qw/configure color1 -background black -foreground white/);
	$t->tag(qw/configure color2 -background black -foreground white/);
	$t->tag(qw/configure raised -background white -relief raised -bd 1/);
	$t->tag(qw/configure sunken -background white -relief sunken -bd 1/);
    }
    $t->tag(qw/configure bgstipple  -background black -borderwidth 0
	    -bgstipple gray12/);
    $t->tag(qw/configure fgstipple  -fgstipple gray50/);
    $t->tag(qw/configure underline  -underline on/);
    $t->tag(qw/configure overstrike -overstrike on/);
    $t->tag(qw/configure right      -justify right/);
    $t->tag(qw/configure center     -justify center/);
    $t->tag(qw/configure super      -offset 4p -font C_small/);
    $t->tag(qw/configure sub        -offset -2p -font C_small/);
    $t->tag(qw/configure margins    -lmargin1 12m -lmargin2 6m -rmargin 10m/);
    $t->tag(qw/configure spacing     -spacing1 10p -spacing2 2p
	    -lmargin1 12m -lmargin2 6m -rmargin 10m/);

    $t->insert('0.0', 'Text widgets like this one allow you to display ' .
	       'information in a variety of styles.  Display styles are ' .
	       'controlled using a mechanism called ');
    $t->insert('insert', 'tags', 'bold');
    $t->insert('insert', '. Tags are just textual names that you can apply ' .
	       'to one or more ranges of characters within a text widget.  ' .
	       'You can configure tags with various display styles.  If ' .
	       'you do this, then the  tagged characters will be displayed ' .
	       'with the styles you chose.  The available display styles ' .
	       'are:  ');
    $t->insert('insert', "\n\n1. Font.", 'big');
    $t->insert('insert', '  You can choose any X font, ');
    $t->insert('insert', 'large', 'verybig');
    $t->insert('insert', ' or small.');
    $t->insert('insert', "\n\n2. Color.", 'big');
    $t->insert('insert', '  You can change either the ');
    $t->insert('insert', 'background', 'color1');
    $t->insert('insert', ' or ');
    $t->insert('insert', 'foreground', 'color2');
    $t->insert('insert', "\ncolor, or ");
    $t->insert('insert', 'both', ['color1', 'color2']);
    $t->insert('insert', '.');
    $t->insert('insert', "\n\n3. Stippling.", 'big');
    $t->insert('insert', '  You can cause either the ');
    $t->insert('insert', 'background', 'bgstipple');
    $t->insert('insert', ' or ');
    $t->insert('insert', 'foreground', 'fgstipple');
    $t->insert('insert', "\ninformation to be drawn with a stipple fill instead of a solid fill.");
    $t->insert('insert', "\n\n4. Underlining.", 'big');
    $t->insert('insert', '  You can ');
    $t->insert('insert', 'underline', 'underline');
    $t->insert('insert', ' ranges of text.');
    $t->insert('insert', "\n\n5. Overstrikes.", 'big');
    $t->insert('insert', "  You can ");
    $t->insert('insert', "draw lines through", 'overstrike');
    $t->insert('insert', " ranges of text.");
    $t->insert('insert', "\n\n6. 3-D effects.", ' big');
    $t->insert('insert', "  You can arrange for the background to be drawn ");
    $t->insert('insert', 'with a border that makes characters appear either ');
    $t->insert('insert', 'raised', 'raised');
    $t->insert('insert', ' or ');
    $t->insert('insert', 'sunken', 'sunken');
    $t->insert('insert', '.');
    $t->insert('insert', "\n\n7. Justification.", 'big');
    $t->insert('insert', " You can arrange for lines to be displayed\n");
    $t->insert('insert', "left-justified,\n");
    $t->insert('insert', "right-justified, or\n", 'right');
    $t->insert('insert', "centered.", 'center');
    $t->insert('insert', "\n\n8. Superscripts and subscripts." , 'big');
    $t->insert('insert', " You can control the vertical ");
    $t->insert('insert', "position of text to generate superscript effects " .
	       "like 10");
    $t->insert('insert', "n", 'super');
    $t->insert('insert', " or subscript effects like X");
    $t->insert('insert', "i", 'sub');
    $t->insert('insert', ".");
    $t->insert('insert', "\n\n9. Margins.", 'big');
    $t->insert('insert', " You can control the amount of extra space left");
    $t->insert('insert', " on\neach side of the text:\n");
    $t->insert('insert', "This paragraph is an example of the use of ", 'margins');
    $t->insert('insert', "margins.  It consists of a single line of text ", 'margins');
    $t->insert('insert', "that wraps around on the screen.  There are two ", 'margins');
    $t->insert('insert', "separate left margin values, one for the first ", 'margins');
    $t->insert('insert', "display line associated with the text line, ", 'margins');
    $t->insert('insert', "and one for the subsequent display lines, which ", 'margins');
    $t->insert('insert', "occur because of wrapping.  There is also a ", 'margins');
    $t->insert('insert', "separate specification for the right margin, ", 'margins');
    $t->insert('insert', "which is used to choose wrap points for lines.", 'margins');

    $t->insert('insert', "\n\n10. Spacing.", 'big');
    $t->insert('insert', " You can control the spacing of lines with three ");
    $t->insert('insert', "separate parameters.  \"Spacing1\" tells how much ");
    $t->insert('insert', "extra space to leave\nabove a line, \"spacing3\" ");
    $t->insert('insert', "tells how much space to leave below a line,\nand ");
    $t->insert('insert', "if a text line wraps, \"spacing2\" tells how much ");
    $t->insert('insert', "space to leave\nbetween the display lines that ");
    $t->insert('insert', "make up the text line.\n");
    $t->insert('insert', "These indented paragraphs illustrate how spacing ", 'spacing');
    $t->insert('insert', "can be used.  Each paragraph is actually a ", 'spacing');
    $t->insert('insert', "single line in the text widget, which is ", 'spacing');
    $t->insert('insert', "word-wrapped by the widget.\n", 'spacing');
    $t->insert('insert', "Spacing1 is set to 10 points for this text, ", 'spacing');
    $t->insert('insert', "which results in relatively large gaps between ", 'spacing');
    $t->insert('insert', "the paragraphs. Spacing2 is set to 2 points, ", 'spacing');
    $t->insert('insert', "which results in just a bit of extra space ", 'spacing');
    $t->insert('insert', "within a pararaph.  Spacing3 isn't used ", 'spacing');
    $t->insert('insert', "in this example.\n", 'spacing');
    $t->insert('insert', "To see where the space is, select ranges of ", 'spacing');
    $t->insert('insert', "text within these paragraphs.  The selection ", 'spacing');
    $t->insert('insert', "highlight will cover the extra space.", 'spacing');

    $t->mark(qw/set insert 0.0/);

} # end style

1;
