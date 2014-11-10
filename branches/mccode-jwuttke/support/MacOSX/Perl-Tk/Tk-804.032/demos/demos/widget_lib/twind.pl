# twind.pl

use Plot;
use subs qw/twind_create_plot twind_delete_plot twind_restore_bg/;
use vars qw/$TOP/;

sub twind {

    # Create a top-level window with a text widget that demonstrates the
    # use of embedded windows in Text widgets.

    my($demo) = @_;
    $TOP = $MW->WidgetDemo(
        -name     => $demo,
        -text     => '',
        -title    => 'Text Demonstration - Embedded Windows',
        -iconname => 'twind',
    );

    # By default, when you create a Scrolled instance of a Perl/Tk widget
    # the scrollbars are always displayed; that is, they are required.  But
    # you can have optional scrollbars as well, specified via the -scrollbars
    # specifier.  So, assume scrollbars can be postioned 'nsew' (north, south
    # east or west), or 'se' for southeast, etcetera.  You specify 'required'
    # or 'optional' using an 'r' or 'o' character, respectively, preceeding
    # the scrollbar position.  So the following Scrolled widget has an
    # optional scrollbar at the bottom of the text widget and a required
    # scrollbar positioned to the right.
    #
    # Optional scrollbars are only displayed if they are required, so, the
    # the southern scrollbar is displayed IFF -wrap => none.

    my $t = $TOP->Scrolled(qw/Text -setgrid true -width 70 -height 35
        -wrap word -highlightthickness 0 -borderwidth 0 -scrollbars osre
	-font/ => $FONT)->pack;

    $t->tag(qw/configure center -justify center	-spacing1 5m -spacing3 5m/);
    $t->tag(qw/configure buttons -lmargin1 1c -lmargin2 1c -rmargin 1c
	    -spacing1 3m -spacing2 0 -spacing3 0/);

    my $t_on = $t->Button(
        -text => 'Turn On',
        -command => [$t => qw/configure -wrap none/],
        -cursor => 'top_left_arrow',
    );
    my $t_off = $t->Button(
        -text => 'Turn Off',
        -command => [$t => qw/configure -wrap word/],
	-cursor => 'top_left_arrow',
    );

    my $t_click = $t->Button(
        -text    => 'Click Here',
	-command => [\&twind_create_plot, $t],
	-cursor  => 'top_left_arrow',
    );
    my $t_delete = $t->Button(
        -text    => 'Delete',
	-command => [\&twind_delete_plot, $t],
	-cursor  => 'top_left_arrow',
    );

    $t->insert('end', "A text widget can contain other widgets embedded ");
    $t->insert('end', "in it.  These are called ");
    $t->insert('end', "\"embedded windows\"");
    $t->insert('end', ", and they can consist of arbitrary widgets.  ");
    $t->insert('end', "For example, here are two embedded button ");
    $t->insert('end', "widgets.  You can click on the first button to ");
    $t->window('create', 'end', -window => $t_on);
    $t->insert('end', " horizontal scrolling, which also turns off ");
    $t->insert('end', "word wrapping.  Or, you can click on the second ");
    $t->insert('end', "button to\n");
    $t->window('create', 'end', -window => $t_off);
    $t->insert('end', " horizontal scrolling and turn back on word ");
    $t->insert('end', "wrapping.\n\n");

    $t->insert('end', "Or, here is another example.  If you ");
    $t->window('create', 'end', -window => $t_click);
    $t->insert('end', " a canvas displaying an x-y plot will appear ");
    $t->insert('end', "right here.");
    $t->mark('set', 'plot', 'insert');
    $t->mark('gravity', 'plot', 'left');
    $t->insert('end', "  You can drag the data points around with the ");
    $t->insert('end', "mouse, or you can click here to ");
    $t->window('create', 'end', -window => $t_delete);
    $t->insert('end', " the plot again.\n\n");

    $t->insert('end', "You may also find it useful to put embedded windows");
    $t->insert('end', " in a text without any actual text.  In this case ");
    $t->insert('end', "the text widget acts like a geometry manager.  For ");
    $t->insert('end', "example, here is a collection of buttons laid out ");
    $t->insert('end', "neatly into rows by the text widget.  These buttons");
    $t->insert('end', " can be used to change the background color of the ");
    $t->insert('end', "text widget (\"Default\" restores the color to ");
    $t->insert('end', "its default).  If you click on the button labeled ");
    $t->insert('end', "\"Short\", it changes to a longer string so that ");
    $t->insert('end', "you can see how the text widget automatically ");
    $t->insert('end', "changes the layout.  Click on the button again ");
    $t->insert('end', "to restore the short string.\n");

    my $t_default = $t->Button(
        -text => 'Default',
	-command => [\&twind_restore_bg, $t],
	-cursor  => 'top_left_arrow',
    );
    $t->window('create', 'end', -window => $t_default, -padx => 3);
    my $embToggle = 'Short';
    my $t_toggle = $t->Checkbutton(
        -textvariable => \$embToggle,
        -indicatoron  => 0,
        -variable     => \$embToggle,
        -onvalue      => 'A much longer string',
        -offvalue     => 'Short',
        -cursor       => 'top_left_arrow',
    );
    $t->window('create', 'end', -window => $t_toggle,
			     -padx => 3, -pady => 2);
    my($i, $color) = (1, '');
    foreach $color (qw(AntiqueWhite3 Bisque1 Bisque2 Bisque3 Bisque4
		       SlateBlue3 RoyalBlue1 SteelBlue2 DeepSkyBlue3
		       LightBlue1 DarkSlateGray1 Aquamarine2 DarkSeaGreen2
		       SeaGreen1 Yellow1 IndianRed1 IndianRed2 Tan1 Tan4)) {
	my $col = $t->Button(
            -text   => "$color",
	    -cursor => 'top_left_arrow',
        );
        $col->configure(-command => sub {
	    $t->configure(-background => $color);
	});
        $t->window('create', 'end', -window => $col,
	             -padx => 3, -pady => 2);
        $i++;
    }
    $t->tag('add', 'buttons', $t_default, 'end');

} # end twind

sub twind_create_plot {

    # We are required to create a new Plot object everytime since embedded
    # widgets are destroyed when their tag is deleted. (Too bad.)

    my($text) = @_;

    if (not Exists($twind::plot)) {
        $twind::plot = $text->Plot(
	    -title_color        => 'Brown',
            -inactive_highlight => 'Skyblue2',
            -active_highlight   => 'red',
        );

        while ($text->get('plot') =~ / |\t|\n/) {
            $text->delete('plot');
	}
	$text->insert('plot', "\n");
	$text->window('create', 'plot', -window => $twind::plot);
	$text->tag('add', 'center', 'plot');
	$text->insert('plot', "\n");
    } # ifend

} # end twind_create_plot

sub twind_delete_plot {

    my($text) = @_;

    if (Exists($twind::plot)) {
	$text->delete($twind::plot);
	while ($text->get('plot') =~ / |\t|\n/) {
	    $text->delete('plot');
	}
	$text->insert('plot', '  ');
    }

} # end twind_delete_plot

sub twind_restore_bg {

    my($text) = @_;

    $text->configure(-background =>
		     ($text->Subwidget('text')->configure(-background))[3]);

} # end twind_restore_bg

1;
