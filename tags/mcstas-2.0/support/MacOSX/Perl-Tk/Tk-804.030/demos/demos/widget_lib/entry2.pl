# entry2.pl

use vars qw/$TOP/;

sub entry2 {

    # Create a top-level window that displays a bunch of entries with
    # scrollbars.

    my($demo) = @_;
    $TOP = $MW->WidgetDemo(
        -name     => $demo,
        -text     => ['Three different entries are displayed below, with a scrollbar for each entry.  You can add characters by pointing, clicking and typing.  The normal Motif editing characters are supported, along with many Emacs bindings.  For example, Backspace and Control-h delete the character to the left of the insertion cursor and Delete and Control-d delete the chararacter to the right of the insertion cursor.  For entries that are too large to fit in the window all at once, you can scan through the entries by dragging with mouse button2 pressed.', -wraplength => '5i'],
        -title    => 'Entry Demonstration (with scrollbars)',
        -iconname => 'entry2',
    );

    my(@pl) = qw/-side top -fill x/;
    my(@scrolled_attributes) = qw/Entry -relief sunken -scrollbars s/;
    my(@spacer_attributes) = qw/-width 20 -height 10/;

    my $e1 = $TOP->Scrolled(@scrolled_attributes)->pack(@pl);
    my $spacer1 = $TOP->Frame(@spacer_attributes)->pack(@pl);
    my $e2 = $TOP->Scrolled(@scrolled_attributes)->pack(@pl);
    my $spacer2 = $TOP->Frame(@spacer_attributes)->pack(@pl);
    my $e3 = $TOP->Scrolled(@scrolled_attributes)->pack(@pl);

    $e1->insert(0, 'Initial value');
    $e2->insert('end', 'This entry contains a long value, much too long to fit in the window at one time, so long in fact that you\'ll have to scan or scroll to see the end.');

} # end entry2

1;
