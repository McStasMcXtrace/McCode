# entry1.pl

use vars qw/$TOP/;

sub entry1 {

    # Create a top-level window that displays a bunch of entries.

    my($demo) = @_;
    $TOP = $MW->WidgetDemo(
        -name     => $demo,
        -text     => ['Three different entries are displayed below.  You can add characters by pointing, clicking and typing.  The normal Motif editing characters are supported, along with many Emacs bindings.  For example, Backspace and Control-h delete the character to the left of the insertion cursor and Delete and Control-d delete the chararacter to the right of the insertion cursor.  For entries that are too large to fit in the window all at once, you can scan through the entries by dragging with mouse button2 pressed.', qw/-wraplength 5i/],
        -title    => 'Entry Demonstration (no scrollbars)',
        -iconname => 'entry1',
    );

    my(@relief) = qw/-relief sunken/;
    my(@pl) = qw/-side top -padx 10 -pady 5 -fill x/;
    my $e1 = $TOP->Entry(@relief)->pack(@pl);
    my $e2 = $TOP->Entry(@relief)->pack(@pl);
    my $e3 = $TOP->Entry(@relief)->pack(@pl);

    $e1->insert(0, 'Initial value');
    $e2->insert('end', 'This entry contains a long value, much too long to fit in the window at one time, so long in fact that you\'ll have to scan or scroll to see the end.');

} # end entry1

1;
