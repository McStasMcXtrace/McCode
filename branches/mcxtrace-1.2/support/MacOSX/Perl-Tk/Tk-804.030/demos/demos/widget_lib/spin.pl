# spin.pl

use vars qw/$TOP/;

sub spin {

    # This demonstration script creates several spinbox widgets.

    my($demo) = @_;
    $TOP = $MW->WidgetDemo(
        -name       => $demo,
        -text       => ['Three different spin-boxes are displayed below.  You can add characters by pointing, clicking and typing.  The normal Motif editing characters are supported, along with many Emacs bindings.  For example, Backspace and Control-h delete the character to the left of the insertion cursor and Delete and Control-d delete the chararacter to the right of the insertion cursor.  For values that are too large to fit in the window all at once, you can scan through the value by dragging with mouse button2 pressed.  Note that the first spin-box will only permit you to type in integers, and the third selects from a list of Australian cities.',
        -wraplength => '5i'],
        -title      => 'Spinbox Demonstration',
        -iconname   => 'spin',
    );

    my $s1 = $TOP->Spinbox(
        qw/-from 1 -to 10 -width 10 -validate key/,
        -validatecommand => sub {
	    my ($proposed, $changes, $current, $index, $type) = @_;
	    return not $proposed =~ m/[^\d]/g;
	},
    );
    my $s2 = $TOP->Spinbox(
        qw/-from 0 -to 3 -increment .5 -format %05.2f -width 10/,
    );
    my $s3 = $TOP->Spinbox(
        -values => [qw/Canberra Sydney Melbourne Perth Adelaide Brisbane Hobart
		    Darwin/, 'Alice Springs'],
        -width  => 10,
    );
    
    $s1->pack($s2, $s3, qw/-side top -pady 5 -padx 10/);

} # end spin.pl
