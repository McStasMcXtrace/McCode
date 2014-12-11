# bitmaps.pl

use subs qw/bitmaps_row/;
use vars qw/$TOP/;

sub bitmaps {

    # Create a top-level window that displays all of Tk's built-in bitmaps.

    my($demo) = @_;
    $TOP = $MW->WidgetDemo(
        -name     => $demo,
        -text     => 'This window displays all of Tk\'s built-in bitmaps, along with the names you can use for them in Perl scripts.',
        -title    => 'Bitmap Demonstration',
        -iconname => 'bitmaps',
    );

    my $frame = $TOP->Frame;
    $frame->pack(qw/-side top -expand yes -fill both/);
    bitmaps_row $frame, qw/error gray12 gray25 gray50 gray75 hourglass/;
    bitmaps_row $frame, qw/info questhead question Tk transparent warning/;

} # end bitmaps

sub bitmaps_row {

    # The procedure below creates a new row of bitmaps in a window.

    my($w, @names) = @_;

    my $row = $w->Frame->pack(qw/-side top -fill both/);

    foreach my $bitmap_name (@names) {
	my $bit = $row->Frame;
	$bit->pack(qw/-side left -fill both -pady .25c -padx .25c/);
	my $label = $bit->Label(-text => $bitmap_name, -width => 9);
	$label->pack(qw/-side bottom/);
	my $bitmap = $bit->Label('-bitmap' => $bitmap_name);
	$bitmap->pack(qw/-side bottom/);
    }

} # end bitmaps_row

1;
