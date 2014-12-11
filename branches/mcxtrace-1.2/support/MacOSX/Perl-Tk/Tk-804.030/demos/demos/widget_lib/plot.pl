# plot.pl

use Plot;
use vars qw/$TOP/;

sub plot {

    # Create a top-level window containing a canvas displaying a simple
    # graph with data points that can be dragged with the pointing device.

    my($demo) = @_;
    $TOP = $MW->WidgetDemo(
        -name     => $demo,
        -text     => "This window displays a canvas widget containing a simple 2-dimensional plot.  You can doctor the data by dragging any of the points with mouse button 1.\n\nYou can also select a printable area with the mouse button 2.",
        -title    => 'Plot Demonstration',
        -iconname => 'plot',
    );

    my $c = $TOP->Plot(
        -title_color        => 'Brown',
        -inactive_highlight => 'Skyblue2',
        -active_highlight   => 'red',
    );
    $c->pack(qw/-fill x/);

} # end plot

1;
