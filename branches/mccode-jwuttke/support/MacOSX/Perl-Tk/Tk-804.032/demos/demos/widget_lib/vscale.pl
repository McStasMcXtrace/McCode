# vscale.pl

use subs qw/vscale_height/;
use vars qw/$TOP/;

sub vscale {

    # Create a top-level window that displays a vertical scale.

    my($demo) = @_;
    $TOP = $MW->WidgetDemo(
        -name     => $demo,
        -text     => 'An arrow and a vertical scale are displayed below.  If you click or drag mouse button 1 in the scale, you can change the size of the arrow.',
        -title    => 'Vertical Scale Demonstration',
        -iconname => 'vscale',
    );

    my $frame = $TOP->Frame(-borderwidth => 10)->pack;

    my $canvas = $frame->Canvas(
        qw/-width 50 -height 50 -borderwidth 0 -highlightthickness 0/);
    $canvas->createPolygon(qw/0 0 1 1 2 2 -fill SeaGreen3 -tags poly/);
    $canvas->createLine(qw/0 0 1 1 2 2 0 0 -fill black -tags line/);

    my $scale = $frame->Scale(qw/-orient vertical -length 284 -from 0
        -to 250 -tickinterval 50 -command/ => [\&vscale_height, $canvas]);
    $scale->set(75);

    $scale->pack(qw/-side left -anchor ne/);
    $canvas->pack(qw/-side left -anchor nw -fill y/)

} # end vscale

sub vscale_height {

    my($w, $height) = @_;

    $height += 21;
    my $y2 = $height - 30;
    $y2 = 21 if $y2 < 21;
    $w->coords('poly', 15, 20, 35, 20, 35, $y2, 45, $y2, 25, $height, 5, $y2,
	       15, $y2, 15, 20);
    $w->coords('line', 15, 20, 35, 20, 35, $y2, 45, $y2, 25, $height, 5, $y2,
	       15, $y2, 15, 20);

} # end vscale_height

1;
