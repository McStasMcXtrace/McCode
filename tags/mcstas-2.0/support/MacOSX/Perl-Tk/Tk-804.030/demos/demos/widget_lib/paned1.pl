# paned1.pl

use vars qw/$TOP/;

sub paned1 {

    # This demonstration script creates a toplevel window containing
    # a paned window that separates two windows horizontally.

    my($demo) = @_;
    $TOP = $MW->WidgetDemo(
        -name     => $demo,
        -text     => 'The sash between the two coloured windows below can be used to divide the area between them. Use the left mouse button to resize without redrawing by just moving the sash, and use the middle mouse button to resize opaquely (always redrawing the windows in each position.)',
        -title    => 'Horizontal Paned Window Demonstration',
        -iconname => 'paned1',
    );

    my $pw = $TOP->Panedwindow;
    $pw->pack(qw/-side top -expand yes -fill both -pady 2 -padx 2m/);

    my $l1 = $pw->Label(-text => "This is the\nleft side", -background => 'yellow');
    my $l2 = $pw->Label(-text => "This is the\nright side", -background =>'cyan');

    $pw->add($l1, $l2);

} # end paned1
