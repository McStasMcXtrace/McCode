# image1.pl

use vars qw/$TOP/;

sub image1 {

    # This demonstration script displays two image widgets.

    my($demo) = @_;
    $TOP = $MW->WidgetDemo(
        -name     => $demo,
        -text     => 'This demonstration displays two images, each in a separate label widget.',
        -title    => 'Image Demonstration #1',
        -iconname => 'image1',
    );

    my(@pl) = qw/-side top -padx .5m -pady .5m/;
    $TOP->Photo('image1a', -file => Tk->findINC('demos/images/earth.gif'));
    $TOP->Label(-image => 'image1a')->pack(@pl);

    $TOP->Photo('image1b', -file => Tk->findINC('demos/images/earthris.gif'));
    $TOP->Label(-image => 'image1b')->pack(@pl);

} # end image1

1;


