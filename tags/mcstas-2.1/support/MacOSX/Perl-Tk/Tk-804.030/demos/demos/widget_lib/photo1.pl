# photo1.pl

use vars qw/$TOP/;

sub photo1 {

    my($demo) = @_;

    $TOP = $MW->WidgetDemo(
        -name             => $demo,
        -text             => 'This demonstration displays, for two seconds, a picture of a teapot over a green background, then proceeeds to render transparent a 50 x 50 pixel area of the teapot so that the green background shows through.',
        -title            => 'Transparent Pixels',
        -iconname         => 'photo1',
    );

    my $l = $TOP->Label( qw/ -background green -width 300 -height 300 / )->pack;

    my $f1 = $TOP->Photo( -file => Tk->findINC( 'demos/images/teapot.ppm' ) );
    $l->configure( -image => $f1 );
    $TOP->idletasks;
    $TOP->after(2000);

    foreach my $x ( 50 .. 100 ) {
	foreach my $y ( 50 .. 100 ) {
	    $f1->transparencySet( $x, $y, 1 );
	    $f1->update;
	}
    }

} # end photo1
