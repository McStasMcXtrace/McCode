# photo2.pl

use vars qw/ $TOP $photo2_use_bg /;
use strict;

sub photo2 {

    my( $demo ) = @_;

    my $alpha;			# transparency
    my( $dx, $dy ) = ( 0, 0 );	# delta offsets
    my $svar = 100;		# Scale variable
    $photo2_use_bg = 0;         # nz IFF a green background

    $TOP = $MW->WidgetDemo(
        -name             => $demo,
        -text             => [ "This demonstration illustrates compositing.  Commonly, each pixel of a color image is represented by three color components, red, green and blue, each specifiying a relative percentage of the color \"white\", such that, when the components are combined, they produce a visible color. Here are a few samples of 8-bit RGB triplets and the visible colors they produce:\n
(255, 255, 255) = white   (000, 000, 000) = black\n(255, 000, 000) = red      (000, 255, 000) = green   (000, 000, 255) = blue\n\nAn additional transparency factor can be supplied that describes the importance of each pixel's RGB components when composited over another image. This transparency channel is called the Alpha channel.  Although the Alpha channel is said to contain transparency information, in fact, it actually contains opacity information.\n\nIt is still possible to think in terms of transparency, although in this case the transparency is applied to the background image over which the current image is composited. The composited color is computed as:\n\n (image_RGB_component * alpha) + (background_RGB_component * (1.0 - alpha))", -wraplength => 640 ],
        -title            => 'Alpha Channel Compositing',
        -iconname         => 'photo2',
    );

    my $mw = $TOP;
    my $i1 = $mw->Photo( -file => Tk->findINC( 'demos/images/earth.gif' ) ); 
    my $i2 = $mw->Photo( -file => Tk->findINC( 'demos/images/earthris.gif' ) ); 
    my $i3 = $mw->Photo( qw/ -format png -width 320 -height 200 / );

    my $tf = $mw->Frame;
    $tf->Label( -image => $i2 )->pack( qw/ -side left / );
    $tf->Label( -image => $i1 )->pack( qw/ -side left / );

    my $bf = $mw->Frame;
    my $controls = $bf->Frame;
    my $results  = $bf->Frame;

    $controls->pack( qw/ -side left -fill both -expand 1/ ) ;
    $results->pack( qw/ -side right/ );

    $controls->Label(
        -font       => '9x15',
        -foreground => 'blue',
        -text       => 'Left Image Options',
    )->pack;
    my $s = $controls->Scale(
        qw/
        -from            100.0
        -resolution      10.0
        -tickinterval    10.0
        -to              0.0
        /,
        -label        => '% Opacity',
        -variable     => \$svar,
    );
    $svar = 80;

    my $spacer = $controls->Frame( qw/ -borderwidth 2 -relief solid -width 5/ );

    my $rt = $controls->Label( -text => 'Background' );
    my $r = $controls->Frame;
    my $r1 = $r->Radiobutton(
        -anchor   => 'w',
        -text     => 'Right Image',
        -value    => 0,
        -variable => \$photo2_use_bg,
        -width    => 20,
    );
    my $r2 = $r->Radiobutton(
        -anchor   => 'w',
        -text     => 'The Color Green',
        -value    => 1,
        -variable => \$photo2_use_bg,
        -width    => 20,
    );
    $r2->pack( qw/ -side bottom/ );
    $r1->pack( qw/ -side bottom/ );

    my $b = $controls->Button(
        -command => [ \&photo2_blend, $i1, $i2, $i3, $dx, $dy, \$svar ],
        -text    => 'Composite    ==>>',
    );

    my $l = $results->Label( -image => $i3 );

    $tf->pack;
    $bf->pack( qw/-fill both -expand 1/ ) ;
    $b->pack( qw/ -side bottom -fill x/ );
    $s->pack( qw/ -side left -fill both -expand 1 / );
    $spacer->pack( qw/ -side left -fill y -expand 1 / );
    $rt->pack( qw/ -side top / );
    $r->pack( qw/ -side right / );
    $l->pack( qw/ -side right / );

} # end photo2

sub photo2_blend {

    my( $img1, $img2, $img3, $dx, $dy, $svar_ref ) = @_;

    my $a2 = $$svar_ref / 100.0;
    my $a1 = 1.0 - $a2;

    my $width1  = $img1->width;
    my $height1 = $img1->height;
    my $width2  = $img2->width;
    my $height2 = $img2->height;

    $img3->blank;

    my $x1 = $dx;
    my $x2 = 0;

    for ( my $i = 0; $i < $width1;  $i++ ) {
	last if $i > $width2;
	my $y1 = $dy;
	my $y2 = 0;

	for ( my $j = 0; $j < $height1; $j++ ) {
	    last if $j > $height2;

	    # Skip if no pixel at this coordinate or if transparent. Else,
	    # combine the proper percentage of each color component and
	    # put the pixel into the third image.

	    Tk::catch {
		if ( not $img2->transparencyGet( $x2, $y2 ) ) {

		    my ( @c1 ) = $img1->get( $x1, $y1 );
		    my ( @c2 ) = $img2->get( $x2, $y2 );

		    # You can use a colored background rather than another
		    # image to test alpha channels.  Uncomment this line to
		    # ignore the first image and force a green backround.
		    @c1 = ( 0, 255, 0 ) if $photo2_use_bg;

		    foreach my $c ( 0 .. 2 ) {
			$c1[$c] = $c1[$c] * $a1 + $c2[$c] * $a2;
		    }
		    
		    my $color = sprintf( "#%02x%02x%02x", @c1[0 .. 2] );
		    $img3->put( $color, -to => $x1, $y1 );

		} # ifend
	    }; # catchend

	    $y1++;
	    $y2++;

	} # forend $j

	$img3->update;
	$x1++;
	$x2++;

    } # forend $i

} # end photo2_blend
