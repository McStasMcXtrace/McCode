
package # hide from CPAN indexer
    Ball;

# Ball.pm, a class module that allows concurrent simulation (canvas) instances.
#
# This is simply a class module, nothing fancy like a derived widget or
# composite widget.  It has two virtual methods, new() and move_one_ball().
# There are two static methods, get_canvas_hash() and move_all_balls().
#
# Essentially, move_all_balls() is invoked to move all of the balls in a
# simulation's @BALLS list once - from their current to their new postion.
# After moving one ball a call to DoOneEvent() is made to handle pending
# XEvents.  The *user* of this  module, in this case bounce.pl, has their
# own main loop which also calls DoOneEvent() and move_all_balls() to keep
# the simulation active.
#
# Gurusamy Sarathy (gsar@engin.umich.edu)
# Tidied up by SOL.

use vars qw/$VERSION/;
$VERSION = '4.005'; # $Id: //depot/Tkutf8/demos/demos/widget_lib/Ball.pm#4 $

use Tk::Canvas;
use Tk::Widget;
use Tk qw/DoOneEvent DONT_WAIT/;
Construct Tk::Canvas 'Ball';
use strict;

# Class Ball global variables.

my %BALLS = ();			# hold @BALLS list on a per canvas basis
my (%DEFAULTS) = (		# Ball constructor option defaults
		  -color    => 'blue',
		  -size     => 20.0,
		  -position => [12.0,12.0],
		  -velocity => [6.0, 9.0],
		  );

sub new {			# Ball object constructor

    # Create a new Ball object, which just happens to be a Canvas item.
    # Fill-in values for defaulted parameters, create the oval item, and
    # store object-specific information in the ball's hash.
    #
    # Finally, update the class global %BALLS hash, indexed by a hashed canvas
    # reference, with the new ball.  Note the special Tk::bind statement that
    # removes a canvas from the %BALLS hash when the canvas is destroyed, thus
    # keeping %BALLS trimmed and preventing a very slow memory leak.

    my($class, $canvas, %args) = @_;

    my @missing_args = grep ! defined $args{$_}, keys %DEFAULTS;
    @args{@missing_args} = @DEFAULTS{@missing_args};
    my($color, $size, $pos, $vel) = @args{-color, -size, -position, -velocity};

    my $ball = $canvas->create('oval',
        ($pos->[0] - ($size/2.0)), ($pos->[1] - ($size/2.0)),
        ($pos->[0] + ($size/2.0)), ($pos->[1] + ($size/2.0)),
        -fill => $color,
    );
    $canvas->Tk::bind(
        '<Destroy>' => sub {delete $BALLS{Ball->get_canvas_hash($canvas)}}
    );

    my $ball_obj = {'canvas_ID' => $ball,
		    'canvas'    => $canvas,
		    'color'     => $color,
		    'size'      => $size,
		    'pos'       => [@$pos],
		    'vel'       => [@$vel],
                   };

    push @{$BALLS{Ball->get_canvas_hash($canvas)}->{'BALLS'}}, $ball_obj;
    return bless $ball_obj, $class;

} # end new, Ball constructor

sub get_canvas_hash {

    # Hash a canvas reference to a key for indexing into the %BALLS hash.
    # For now, just use the string-ified widget reference.  If this trick
    # were ever to fail in the future then only this code needs to be fixed
    # and the Ball class would be up and running in short oder.

    my($class, $canvas) = @_;

    return $canvas

} # end get_canvas_hash

sub move_one_ball {

    # Move one ball, belonging to one simulation, one clock tick.

    my ($ball_obj, $speed_ratio) = @_;

    my($ball, $canv, $minx, $miny, $maxx, $maxy);
    my($ballx, $bally, $deltax, $deltay);

    $speed_ratio = 1.0 unless defined $speed_ratio;
    $ball = $ball_obj->{'canvas_ID'};
    $canv = $ball_obj->{'canvas'};
    $ballx = $ball_obj->{'pos'}[0];
    $bally = $ball_obj->{'pos'}[1];

    $minx = $ball_obj->{'size'} / 2.0;
    $maxx = $ball_obj->{'canvas'}->cget(-width) - $minx;

    $miny = $ball_obj->{'size'} / 2.0;
    $maxy = $ball_obj->{'canvas'}->cget(-height) - $miny;

    if ($ballx > $maxx || $ballx < $minx) {
        $ball_obj->{'vel'}[0] = -1.0 * $ball_obj->{'vel'}[0];
    }
    if ($bally > $maxy || $bally < $miny) {
        $ball_obj->{'vel'}[1] = -1.0 * $ball_obj->{'vel'}[1];
    }

    $deltax = $ball_obj->{'vel'}[0] * $speed_ratio;
    $deltay = $ball_obj->{'vel'}[1] * $speed_ratio;

    $canv->move($ball, $deltax, $deltay);
    $ball_obj->{'pos'}[0] = $ballx + $deltax;
    $ball_obj->{'pos'}[1] = $bally + $deltay;

    return $ball_obj;

} # end move_one_ball

sub move_all_balls {

    # Move all the balls belong to one simulation instance one clock tick.

    my($class, $canvas, $speed_ratio) = @_;

    foreach (@{$BALLS{Ball->get_canvas_hash($canvas)}->{'BALLS'}}) {
        $_->move_one_ball($speed_ratio);
        DoOneEvent(DONT_WAIT);		# be kind and process XEvents if they arise
    }

} # end move_all_balls

1;
