# trace1.pl

use Tk::widgets qw/  Trace /;
use vars qw/ $TOP /;
use strict;

sub trace1 {

    my( $demo ) = @_;

    $TOP = $MW->WidgetDemo(
        -name             => $demo,
        -text             => "This demonstration animates an analog display as you move the Scale's slider.",
        -title            => 'Move a meter tied to a variable',
        -iconname         => 'trace1',
    );

    my $mw = $TOP;
    my $v;			# variable to trace

    my $c = $mw->Canvas(qw/-width 200 -height 110 -bd 2 -relief sunken/)->grid;
    $c->createLine(qw/ 100 100 10 100  -tag meter -arrow last -width 5/);
    my $s = $mw->Scale(qw/-orient h -from 0 -to 100 -variable/ => \$v)->grid;
    $mw->Label(-text => 'Slide Me')->grid;

    # Trace $v when written.  The callback is supplied three explicit arguments:
    # the index if an array or hash, else undef, the proposed new value, and the
    # trace operation (rwu) for read, write, undef, respectively. Additionally,
    # we pass the Canvas and Scale widget references.

    $mw->traceVariable(\$v, 'w' => [\&trace1_update_meter, $c, $s]);

} # end trace1

sub trace1_update_meter {

    my( $index, $value, $op, $c, $s ) = @_;

    return if $op eq 'u';

    my($min, $max) = ($s->cget(-from), $s->cget(-to));
    my $pos = $value / abs($max - $min);
    my $pi = 3.1415926;
    my $x = 100.0 - 90.0 * (cos( $pos * $pi ));
    my $y = 100.0 - 90.0 * (sin( $pos * $pi ));
    $c->coords(qw/meter 100 100/, $x, $y);
    return $value;

 } # end trace1_update_meter
