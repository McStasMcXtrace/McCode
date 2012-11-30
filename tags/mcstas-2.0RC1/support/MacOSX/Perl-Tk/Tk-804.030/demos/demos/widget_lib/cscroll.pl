# cscroll.pl

use subs qw/cscroll_button cscroll_enter cscroll_leave/;
use vars qw/$TOP/;

sub cscroll {

    # Create a top-level window containing a simple canvas that can be
    # scrolled in two dimensions.

    my($demo) = @_;
    $TOP = $MW->WidgetDemo(
        -name     => $demo,
        -text     => 'This window displays a canvas widget that can be scrolled either using the scrollbars or by dragging with button 2 in the canvas.  If you click button 1 on one of the rectangles, its indices will be printed on stdout.',
        -title    => 'Scrollable Canvas Demonstration',
        -iconname => 'cscroll',
    );

    my $c = $TOP->Scrolled(qw/Canvas -relief sunken -borderwidth 2
        -scrollbars se -scrollregion/ => ['-10c', '-10c', '50c', '20c']);
    $c->pack(qw/-expand yes -fill both/);

    my($bg, $i, $j, $x, $y) = ($c->configure(-background))[4];
    for ($i = 0; $i < 20; $i++) {
	$x = -10 + 3 * $i;
	$j = 0;
	$y = -10;
	while ($j < 10) {
	    $c->createRectangle("${x}c", "${y}c",
		       ($x+2).'c', ($y+2).'c',
		       -outline => 'black', -fill => $bg, -tags => 'rect');
	    $c->createText(($x+1).'c', ($y+1).'c',
		       -text => "$i,$j", -anchor => 'center', -tags => 'text');
	    $j++;
	    $y += 3;
	} # whilend
    } # forend

    my $old_fill = '';
    $c->bind('all', '<Any-Enter>' => [\&cscroll_enter, \$old_fill]);
    $c->bind('all', '<Any-Leave>' => [\&cscroll_leave, \$old_fill]);
    $c->bind('all', '<1>' => \&cscroll_button);

    $c->CanvasBind('<2>' => [ scanMark => Ev('x'), Ev('y') ]);
    $c->CanvasBind('<B2-Motion>' => [ scanDragto => Ev('x'), Ev('y') ]);

} # end cscroll

sub cscroll_button {

    my($c) = @_;

    my ($id) = $c->find(qw/withtag current/);
    $id++ if ($c->gettags('current'))[0] ne 'text';
    print STDOUT 'You buttoned at ', ($c->itemconfigure($id, -text))[4], "\n";

} # end cscroll_button

sub cscroll_enter {

    my($c, $old_fill) = @_;

    my ($id) = $c->find(qw/withtag current/);
    $id-- if ($c->gettags('current'))[0] eq 'text';
    $$old_fill = ($c->itemconfigure($id, -fill))[4];
    if ($c->depth > 1) {
	$c->itemconfigure($id, -fill => 'SeaGreen1');
    } else {
	$c->itemconfigure($id, -fill => 'black');
	$c->itemconfigure($id+1, -fill => 'white');
    }

} # end cscroll_enter

sub cscroll_leave {

    my($c, $old_fill) = @_;

    my ($id) = $c->find(qw/withtag current/);
    $id-- if ($c->gettags('current'))[0] eq 'text';
    $c->itemconfigure($id, -fill => $$old_fill);
    $c->itemconfigure($id+1, -fill => 'black');

} # end cscroll_leave

1;
