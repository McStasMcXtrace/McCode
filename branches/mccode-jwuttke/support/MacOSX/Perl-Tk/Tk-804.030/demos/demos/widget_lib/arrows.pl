# arrows.pl

use subs qw/arrow_err arrow_move1 arrow_move2 arrow_move3 arrow_setup/;
use vars qw/$TOP/;

sub arrows {

    # Create a top-level window containing a canvas demonstration that
    # allows the user to experiment with arrow shapes.

    my($demo) = @_;
    $TOP = $MW->WidgetDemo(
        -name     => $demo,
        -text     => ['This widget allows you to experiment with different widths and arrowhead shapes for lines in canvases.  To change the line width or the shape of the arrowhead, drag any of the three boxes attached to the oversized arrow.  The arrows on the right give examples at normal scale.  The text at the bottom shows the configuration options as you\'d enter them for a canvas line item.', qw/-wraplength 5i/],
        -title    => 'Arrowhead Editor Demonstration',
        -iconname => 'arrows',
    );

    my $c = $TOP->Canvas(
        -width       => '500',
        -height      => '350',
        -relief      => 'sunken',
	-borderwidth => 2,
    )->pack(qw/-expand yes -fill both/);

    my %ainfo;			# arrow information hash
    $ainfo{a} = 8;
    $ainfo{b} = 10;
    $ainfo{c} = 3;
    $ainfo{width} = 2;
    $ainfo{move_sub} = undef;
    $ainfo{x1} = 40;
    $ainfo{x2} = 350;
    $ainfo{'y'} = 150;
    $ainfo{smallTips} = [5, 5, 2];
    $ainfo{count} = 0;

    if ($TOP->depth > 1) {
	$ainfo{bigLineStyle} = [qw/-fill SkyBlue1/];
	$ainfo{boxStyle}     = [-fill => undef, qw/-outline black -width 1/];
	$ainfo{activeStyle}  = [qw/-fill red -outline black -width 1/];
    } else {
	$ainfo{bigLineStyle} = [
            -fill    => 'black',
            -stipple => '@'.Tk->findINC('demos/images/grey.25'),
        ];
	$ainfo{boxStyle}     = [-fill => undef, qw/-outline black -width 1/];
	$ainfo{activeStyle}  = [qw/-fill black -outline black -width 1/];
    }
    arrow_setup $c, \%ainfo;

    # Bindings to highlight the 3 tiny resize boxes.

    foreach ([qw/<Enter> activeStyle/], [qw/<Leave> boxStyle/]) {
        $c->bind('box', $_->[0] =>[
            sub {
		my($c, $style) = @_;
		$c->itemconfigure('current', @{$ainfo{$style}})
	    }, $_->[1]],
        );
    }
    $c->bind(qw/box <B1-Enter>/ => 'NoOp');
    $c->bind(qw/box <B1-Leave>/ => 'NoOp');

    # Bindings that select one of the 3 tiny resize boxes' "move code".

    my $n;
    for $n (1,2,3) {
	$c->bind("box${n}", '<1>' =>
            sub {$ainfo{move_sub} = \&{"arrow_move${n}"}}
        );
    }

    # Bindings to move a resize box and redraw the arrow.

    $c->bind('box', '<B1-Motion>' =>
        sub {&{$ainfo{move_sub}}($c, \%ainfo)}
    );
    $c->Tk::bind('<Any-ButtonRelease-1>' => [\&arrow_setup, \%ainfo]);

} # end arrows

sub arrow_err {

    my($c) = @_;

    my $i = $c->createText(qw/.6i .1i -anchor n -text/ => "Range error!");
    $c->after(4000, sub { $c->delete($i) });

} # end errow_err

sub arrow_move1 {

    my($c, $v) = @_;
    my $e = $c->XEvent;

    my($x, $y, $err) = ($e->x, $e->y, 0);
    my $newA = int(($v->{x2} + 5 - int($c->canvasx($x))) / 10);
    $newA = 0, $err = 1 if $newA < 0;
    $newA = 25, $err = 1 if $newA > 25;
    if ($newA != $v->{a}) {
	$c->move('box1', 10 * ($v->{a} - $newA), 0);
	$v->{a} = $newA;
    }
    arrow_err($c) if $err;

} # end arrow_move1

sub arrow_move2 {

    my($c, $v) = @_;
    my $e = $c->XEvent;

    my($x, $y, $errx, $erry) = ($e->x, $e->y, 0, 0);
    my $newB = int(($v->{x2} + 5 - int($c->canvasx($x))) / 10);
    $newB = 0, $errx = 1 if $newB < 0;
    $newB = 25, $errx = 1 if $newB > 25;
    my $newC = int(($v->{'y'} + 5 - int($c->canvasy($y)) - 5 * $v->{width})
		   / 10);
    $newC = 0, $erry = 1 if $newC < 0;
    $newC = 12, $erry = 1 if $newC > 12;
    if (($newB != $v->{b}) or ($newC != $v->{c})) {
	$c->move('box2', 10*($v->{b}-$newB), 10*($v->{c}-$newC));
	$v->{b} = $newB;
	$v->{c} = $newC;
    }
    arrow_err($c) if $errx or $erry;

} # end arrow_move2

sub arrow_move3 {

    my($c, $v) = @_;
    my $e = $c->XEvent;

    my($x, $y, $err) = ($e->x, $e->y, 0);
    my $newWidth = int(($v->{'y'} + 2 - int($c->canvasy($y))) / 5);
    $newWidth = 0, $err = 1 if $newWidth < 0;
    $newWidth = 20, $err = 1 if $newWidth > 20;
    if ($newWidth != $v->{width}) {
	$c->move('box3', 0, 5*($v->{width}-$newWidth));
	$v->{width} = $newWidth;
    }
    arrow_err($c) if $err;

} # end arrow_move3

sub arrow_setup {

    # The procedure below completely regenerates all the text and graphics in
    # the canvas window.  It's called when the canvas is initially created,
    # and also whenever any of the parameters of the arrow head are changed
    # interactively.  The argument is the name of the canvas widget to be
    # regenerated, and also the name of a global variable containing the
    # parameters for the display.

    my($c, $v) = @_;

    # Remember the current box, if there is one.

    my(@tags) = $c->gettags('current');
    my $cur = defined $tags[0] ? $tags[lsearch('box?', @tags)] : '';

    # Create the arrow and outline.

    $c->delete('all');
    $c->createLine($v->{x1}, $v->{'y'}, $v->{x2}, $v->{'y'},
	       -width => 10*$v->{width},
	       -arrowshape => [10*$v->{a}, 10*$v->{b}, 10*$v->{c}],
	       -arrow => 'last', @{$v->{bigLineStyle}});
    my $xtip = $v->{x2}-10*$v->{b};
    my $deltaY =  10*$v->{c}+5*$v->{width};
    $c->createLine($v->{x2}, $v->{'y'}, $xtip, $v->{'y'}+$deltaY,
	       $v->{x2}-10*$v->{a}, $v->{'y'}, $xtip, $v->{'y'}-$deltaY,
	       $v->{x2}, $v->{'y'}, -width => 2, -capstyle => 'round',
	       -joinstyle => 'round');

    # Create the boxes for reshaping the line and arrowhead.

    $c->createRectangle($v->{x2}-10*$v->{a}-5, $v->{'y'}-5,
	       $v->{x2}-10*$v->{a}+5, $v->{'y'}+5, @{$v->{boxStyle}},
	       -tags => ['box1', 'box']);
    $c->createRectangle($xtip-5, $v->{'y'}-$deltaY-5, $xtip+5,
	       $v->{'y'}-$deltaY+5, @{$v->{boxStyle}},
	       -tags => ['box2', 'box']);
    $c->createRectangle($v->{x1}-5, $v->{'y'}-5*$v->{width}-5,
	       $v->{x1}+5, $v->{'y'}-5*$v->{width}+5, @{$v->{boxStyle}},
	       -tags => ['box3', 'box']);

    # Create three arrows in actual size with the same parameters

    $c->createLine($v->{x2}+50, 0, $v->{x2}+50, 1000, -width => 2);
    my $tmp = $v->{x2}+100;
    $c->createLine($tmp, $v->{'y'}-125, $tmp, $v->{'y'}-75,
	       -width => $v->{width}, -arrow => 'both',
	       -arrowshape => [$v->{a}, $v->{b}, $v->{c}]);
    $c->createLine($tmp-25, $v->{'y'}, $tmp+25, $v->{'y'},
	       -width => $v->{width}, -arrow => 'both',
	       -arrowshape =>[$v->{a}, $v->{b}, $v->{c}]);
    $c->createLine($tmp-25, $v->{'y'}+75, $tmp+25, $v->{'y'}+125,
	       -width => $v->{width}, -arrow => 'both',
	       -arrowshape => [$v->{a}, $v->{b}, $v->{c}]);
    $c->itemconfigure($cur, @{$v->{activeStyle}}) if $cur =~ /box?/;

    # Create a bunch of other arrows and text items showing the current
    # dimensions.

    $tmp = $v->{x2}+10;
    $c->createLine($tmp, $v->{'y'}-5*$v->{width}, $tmp, $v->{'y'}-$deltaY,
	       -arrow => 'both', -arrowshape => $v->{smallTips});
    $c->createText($v->{x2}+15, $v->{'y'}-$deltaY+5*$v->{c},
	       -text => $v->{c}, -anchor => 'w');
    $tmp =  $v->{x1}-10;
    $c->createLine($tmp, $v->{'y'}-5*$v->{width}, $tmp,
	       $v->{'y'}+5*$v->{width}, -arrow => 'both',
	       -arrowshape => $v->{smallTips});
    $c->createText($v->{x1}-15, $v->{'y'}, -text => $v->{width},
	       -anchor => 'e');
    $tmp = $v->{'y'}+5*$v->{width}+10*$v->{c}+10;
    $c->createLine($v->{x2}-10*$v->{a}, $tmp, $v->{x2}, $tmp,
	       -arrow => 'both', -arrowshape => $v->{smallTips});
    $c->createText($v->{x2}-5*$v->{a}, $tmp+5, -text => $v->{a},
	       -anchor => 'n');
    $tmp = $tmp+25;
    $c->createLine($v->{x2}-10*$v->{b}, $tmp, $v->{x2}, $tmp,
	       -arrow => 'both', -arrowshape => $v->{smallTips});
    $c->createText($v->{x2}-5*$v->{b}, $tmp+5, -text => $v->{b},
	       -anchor => 'n');

    $c->createText($v->{x1}, 310, -text => "-width =>  $v->{width}",
	       -anchor => 'w',
	       -font => '-*-Helvetica-Medium-R-Normal--*-180-*-*-*-*-*-*');
    $c->createText($v->{x1}, 330,
	       -text => "-arrowshape =>  [$v->{a}, $v->{b}, $v->{c}]",
	       -anchor => 'w',
	       -font => '-*-Helvetica-Medium-R-Normal--*-180-*-*-*-*-*-*');

    $v->{count}++;

} # end arrow_setup

1;
