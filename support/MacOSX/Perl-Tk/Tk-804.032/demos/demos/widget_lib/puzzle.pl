# puzzle.pl

use subs qw/puzzle_switch/;
use vars qw/$TOP/;

sub puzzle {

    # Create a top-level window containing a 15-puzzle game.

    my($demo) = @_;
    $TOP = $MW->WidgetDemo(
        -name     => $demo,
        -text     => 'A 15-puzzle appears below as a collection of buttons.  Click on any of the pieces next to the space, and that piece will slide over the space.  Continue this until the pieces are arranged in numerical order from upper-left to lower-right.',
        -title    => '15-Puzzle Demonstration',
        -iconname => 'puzzle',
    );

    # Special trick: select a darker color for the space by creating a
    # scrollbar widget and using its trough color.

    my $s = $TOP->Scrollbar;
    my $frame = $TOP->Frame(
        -width       => 120,
        -height      => 120,
        -borderwidth => '2',
        -relief      => 'sunken',
        -background  => $s->cget(-troughcolor),
    );
    $frame->pack(qw/-side top -padx 1c -pady 1c/);
    $s->destroy;

    my(@order) = (3, 1, 6, 2, 5, 7, 15, 13, 4, 11, 8, 9, 14, 10, 12);
    my %xpos = ();
    my %ypos = ();

    my($i, $num, $frame_num);
    for ($i=0; $i<15; $i++) {
	$num = $order[$i];
	$xpos{$num} = ($i%4) * 0.25;
	$ypos{$num} = (int($i/4)) * 0.25;
	$frame_num = $frame->Button(
            -relief             => 'raised',
            -text               => $num,
            -highlightthickness => 0,
        );
	$frame_num->configure(
            -command => [\&puzzle_switch, $frame_num, $num, \%xpos, \%ypos],
        );
	$frame_num->place(
            -relx      => $xpos{$num},
            -rely      => $ypos{$num},
            -relwidth  => 0.25,
	    -relheight => 0.25,
        );
    } # forend all puzzle numbers
    $xpos{'space'} = 0.75;
    $ypos{'space'} = 0.75;

} # end puzzle

sub puzzle_switch {

    # Procedure invoked by buttons in the puzzle to resize the puzzle entries.

    my($w, $num, $xpos, $ypos) = @_;

    if (    (($ypos->{$num} >= ($ypos->{'space'} - 0.01)) &&
	     ($ypos->{$num} <= ($ypos->{'space'} + 0.01))
         &&  ($xpos->{$num} >= ($xpos->{'space'} - 0.26)) &&
	     ($xpos->{$num} <= ($xpos->{'space'} + 0.26)))
	 || (($xpos->{$num} >= ($xpos->{'space'} - 0.01)) &&
	     ($xpos->{$num} <= ($xpos->{'space'} + 0.01))
	 &&  ($ypos->{$num} >= ($ypos->{'space'} - 0.26)) &&
	     ($ypos->{$num} <= ($ypos->{'space'} + 0.26))) ) {
	my $tmp = $xpos->{'space'};
	$xpos->{'space'} = $xpos->{$num};
	$xpos->{$num} = $tmp;
	$tmp = $ypos->{'space'};
	$ypos->{'space'} =  $ypos->{$num};
	$ypos->{$num} = $tmp;
	$w->place(-relx => $xpos->{$num}, -rely => $ypos->{$num});
    }

} # end puzzle_switch

1;
