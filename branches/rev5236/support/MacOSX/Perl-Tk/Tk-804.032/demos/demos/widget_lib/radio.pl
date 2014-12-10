# radio.pl

use Tk::widgets qw/LabFrame/;
use vars qw/$TOP/;

sub radio {

    # Create a top-level window that displays a bunch of radio buttons.

    my($demo) = @_;
    $TOP = $MW->WidgetDemo(
        -name     => $demo,
        -text     => ['Two groups of radiobuttons are displayed below.  If you click on a button then the button will become selected exclusively among all the buttons in its group.  A Perl variable is associated with each group to indicate which of the group\'s buttons is selected.  Click the "See Variables" button to see the current values of the variables.', qw/-wraplength 5i/],
        -title    => 'Radiobutton Demonstration',
        -iconname => 'radio',
    );

    my $var = $TOP->Button(
        -text    => 'See Variables',
        -command => [\&see_vars, $TOP, [
                                      ['point size', \$POINT_SIZE],
                                      ['color',      \$COLOR],
                                      ['alignment',  \$ALIGN],
				      ]
		     ],
    );
    $var->pack(qw/-side bottom -expand 1/);

    my @pl = qw/-side left -expand 1 -padx .5c -pady .5c/;
    my $left  = $TOP->LabFrame(-label => 'Point Size')->pack(@pl);
    my $mid   = $TOP->LabFrame(-label => 'Color')->pack(@pl);
    my $right = $TOP->LabFrame(-label => 'Alignment')->pack(@pl);

    @pl = qw/-side top -pady 2 -anchor w/;
    foreach my $p (10, 12, 18, 24) {
	$left->Radiobutton(
            -text     => "Point Size $p",
            -variable => \$POINT_SIZE,
            -relief   => 'flat',
            -value    => $p,
        )->pack(@pl);
    }

    foreach my $c (qw/Red Green Blue Yellow Orange Purple/) {
	$mid->Radiobutton(
            -text     => $c,
            -variable => \$COLOR,
            -relief   => 'flat',
            -value    => lc($c),
            -command  => sub {$mid->configure(-foreground => $c)},
        )->pack(@pl);
    }

    my $l = $right->Label(qw/-text Label -bitmap questhead -compound left/);
    $l->configure(-width  => $l->reqwidth, -compound => 'top');
    $l->configure(-height => $l->reqheight);
    my %w;
    foreach my $a (qw/Top Left Right Bottom/) {
	my $lower = lc $a;
	$w{$lower} = $right->Radiobutton(
            -text        => $a,
            -variable    => \$ALIGN,
	    -relief      => 'flat',
            -value       => $lower,
            -indicatoron => 0,
            -width       => 7,
	    -command     => sub {
		$l->configure(-compound => $ALIGN);
	    },
        );
    }
    Tk::grid('x', $w{'top'});
    $w{'left'}->grid($l, $w{'right'});
    Tk::grid('x', $w{'bottom'});

} # end radio

1;
