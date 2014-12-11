# check.pl

use vars qw/$TOP/;

sub check {

    # Create a top-level window that displays a bunch of check buttons.

    my($demo) = @_;
    $TOP = $MW->WidgetDemo(
        -name     => $demo,
        -text     => 'Three checkbuttons are displayed below.  If you click on a button, it will toggle the button\'s selection state and set a Perl variable to a value indicating the state of the checkbutton.  Click the "See Variables" button to see the current values of the variables.',
        -title    => 'Checkbutton Demonstration',
        -iconname => 'check',
    );

    my $var = $TOP->Button(
        -text    => 'See Variables',
        -command => [\&see_vars, $TOP, [
					['wipers', \$WIPERS],
					['brakes', \$BRAKES],
					['sober',  \$SOBER],
					],
		     ],
    );
    $var->pack(qw/-side bottom -expand 1/);

    my(@pl) = qw/-side top -pady 2 -anchor w/;
    my $b1 = $TOP->Checkbutton(
        -text     => 'Wipers OK',
        -variable => \$WIPERS,
	-relief   => 'flat')->pack(@pl);
    my $b2 = $TOP->Checkbutton(
        -text     => 'Brakes OK',
        -variable => \$BRAKES,
	-relief   => 'flat')->pack(@pl);
    my $b3 = $TOP->Checkbutton(
        -text     => 'Driver Sober',
        -variable => \$SOBER,
	-relief   => 'flat')->pack(@pl);

} # end check

1;
