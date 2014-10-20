# labels.pl

use vars qw/$TOP/;

sub labels {

    # Create a top-level window that displays a bunch of labels.  @pl is the
    # "packing list" variable which specifies the list of packer attributes.

    my($demo) = @_;
    $TOP = $MW->WidgetDemo(
        -name     => $demo,
        -text     => 'Five labels are displayed below: three textual ones on the left, and an image label and a text label on the right.  Labels are pretty boring because you can\'t do anything with them.',
        -title    => 'Label Demonstration',
        -iconname => 'label',
    );

    my(@pl) = qw/-side left -expand yes -padx 10 -pady 10 -fill both/;
    my $left = $TOP->Frame->pack(@pl);
    my $right = $TOP->Frame->pack(@pl);

    @pl = qw/-side top -expand yes -pady 2 -anchor w/;
    my $left_l1 = $left->Label(-text => 'First label')->pack(@pl);
    my $left_l2 = $left->Label(
        -text   => 'Second label, raised just for fun',
        -relief => 'raised',
    )->pack(@pl);
    my $left_l3 = $left->Label(
        -text   => 'Third label, sunken',
        -relief => 'sunken',
    )->pack(@pl);

    @pl = qw/-side top/;
    my $right_bitmap = $right->Label(
        -image       => $TOP->Photo(-file => Tk->findINC('Xcamel.gif')),
        -borderwidth => 2,
	-relief      => 'sunken',
    )->pack(@pl);
    my $right_caption = $right->Label(-text => 'Perl/Tk')->pack(@pl);

} # end labels

1;
