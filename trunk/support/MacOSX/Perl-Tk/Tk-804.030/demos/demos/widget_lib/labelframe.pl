# labelframe.pl

use vars qw/$TOP/;

sub labelframe {

    # Create a top-level window that displays several Labelframe widgets.

    my($demo) = @_;
    $TOP = $MW->WidgetDemo(
        -geometry_manager => 'grid',
        -name             => $demo,
        -text             => 'Labelframes are used to group related widgets together. The label maybe either plain text or another widget.',
        -title            => 'Labelframe Demonstration',
        -iconname         => 'labelframe',
    );

    # A group of radiobuttons in a labelframe

    my $lf1 = $TOP->Labelframe(qw/-text Value -padx 2 -pady 2/);
    $lf1->grid(qw/-row 0 -column 0 -pady 2m -padx 2m/);

    my $lfdummy;
    foreach my $value (1 .. 4) {
	$lf1->Radiobutton(
            -text     => "This is value $value" ,
            -variable => \$lfdummy,
            -value    => $value,
        )->pack(qw/-side top -fill x -pady 2/);
    }

    # A label window controlling a group of options.

    my $lf2 = $TOP->Labelframe(qw/-pady 2 -padx 2/);
    $lf2->grid(qw/-row 0 -column 1 -pady 2m -padx 2m/);
    my $lfdummy2;
    my $cb;
    $cb = $lf2->Checkbutton(
        -text     => 'Use this option',
        -variable =>  \$lfdummy2,
        -command  => sub {&labelframe_buttons($lf2, $cb, \$lfdummy2)},
        -padx     => 0,
    );
    $lf2->configure(-labelwidget => $cb);

    foreach my $str (qw/Option1 Option2 Option3/) {
	$lf2->Checkbutton(-text => $str)->pack(qw/-side top -fill x -pady 2/);
    }

    &labelframe_buttons($lf2, $cb, \$lfdummy2);

    $TOP->gridColumnconfigure([0, 1], -weight => 1);

} # end labelframe

sub  labelframe_buttons {

    # The state of the sub-Checkbuttons is dependent upon the state of
    # the master -labelwidget Checkbutton.

    my ($lf, $cb, $var_ref) = @_;

    foreach my $child ($lf->children) {
        next if $child == $cb;
        if ($$var_ref) {
            $child->configure(qw/-state normal/);
        } else {
            $child->configure(qw/-state disabled/);
        }
    }

} # end labelframe_buttons

1;
