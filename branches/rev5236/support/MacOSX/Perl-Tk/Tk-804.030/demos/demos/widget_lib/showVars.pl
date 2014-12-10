

sub showVars {

    # Create a top-level window that displays a bunch of global variable values
    # and keeps the display up-to-date even when the variables change value.
    #
    # Arguments:
    #    w    - Perl widget reference of parent.
    #    vars -	Name of variable(s) to monitor.

    $showVars->destroy if Exists($showVars);
    $showVars = shift->Toplevel();
    my $w = $showVars;
    dpos($w);
    $w->title('Variable values');
    $w->iconname('Variables');

    my $w_title = $w->Label(-text => 'Variable values:', -width => 20, -anchor, 'center',
			     -font => '-Adobe-helvetica-medium-r-normal--*-180-*-*-*-*-*-*');
    $w_title->pack(-fill => 'x');
    my $i;
    foreach $i (@_) {
	my $w_i = $w->Frame();
	my $w_i_name = $w_i->Label(-text => "$i: ");
	my $w_i_value = $w_i->Label(-textvariable => \${$i});
	$w_i_name->pack(-side => 'left');
	$w_i_value->pack(-side => 'left');
	$w_i->pack(-side => 'top', -anchor => 'w');
    }
    $w->Button(-text => 'OK', -command => [$w => 'destroy'])->
      pack(-side => 'bottom', -pady => 2);

} # end showVars


1;
