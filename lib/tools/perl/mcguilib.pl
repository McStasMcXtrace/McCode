use Tk;
use Tk::DialogBox;

# Query user for instrument parameters and simulation options for a
# McStas simulation.
# Input: top-level window for the dialog, instrument info descriptor, and
#        simulation info descriptor.
# Output: user action ("Start" or "Cancel") and new simulation info
#         descriptor.

sub simulation_dialog {
    my ($win, $ii, $origsi) = @_;
    my %si = %$origsi;
    my $doseed;
    if($origsi->{'Seed'}) {
	$si{'Seed'} = $origsi->{'Seed'};
	$doseed = 1;
    } else {
	$si{'Seed'} = "";
	$doseed = 0;
    }
    $si{'Autoplot'} = 0 unless $si{'Autoplot'};

    my $dlg = $win->DialogBox(-title => "Run simulation",
			      -buttons => ["Start", "Cancel"]);

    $dlg->add('Label',
	      -text => 'Instrument parameters:',
	      -anchor => 'w',
	      -justify => 'left')->pack(-fill => 'x');

    # Set up the parameter input fields.
    my $parm_frame = $dlg->add('Frame');
    $parm_frame->pack;
    my @parms = @{$ii->{'Parameters'}};
    my $numrows = int ((@parms + 2)/3);
    if($numrows > 0) {
	my $row = 0;
	my $col = 0;
	my $p;
	for $p (@parms) {
	    my $w = $parm_frame->Label(-text => "$p:", -justify => 'right');
	    $w->grid(-row => $row, -column => $col, -sticky => 'e');
	    $col++;
	    $si{'Params'}{$p} = "" unless defined($si{'Params'}{$p});
	    $w = $parm_frame->Entry(-relief => 'sunken',
				    -width=>10,
				    -textvariable => \$si{'Params'}{$p},
				    -justify => 'right');
	    $w->grid(-row => $row, -column => $col);
	    $col++;
	    if($col >= 6) {
		$col = 0;
		$row++;
	    }
	}
    }

    my $opt_frame = $dlg->add('Frame');
    $opt_frame->pack(-anchor => 'w');
    my $f1 = $opt_frame->Frame;
    $f1->pack(-anchor => 'w');
    $f1->Label(-text => "Neutron count:")->pack(-side => 'left');
    my $ncount_entry = $f1->Entry(-relief => 'sunken',
				  -width=>10,
				  -justify => 'right',
				  -textvariable => \$si{'Ncount'});
    $ncount_entry->pack(-side => 'left');
    $opt_frame->Checkbutton(-text => "Plot results",
			    -variable => \$si{'Autoplot'},
			    -relief => 'flat')->pack(-anchor => 'w');
    my $f2 = $opt_frame->Frame;
    $f2->pack(-anchor => 'w');
    $f2->Radiobutton(-text => "Random seed",
		     -variable => \$doseed,
		     -relief => 'flat',
		     -value => 0)->pack(-side => 'left');
    $f2->Radiobutton(-text => "Set seed to",
		     -variable => \$doseed,
		     -relief => 'flat',
		     -value => 1)->pack(-side => 'left');
    $ncount_entry = $f2->Entry(-relief => 'sunken',
			       -width=>10,
			       -textvariable => \$si{'Seed'},
			       -justify => 'right');
    $ncount_entry->pack(-side => 'left');

    my $f3 = $opt_frame->Frame;
    $f3->pack(-anchor => 'w');
    $f3->Radiobutton(-text => "Simulate",
		     -variable => \$si{'Trace'},
		     -relief => 'flat',
		     -value => 0)->pack(-side => 'left');
    $f3->Radiobutton(-text => "Trace",
		     -variable => \$si{'Trace'},
		     -relief => 'flat',
		     -value => 1)->pack(-side => 'left');

    my $res = $dlg->Show;
    $si{'Seed'} = 0 unless $doseed;
    return ($res, \%si);
}

1;
