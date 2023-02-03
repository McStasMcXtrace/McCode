# Library of McStas gui functions
#
#   This file is part of the McStas neutron ray-trace simulation package
#   Copyright (C) 1997-2008, All rights reserved
#   Risoe National Laborartory, Roskilde, Denmark
#   Institut Laue Langevin, Grenoble, France
#
#   This program is free software; you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation; version 3 of the License.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
#
#   You should have received a copy of the GNU General Public License
#   along with this program; if not, write to the Free Software
#   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
#

use Tk;
use Tk::DialogBox;
use Tk::ROText;
use Tk::Listbox;
use Tk::DirTree;

# For calling mcplot properly in the case of Matlab backend
use Cwd;
use File::Basename;
# For copying files - 'Site' menu
use File::Copy;

# For handling backgrounding on unix vs. Win32...
use Config;

sub get_dir_name {
    my ($dlg, $default) = @_;
    my $oldgrab = $dlg->grabStatus;
    $dlg->grabRelease;
    my $f = $default ?
        $dlg->getSaveFile(-title => "Select output file name",
                          -initialfile => $default) :
        $dlg->getSaveFile(-title => "Select output file name");
    $dlg->grab if $oldgrab eq 'local';
    return $f;
}

sub select_dir {
    my ($start_dir) = @_;
    my $top = new MainWindow;
    $top->withdraw;
    
    my $t = $top->Toplevel;
    $t->title("Choose directory:");
    my $ok = 0; 
    
    my $f = $t->Frame->pack(-fill => "x", -side => "bottom");
    
    if($start_dir) {
	$curr_dir = $start_dir;
    } else {
	$curr_dir = Cwd::cwd();
    }
    
    my $d;
    $d = $t->Scrolled('DirTree',
		      -scrollbars => 'osoe',
		      -width => 35,
		      -height => 20,
		      -selectmode => 'browse',
		      -exportselection => 1,
		      -browsecmd => sub { $curr_dir = shift },
		      -command   => sub { $ok = 1 },
		      )->pack(-fill => "both", -expand => 1);
    $f->Button(-text => 'Ok',
	       -command => sub { $ok =  1 })->pack(-side => 'left');
    $f->Button(-text => 'Cancel',
	       -command => sub { $ok = -1 })->pack(-side => 'left');
    
    $f->waitVariable(\$ok);
    
    $top->destroy;
    if ($ok == 1) {
	return $curr_dir;
    } else {
	return $start_dir;
    }
}


# Query user for instrument parameters and simulation options for a
# McStas simulation.
# Input: top-level window for the dialog, instrument info descriptor, and
#        simulation info descriptor.
# Output: user action ("Start" or "Cancel") and new simulation info
#         descriptor.

my %typeabbrev = ('double' => "D", 'int' => "I", 'string' => "S");
my $typehelp = "(D=floating point, I=integer, S=string)";

sub simulation_dialog {
    my ($win, $ii, $origsi) = @_;
    my %si = $origsi ? %$origsi : ();
    if($origsi->{'Seed'} ne "" && $origsi->{'Seed'} ne 0) {
        $si{'Seed'} = $origsi->{'Seed'};
    } else {
        $si{'Seed'} = "";
    }
    $si{'Forcecompile'} = 0 unless $si{'Forcecompile'};
    $si{'Autoplot'}=0 unless $si{'Autoplot'};
    $si{'Ncount'}= 1e6 unless $si{'Ncount'};
    $si{'gravity'}=0 unless $si{'gravity'};
    $si{'GravityWarn'} = 0 unless $si{'GravityWarn'};
    $si{'Mode'}  = 0 unless $si{'Mode'};
    $si{'NScan'} = 0 unless $si{'NScan'};
    $si{'Detach'} = 0 unless $si{'Detach'};
    $si{'cluster'}= $MCSTAS::mcstas_config{'CLUSTER'};
    # 'Inspect' field for use of mcdisplay's built-in
    # neutron filter, filtering away all neutrons not
    # reaching a given component
    # PW 20030314
    $si{'Inspect'} = '' unless $si{'Inspect'};
    # Similarly, First and Last fields for selection of
    # component range to visualize..
    $si{'First'} = '' unless $si{'First'};
    $si{'Last'} = '' unless $si{'Last'};
    my $plotter = $MCSTAS::mcstas_config{'PLOTTER'};

    my $name_instr = $ii->{'Instrument-source'};
    my $dlg = $win->DialogBox(-title => "Run simulation $name_instr",
                              -buttons => ["Start", "Cancel"]);
    my $data = component_information($ii->{'Instrument-source'});
    my $top_frame = $dlg->Frame(-relief => 'raised', -border => 1);
    $b = $dlg->Balloon(-state => 'balloon');
    $top_frame->pack(-fill => 'x');
    $top_frame->Label(-text => "Instrument source: $ii->{'Instrument-source'}",
          -anchor => 'w',
          -justify => 'left',
          -fg => 'red')->pack(-side => 'left');
    my $genhtml = $top_frame->Button(-text => "HTML docs", -width => 11,
                -command => sub {mcdoc_current($win)} )->pack(-side => 'right');
    $b->attach($genhtml, -balloonmsg => "Generate documentation\nfor this instrument");
    # Set up the parameter input fields.
    my @parms = @{$ii->{'Parameters'}};
    my $numrows = int ((@parms + 2)/3);
    if($numrows > 0) {
        my $choiceparam = $dlg->add('Label',
                  -text => "Instrument parameters $typehelp:",
                  -anchor => 'w', -fg => 'blue',
                  -justify => 'left')->pack(-fill => 'x');
        $b->attach($choiceparam, -balloonmsg => "Specify instrument parameters\n ranges are 'MIN,MAX' for scans/optimization, \n or MIN,GUESS,VAL for optimization");
        my $parm_frame = $dlg->Frame;
        $parm_frame->pack(-fill => 'both');
        my $row = 0;
        my $col = 0;
        my $p;
        for $p (@parms) {
            # Give parameter type as abbrevation.
            my ($type, $u);
            $type = $ii->{'Parameter-types'}{$p};
            $u = " ($typeabbrev{$type})" if $type;
            $u = "" unless $u;
            my $w = $parm_frame->Label(-text => "$p$u:", -justify => 'right');
            $w->grid(-row => $row, -column => $col, -sticky => 'e');
            $col++;
            # add tooltip
            for $i (@{$data->{'inputpar'}}) {
              if ($i eq $p) {
                my $ballon;
                if(defined($data->{'parhelp'}{$i}{'default'})) {
                  $ballon = "<$i=$data->{'parhelp'}{$i}{'default'}>: ";
                } else {
                  $ballon = "<$i>: ";
                }
                $ballon .= "[$data->{'parhelp'}{$i}{'unit'}] "
                  if $data->{'parhelp'}{$i}{'unit'};

                $ballon .= "$data->{'parhelp'}{$i}{'text'} "
                  if $data->{'parhelp'}{$i}{'text'};

                $b->attach($w, -balloonmsg => $ballon);
              }
            }
            # entry text field
            $si{'Params'}{$p} = "" unless defined($si{'Params'}{$p});
            if ($si{'Params'}{$p} eq "" && defined($ii->{'Params'}{$p}) && not $ii->{'Params'}{$p} eq 'NULL')
            { $si{'Params'}{$p} = $ii->{'Params'}{$p}; }
            if ($typeabbrev{$type} eq "S" &&
            $si{'Params'}{$p} !~ /\".*\"/ &&
            $si{'Params'}{$p} !~ /\'.*\'/) {
              $si{'Params'}{$p} =~ s!\"!!g; # remove quotes
            }
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

    my $opt_frame = $dlg->Frame;
    $opt_frame->pack(-anchor => 'w', -fill => 'x');

    # ouput dir/force
    my $line = $opt_frame->Frame;
    $line->pack(-anchor => 'w', -fill => 'x');
    my $browsedir = $line->Label(-text => "Output to (dir):")->pack(-side => 'left');

    my $dir_entry = $line->Entry(-relief => 'sunken',
                               -width=>30,
                               -justify => 'left',
                               -textvariable => \$si{'Dir'});
    $dir_entry->pack(-side => 'left');

    $line->Button(-text => "Browse...", -width => 9,
                -command => sub { my $d = select_dir($si{'Dir'});
                                  $si{'Dir'} = $d if $d; } )->pack(-side => 'right');
    $b->attach($browsedir, -balloonmsg => "Select a directory where to store results\nLeave blank to save at instrument location");

    # ncounts/gravitation
    my $line = $opt_frame->Frame;
    $line->pack(-anchor => 'w', -fill => 'x');
    my $choicencount = $line->Label(-text => "Ray count:")->pack(-side => 'left');
    my $ncount_entry = $line->Entry(-relief => 'sunken',
                                  -width=>10,
                                  -justify => 'right',
                                  -textvariable => \$si{'Ncount'});
    $ncount_entry->pack(-side => 'left');
    $b->attach($choicencount, -balloonmsg => "Number of neutron events to generate\nKeep it reasonable for Trace/3D view (1e3)");
    my $gravity = $line->Checkbutton(-text => "gravity (BEWARE)", -variable => \$si{'gravity'})->pack(-side => 'left');
    $b->attach($gravity, -balloonmsg => "Activates gravitation between and inside components\nExtended (long) components must support gravitation (e.g. Guide_gravity)");

    # random seed (follow Ncount)
    my $seed_entry = $line->Label(-text => "    Random seed:")->pack(-side => 'left');
    $line->Entry(-relief => 'sunken',
                               -width=>4,
                               -textvariable => \$si{'Seed'},
                               -justify => 'right'
    )->pack(-side => 'left');
    $b->attach($seed_entry, -balloonmsg => "Seed value (for reproducible results).\nLeave blank for Random.");

    # execution mode
    my $line = $opt_frame->Frame;
    $line->pack(-anchor => 'w');
    my $choiceexec_val;
    if ($si{'Mode'} == 1)     { $choiceexec_val='Trace (3D view)'; }
    elsif ($si{'Mode'} == 0) { $choiceexec_val='Simulate'; }
    elsif ($si{'Mode'} == 2) { $choiceexec_val='Optimize Parameters'; }
    my $choices=[ 'Simulate', 'Trace (3D view)'];
    if ($MCSTAS::mcstas_config{'AMOEBA'}) {
      push @{ $choices }, 'Optimize Parameters';
    }
    if ($MCSTAS::mcstas_config{'AT'} ne 'no' && $Config{'osname'} ne 'MSWin32') {
	push @{ $choices }, 'Simulate (bg)';
	push @{ $choices }, 'Optimize (bg)';
    }
    my $choiceexec = $line->Optionmenu (
      -textvariable=>\$choiceexec_val,
      -options  => $choices,
      -command  => sub {
                      if ($choiceexec_val =~ /Trace/) {
                        $choicepnts->configure(-state=>'disabled');
                        $labelpnts->configure(-foreground=>'gray');
                        $choiceinspect->configure(-text=>'Inspect component: ',-foreground=>'black');
                        $choicefirst->configure(-foreground=>'black', -text=>'First component: ');
                        $choicelast->configure(-foreground=>'black', -text=>'Last component: ');
                      } elsif ($choiceexec_val =~ /Simulate/) {
                        $choicepnts->configure(-state=>'normal');
                        $labelpnts->configure(-foreground=>'black',-text=>'# steps');
                        $choiceinspect->configure(-foreground=>'gray');
                        $choicefirst->configure(-foreground=>'gray');
                        $choicelast->configure(-foreground=>'gray');

                      } elsif ($choiceexec_val =~ /Optimize/) {
                        $choicepnts->configure(-state=>'normal');
                        $labelpnts->configure(-foreground=>'black',-text=>'# optim');
                        $choiceinspect->configure(-foreground=>'black',-text=>'Maximize monitor: ');
                        $choicefirst->configure(-foreground=>'black',-text=>'Maximize monitor: ');
                        $choicelast->configure(-foreground=>'black',-text=>'Maximize monitor: ');
                      }
                   } ,
      -fg => 'blue'
    )->pack(-side => 'left');

    $b->attach($choiceexec, -balloonmsg => "Select execution mode");
    our $labelpnts = $line->Label(-text => "# steps: ")->pack(-side => 'left');
    $b->attach($labelpnts, -balloonmsg => "Number of scan steps\nsimulation repetitions\nor max optimization steps (20)");
    our $choicepnts = $line->Entry(-relief => 'sunken',
               -width=>4,
               -textvariable => \$si{'NScan'},
               -justify => 'right')->pack(-side => 'left');
    # output format (same line as exec mode)
    
    my $formatchoice = $line->Checkbutton(-text => "Plot results with: ",
					  -variable => \$si{'Autoplot'},
					  -relief => 'flat')->pack(-side => 'left');
    
    $b->attach($formatchoice, -balloonmsg => "Plot automatically result after simulation");
    
    if ($Config{'osname'} eq 'MSWin32') {
      $formatchoice-> configure ( -state => 'disabled' );
      $b->attach($formatchoice, -balloonmsg => "Plot automatically result after simulation (unix only)");
    }
    my $formatchoice_val;
    if ($plotter =~ /McStas|PGPLOT/i)  { $formatchoice_val= 'PGPLOT'; }
    if ($plotter =~ /Gnuplot/i)  { $formatchoice_val= 'Gnuplot'; }
    if ($plotter =~ /Matlab/i)  { $formatchoice_val= 'Matlab'; }
    if ($plotter =~ /HTML|VRML/) { $formatchoice_val='HTML/VRML'; }
    if ($plotter =~ /NeXus|HDF/) { $formatchoice_val='NeXus/HDF'; }
    if ($MCSTAS::mcstas_config{'NEXUS'} ne "") {
      $opts = ['PGPLOT','Gnuplot','Matlab','HTML/VRML','NeXus/HDF'];
    } else {
      $opts = ['PGPLOT','Gnuplot','Matlab','HTML/VRML'];
    }
    my $formatchoice_orig=$formatchoice_val;
    $formatchoice = $line->Optionmenu (
      -textvariable => \$formatchoice_val,
      -options      => $opts, -fg => 'blue'
    )->pack(-side => 'left');
    $b->attach($formatchoice, -balloonmsg => "Select format here or\nfrom File/Preferences menu item");

    # handle clustering methods
    my $line = $opt_frame->Frame;
    $line->pack(-anchor => 'w');
    our $choicecluster=$line->Label(-text => "Clustering:")->pack(-side => 'left');
    $b->attach($choicecluster, -balloonmsg => "Recommended methods:
* Single for normal machines
* MPI on clusters/multi-core machines");
    my $choicecluster_val;
    if ($si{'cluster'} == 0) { $choicecluster_val='None (single CPU)'; }
    elsif ($si{'cluster'} == 2) { $choicecluster_val='MPI (clusters)'; }
    elsif ($si{'cluster'} == 3) { $choicecluster_val='SSH (grid)'; }
    my $choicecluster_orig=$choicecluster_val;
    my $choices=[ 'None (single CPU)'];
    if ($MCSTAS::mcstas_config{'MPIRUN'} ne "no") {
      push @{ $choices }, 'MPI (clusters)';
    }
    if ($MCSTAS::mcstas_config{'SSH'} ne "no") {
      push @{ $choices }, 'SSH (grid)';
    }
    $choicecluster=$line->Optionmenu (
      -textvariable => \$choicecluster_val,
      -command      => sub {
                      if ($choicecluster_val =~ /None|Scans/) {
                        $mpinodes->configure(-state=>'disabled');
                        $labelnodes->configure(-foreground=>'gray');
                      } else {
                        $mpinodes->configure(-state=>'normal');
                        $labelnodes->configure(-foreground=>'black');
                      }
                   },
      -options      => $choices
    )->pack(-side => 'left');

    our $labelnodes=$line->Label(-text => "Number of nodes: ")->pack(-side => 'left');
    $b->attach($labelnodes, -balloonmsg => "Number of nodes to use\nfor Parallelisation");
    our $mpinodes = $line->Entry(-relief => 'sunken',
				 -width=>10,
				 -textvariable => \$MCSTAS::mcstas_config{'NODES'},
				 -justify => 'right')->pack(-side => 'left');
        
    # Gui stuff for selection of 'inspect' parameter
    # PW 20030314
    my $f4 = $opt_frame->Frame;
    $f4->pack(-anchor => 'w', -side => 'top', -fill => 'x');
    our $choiceinspect = $f4->Label(-text => "Inspect component: ", -height => '2',-foreground=>'gray'
    )->pack(-side => 'left');
    $b->attach($choiceinspect, -balloonmsg =>
"Trace mode: only show neutrons reaching selected component
Optimize Mode: signal 1 to maximize. Component MUST be a monitor");
    our($ListBox)=$f4->Scrolled('Listbox',-height => '1', -width => '40', -scrollbars => 'osoe', -exportselection => 'false')->pack(-side => 'right');
    # Selection of 'First' and 'Last' components to visualize
    my $f5 = $opt_frame->Frame;
    $f5->pack(-anchor => 'w', -side => 'top', -fill => 'x');
    our $choicefirst = $f5->Label(-text => "First component: ", -height => '2',-foreground=>'gray'
    )->pack(-side => 'left');
    $b->attach($choicefirst, -balloonmsg => "Trace mode: show instrument geometry FROM this component
Optimize Mode: signal 2 to maximize. Component MUST be a monitor");
    our($ListBoxFirst)=$f5->Scrolled('Listbox',-height => '1', -width => '40', -scrollbars => 'osoe', -exportselection => 'false')->pack(-side => 'right');
    my $f6 = $opt_frame->Frame;
    $f6->pack(-anchor => 'w', -side => 'top', -fill => 'x');
    our $choicelast = $f6->Label(-text => "Last component: ", -height => '2',-foreground=>'gray'
    )->pack(-side => 'left');
    $b->attach($choicelast, -balloonmsg => "Trace mode: show instrument geometry UP TO this component
Optimize Mode: signal 3 to maximize. Component MUST be a monitor");
    our($ListBoxLast)=$f6->Scrolled('Listbox',-height => '1', -width => '40', -scrollbars => 'osoe', -exportselection => 'false')->pack(-side => 'right');

    # fill listboxes
    my @data;
    @data=instrument_information($ii->{'Instrument-source'});
    foreach my $dat (@data) {
        $ListBox->insert('end', $dat);
        $ListBoxFirst->insert('end', $dat);
        $ListBoxLast->insert('end', $dat);
    }
    $ListBox->activate(0);
    $ListBoxFirst->activate(0);

    my $res = $dlg->Show;
    # Check value of ListBoxes -
    my ($index) = $ListBox->curselection();
    if ($index) {
        $si{'Inspect'} = $ListBox->get($index);
        $si{'InsNum'} = $index;
    }
    my ($indexFirst) = $ListBoxFirst->curselection();
    if ($indexFirst) {
        $si{'First'} = $ListBoxFirst->get($indexFirst);
    }
    my ($indexLast) = $ListBoxLast->curselection();
    if ($indexLast) {
        $si{'Last'} = $ListBoxLast->get($indexLast);
    }

    if ($res eq 'Start') {
      # update Plotter in case of change in this dialog (instead of Preferences)
      if ($formatchoice_val =~ /Matlab/i)    { $plotter= 'Matlab'; }
      elsif ($formatchoice_val =~ /Gnuplot/i)  { $plotter= 'Gnuplot'; }
      elsif ($formatchoice_val =~ /McStas|PGPLOT/i)  { $plotter= 'PGPLOT'; }
      elsif ($formatchoice_val =~ /HTML|VRML/i) { $plotter= 'HTML'; }
      elsif ($formatchoice_val =~ /NeXus|HDF/i) { $plotter= 'NeXus'; }

      if ($MCSTAS::mcstas_config{'PLOTTER'} =~ /scriptfile/i && $plotter =~ /|Matlab/i) { $plotter .= "_scriptfile"; }
      # finally set the PLOTTER
      $MCSTAS::mcstas_config{'PLOTTER'} = $plotter;
      
      $si{'Mode'} = do {
        if    ($choiceexec_val =~ /^Simulate/) { 0 }
        elsif ($choiceexec_val =~ /^Trace/)    { 1 }
        elsif ($choiceexec_val =~ /^Optimize/) { 2 }
      };
      # clustering methods do not apply with Trace mode.
      if ($si{'Mode'} == 1) { $choicecluster_val=0; }

      $si{'cluster'} = do {
        if    ($choicecluster_val =~ /^None/)   { 0 }
        elsif ($choicecluster_val =~ /^MPI/)    { 2 } # need recompile
        elsif ($choicecluster_val =~ /^SSH/)    { 3 }
      };

      if (($si{'cluster'} == 1 || $si{'cluster'} == 2) && $choicecluster_orig ne $choicecluster_val) {
        # if changing to MPI clustering, require re-compilation
        $si{'Forcecompile'} = 1;
      }
      if ($formatchoice_val ne $formatchoice_orig && $formatchoice_val =~ /NeXus|HDF/) {
        # if changing to NeXus, require re-compilation
        $si{'Forcecompile'} = 1;
      }

      if ($choiceexec_val =~ /\(bg\)/) { $si{'Detach'} = 1; } 
      else { $si{'Detach'} = 0; }
      
      # add quotes to string instr parameters if needed
      if ($quote) {
        for $p (@parms) {
          if ($ii->{'Parameter-types'}{$p} eq "string" &&
            $si{'Params'}{$p} !~ /\".*\"/ &&
            $si{'Params'}{$p} !~ /\'.*\'/) {
              # Firstly, remove existing quotes  :) 
                  $si{'Params'}{$p} =~ s!\"!!g;
                  $si{'Params'}{$p} =~ s!\'!!g;
                  # Next, add quotes...
                  $si{'Params'}{$p} = "\"$si{'Params'}{$p}\"";
            }
        }
      }
    }

    return ($res, \%si);
}

my $current_plot;

sub dialog_plot_single {
    my ($cl,$di) = @_;
    # Should only do something if we are using PGPLOT
    # PW 20030314
    my $plotter = $MCSTAS::mcstas_config{'PLOTTER'};
    if ($plotter =~ /PGPLOT|McStas/i) {
        $current_plot = $cl->index('active');
        single_plot($MCSTAS::mcstas_config{'PGDEV'}, $di->[$current_plot], 0);
    }
}

sub dialog_hardcopy {
    my ($dlg, $di, $type) = @_;
    # Should only be done if we are using PGPLOT
    # PW 20030314
    if ($MCSTAS::mcstas_config{'PLOTTER'} =~ /PGPLOT|McStas/i) {
        my $default = $current_plot == -1 ?
            "mcstas" : ($di->[$current_plot]{'Filename'});
        my $ext = $type eq "cps" ? "ps" : $type;
        $default .= ".$ext";
        my $oldgrab = $dlg->grabStatus;
        $dlg->grabRelease;

        my $f = $dlg->getSaveFile(-defaultextension => ".$ext",
                                  -title => "Select $ext output file name",
                                  -initialfile => $default);
        $dlg->grab if $oldgrab eq 'local';
        return 0 unless $f;
        if($current_plot == -1) {
            overview_plot("\"$f\"/$type", $di, 0);
        } else {
            my $comp = $di->[$current_plot]{'Filename'};
            single_plot("\"$f\"/$type", $di->[$current_plot], 0);
        }
    }
}

sub plot_dialog {
    my ($win, $ii, $si, $di, $sim_file_name) = @_;
    # Platform checks. Assumption: Either unix type os / Win32.
    my $prefix          = $MCSTAS::mcstas_config{'PREFIX'};
    my $suffix          = $MCSTAS::mcstas_config{'SUFFIX'};
    my $plotcmd         = $MCSTAS::mcstas_config{'PLOTCMD'};

    my @plot_cmd = ();
    if ($Config{'osname'} ne 'MSWin32') { # change spaces into \spaces
      $sim_file_name =~ s! !\ !g;
    }
    push @plot_cmd, "$plotcmd";
    push @plot_cmd, $sim_file_name;
    push @plot_cmd, "--format=$MCSTAS::mcstas_config{'PLOTTER'}";
    my $cmd=join(' ',@plot_cmd);
    spawn_external($w, $cmd);

}

sub preferences_dialog {
    # Choice of plotting backend
    # PW 20030314
    # Choice of internal editor
    # PW 20040527
    my ($win) = @_;
    my $dlg = $win->DialogBox(-title => "McStas: Preferences",
                              -buttons => ["OK"]);
    $b = $dlg->Balloon(-state => 'balloon');
    my $lf = $dlg->Frame(-borderwidth => 2, -relief => 'ridge');
    my $rf = $dlg->Frame(-borderwidth => 2, -relief => 'ridge');
    my $buttons, $edit_buttons;
    my $plotter = $MCSTAS::mcstas_config{'PLOTTER'};

    $lf->pack(-side => 'left', -fill => 'both');

    # data format choice
    my $formatchoice_val;
    my $plotopt = $lf->Label(-text => "Plotting options:", -anchor => 'w', -fg=>'blue'
    )->pack(-fill => 'x');
    $b->attach($plotopt, -balloonmsg => "Select output format/plotter");
    if ($plotter =~ /McStas|PGPLOT/i)  { $formatchoice_val= 'PGPLOT (original McStas)'; }
    elsif ($plotter =~ /Gnuplot/i) { $formatchoice_val= 'Gnuplot'; }
    elsif ($plotter =~ /Matlab/i) {
      $formatchoice_val= 'Matlab' . ($plotter =~ /scriptfile/ ?
        ' scriptfile' : ' (requires Matlab)');
    } elsif ($plotter =~ /HTML|VRML/i) {
      $formatchoice_val='HTML/VRML document';
    }  elsif ($plotter =~ /NeXus|HDF/i) {
      $formatchoice_val='NeXus/HDF file'; }
    if ($MCSTAS::mcstas_config{'NEXUS'} ne "") {
      $opts = ['PGPLOT (original McStas)',
        'Matlab (requires Matlab)', 'Matlab scriptfile',
        'HTML/VRML document','NeXus/HDF file'];
    } else {
      $opts = ['PGPLOT (original McStas)','Gnuplot',
        'Matlab (requires Matlab)', 'Matlab scriptfile',
        'HTML/VRML document'];
    }
    my $formatchoice = $lf->Optionmenu (
      -textvariable => \$formatchoice_val,
      -options      =>$opts
    )->pack(-fill => 'x');

    $pgmultiflag = $lf->Checkbutton(-text => "3-pane view with PGPLOT trace",
				     -relief => 'flat', -variable => \$MCSTAS::mcstas_config{'MCGUI_PGMULTI'})->pack(-fill => 'x');
    $b->attach($pgmultiflag, -balloonmsg => "Check to view 3 panes in PGPLOT mcdisplay");
    # handle clustering methods
    my $choicecluster=$lf->Label(-text => "Clustering:", -anchor => 'w', -fg=>'blue')->pack(-fill => 'x');
    my $choicecluster_val;
    if    ($MCSTAS::mcstas_config{'CLUSTER'} == 0) { $choicecluster_val='None (single CPU)'; }
    elsif ($MCSTAS::mcstas_config{'CLUSTER'} == 2) { $choicecluster_val='MPI (clusters)'; }
    elsif ($MCSTAS::mcstas_config{'CLUSTER'} == 3) { $choicecluster_val='SSH (grid)'; }
    my $choices=[ 'None (single CPU)'];
    if ($MCSTAS::mcstas_config{'MPIRUN'} ne "no") {
      push @{ $choices }, 'MPI (clusters)';
    }
    if ($MCSTAS::mcstas_config{'SSH'} ne "no") {
      push @{ $choices }, 'SSH (grid)';
    }
    $choicecluster=$lf->Optionmenu (
      -textvariable => \$choicecluster_val,
      -options      => $choices
    )->pack(-fill => 'x');
    
    $gridCflag = $lf->Checkbutton(-text => "Force compilation when gridding",
				     -relief => 'flat', -variable => \$MCSTAS::mcstas_config{'GRID_FORCECOMPILE'})->pack(-fill => 'x');
    $b->attach($gridCflag, -balloonmsg => "Required with heterogenous grids");
    
    $editor = $MCSTAS::mcstas_config{'EDITOR'};
    my $editorchoice_val;
    if ($editor == 0) { $editorchoice_val="Simple built-in editor"; }
    elsif ($editor == 1) { $editorchoice_val='Advanced built-in editor';}
    elsif ($editor == 2) { $editorchoice_val="External editor ($MCSTAS::mcstas_config{'EXTERNAL_EDITOR'})";}
    my $editorchoice = $lf->Label(-text => "Editor options:", -anchor => 'w',-fg=>'blue')->pack(-fill => 'x');
    $b->attach($editorchoice, -balloonmsg => "Select editor to use to\ndisplay instrument descriptions");
    $choices=["Simple built-in editor (McStas)","External editor ($MCSTAS::mcstas_config{'EXTERNAL_EDITOR'})"];
    if  ($MCSTAS::mcstas_config{'CODETEXT'} ne "no") {
      push @{ $choices }, 'Advanced built-in editor';
    }
    my $choiceeditor = $lf->Optionmenu (
      -textvariable => \$editorchoice_val,
      -options      => $choices
    )->pack(-fill => 'x');
    my $toolchoice = $lf->Label(-text => "GUI Palette", -anchor => 'w',-fg=>'blue')->pack(-fill => 'x');
    $lf->Entry(-relief => 'sunken',
		       -width=>16,
		       -textvariable => \$MCSTAS::mcstas_config{'TKPALETTE'},
		       -justify => 'right')->pack(-fill => 'x');
    my $toolchoice2 = $lf->Label(-text => "GUI Font", -anchor => 'w',-fg=>'blue')->pack(-fill => 'x');
    $lf->Entry(-relief => 'sunken',
		       -width=>16,
		       -textvariable => \$MCSTAS::mcstas_config{'TKFONT'},
		       -justify => 'right')->pack(-fill => 'x');
    $choicequote = $lf->Checkbutton(-text => "Surround strings with quotes",
               -relief => 'flat', -variable => \$quote)->pack(-fill => 'x');
    $b->attach($choicequote, -balloonmsg => "All string parameters will be surrounded with quotes\nThis option does not allow to pass variable names");
    if ($quote) { $choicequote->select; }
    
    # Tool-definitions
    my $toolchoice = $lf->Label(-text => "Runtime tool options:", -anchor => 'w',-fg=>'blue')->pack(-fill => 'x');
    $lf->Label(-text => "Execution/run command to use:", -anchor => 'n',)->pack(-fill => 'x');
    $lf->Entry(-relief => 'sunken',
		       -width=>16,
		       -textvariable => \$MCSTAS::mcstas_config{'RUNCMD'},
		       -justify => 'right')->pack(-fill => 'x');
    $lf->Label(-text => "Plot command to use:", -anchor => 'n',)->pack(-fill => 'x');
    $lf->Entry(-relief => 'sunken',
		       -width=>16,
		       -textvariable => \$MCSTAS::mcstas_config{'PLOTCMD'},
		       -justify => 'right')->pack(-fill => 'x');
    $lf->Label(-text => "Trace command to use:", -anchor => 'n',)->pack(-fill => 'x');
    $lf->Entry(-relief => 'sunken',
		       -width=>16,
		       -textvariable => \$MCSTAS::mcstas_config{'TRACECMD'},
		       -justify => 'right')->pack(-fill => 'x');

    $MCSTAS::mcstas_config{'CFLAGS_SAVED'} = $MCSTAS::mcstas_config{'CFLAGS'} unless $MCSTAS::mcstas_config{'CFLAGS_SAVED'};
    $MCSTAS::mcstas_config{'CC_SAVED'} = $MCSTAS::mcstas_config{'CC'} unless $MCSTAS::mcstas_config{'CC_SAVED'}; 
    $MCSTAS::mcstas_config{'MPICC_SAVED'} = $MCSTAS::mcstas_config{'MPICC'} unless $MCSTAS::mcstas_config{'MPICC_SAVED'};
    $MCSTAS::mcstas_config{'MPIRUN_SAVED'} = $MCSTAS::mcstas_config{'MPIRUN'} unless $MCSTAS::mcstas_config{'MPIRUN_SAVED'};
    my $compilchoice = $lf->Label(-text => "Compilation options:", -anchor => 'w',-fg=>'blue')->pack(-fill => 'x');
    $choicecflags = $lf->Checkbutton(-text => "Apply compiler flags: (define in textbox below)",
               -relief => 'flat', -variable => \$MCSTAS::mcstas_config{'MCGUI_CFLAGS'})->pack(-fill => 'x');
    $b->attach($choicecflags, -balloonmsg => "Check to compile slower but simulate faster");
    
    $cflags=$lf->Entry(-relief => 'sunken',
			   -width=>16,
			   -textvariable => \$MCSTAS::mcstas_config{'CFLAGS_SAVED'},
			   -justify => 'right')->pack(-fill => 'x');
    $lf->Label(-text => "Compiler to use:", -anchor => 'n',)->pack(-fill => 'x');
    $lf->Entry(-relief => 'sunken',
		       -width=>16,
		       -textvariable => \$MCSTAS::mcstas_config{'CC_SAVED'},
		       -justify => 'right')->pack(-fill => 'x');
    $lf->Label(-text => "MPI Compiler to use:", -anchor => 'n')->pack(-fill => 'x');
    $lf->Entry(-relief => 'sunken',
		       -width=>16,
		       -textvariable => \$MCSTAS::mcstas_config{'MPICC_SAVED'},
		       -justify => 'right')->pack(-fill => 'x');
    $lf->Label(-text => "MPIrun command to use:", -anchor => 'n')->pack(-fill => 'x');
    $lf->Entry(-relief => 'sunken',
		       -width=>16,
		       -textvariable => \$MCSTAS::mcstas_config{'MPIRUN_SAVED'},
		       -justify => 'right')->pack(-fill => 'x');
    my $precchoice = $lf->Label(-text => "Optimization options:", -anchor => 'w',-fg=>'blue')->pack(-fill => 'x');
    $labelprec = $lf->Label(-text => "Precision",
               -relief => 'flat')->pack(-side => 'left');
    $choiceprec=$lf->Entry(-relief => 'sunken',
	       -width=>8,
               -textvariable => \$MCSTAS::mcstas_config{'PREC'},
               -justify => 'right')->pack(-side => 'right');
    $b->attach($labelprec, -balloonmsg => "Determines final precision in optimizations.\nSee McStas manual for details");
    if ($MCSTAS::mcstas_config{'MCGUI_CFLAGS'}) { $choicecflags->select; }

    my $res = $dlg->Show;

    if ($formatchoice_val =~ /Matlab/i)    { $plotter= 'Matlab'; }
    elsif ($formatchoice_val =~ /Gnuplot/i)    { $plotter= 'Gnuplot'; }
    elsif ($formatchoice_val =~ /McStas|PGPLOT/i)  { $plotter= 'PGPLOT'; }
    elsif ($formatchoice_val =~ /HTML|VRML/i) { $plotter= 'HTML'; }
    elsif ($formatchoice_val =~ /NeXus|HDF/i) { $plotter= 'NeXus'; }

    if ($formatchoice_val =~ /scriptfile/i && $plotter =~ /Matlab/i) {
      $plotter .= "_scriptfile";
    }
    # finally set the PLOTTER
    $MCSTAS::mcstas_config{'PLOTTER'} = $plotter;

    $MCSTAS::mcstas_config{'CLUSTER'} = do {
      if     ($choicecluster_val =~ /^None/)   { 0 }
      elsif ($choicecluster_val =~ /^MPI/)    { 2 }
      elsif ($choicecluster_val =~ /^SSH/)  { 3 }
    };

    $MCSTAS::mcstas_config{'EDITOR'}  = do {
      if     ($editorchoice_val =~ /^Simple/)   { 0 }
      elsif ($editorchoice_val =~ /^Advanced/){ 1 }
      elsif ($editorchoice_val =~ /^External/)    { 2 }
    };

    if ($MCSTAS::mcstas_config{'MCGUI_CFLAGS'}) {
      $MCSTAS::mcstas_config{'CFLAGS'} = $MCSTAS::mcstas_config{'CFLAGS_SAVED'};
    } else {
      $MCSTAS::mcstas_config{'CFLAGS'} = "";
    }
    $MCSTAS::mcstas_config{'CC'} = $MCSTAS::mcstas_config{'CC_SAVED'};
    $MCSTAS::mcstas_config{'MPICC'} = $MCSTAS::mcstas_config{'MPICC_SAVED'};
    $MCSTAS::mcstas_config{'MPIRUN'} = $MCSTAS::mcstas_config{'MPIRUN_SAVED'};
    $MPIstuff = $MCSTAS::mcstas_config{'CLUSTER'};
    return ($res);
}


sub fetch_comp_info {
    my ($cname, $cinfo) = @_;
    unless($cinfo->{$cname}) {
        my $info = component_information($cname);
        return undef unless $info;
        $cinfo->{$cname} = $info;
    }
    return $cinfo->{$cname};
}

sub comp_instance_dialog {
    my ($w, $comp) = @_;
    my ($i, $j, $p);
    my $r = {
        'INSTANCE' => "",
        'DEFINITION' => "",
        'VALUE' => { },
        'OPTIONAL' => { },
        'AT' => { 'x' => "", 'y' => "", 'z' => "", 'relative' => "" },
        'ROTATED' => { 'x' => "", 'y' => "", 'z' => "", 'relative' => "" }
    };
    my $dlg = $w->Toplevel(-title => "$comp->{'name'}");
#    $dlg->transient($dlg->Parent->toplevel);
    $dlg->withdraw;
    # Add labels
    $dlg->Label(-text => "Component definition: $comp->{'name'}",
                -anchor => 'w')->pack(-fill => 'x');
    $r->{'DEFINITION'} = $comp->{'name'};
    $dlg->Label(-text => "$comp->{'identification'}{'short'}",
                -padx => 32, -anchor => 'w')->pack(-fill => 'x');
    my $f1 = $dlg->Frame();
    $f1->pack(-fill => 'x');
    $f1->Label(-text => "Author: $comp->{'identification'}{'author'}",
               -anchor => 'w', -justify => 'left')->pack(-side => 'left');
    $f1->Label(-text => "Date: $comp->{'identification'}{'date'}",
               -anchor => 'w', -justify => 'left')->pack(-side => 'right');
    $dlg->Label(-text => "Origin: $comp->{'identification'}{'origin'}",
                -anchor => 'w', -justify => 'left')->pack(-fill => 'x');
    my $f2 = $dlg->Frame();
    $f2->pack(-fill => 'x');
    $f2->Label(-text => "Instance name: ", -fg => 'blue',
               -anchor => 'w', -justify => 'left')->pack(-side => 'left');
    my $entry = $f2->Entry(-relief => 'sunken', -width => 20,
                           -textvariable => \$r->{'INSTANCE'},
                           -justify => 'left');
    $entry->pack(-side => 'left');
    $entry->focus;
    my $t = $dlg->Scrolled(qw/ROText -relief sunken -bd 2 -setgrid true
                           -height 18 -width 80 -scrollbars osoe/);
    $t->pack(-expand => 'yes', -fill => 'both');
    $t->tagConfigure('BLUE', -foreground => 'blue');
    $t->tagConfigure('RED', -foreground => 'red');

    $t->insert('end', "DESCRIPTION: (read it and fill-in PARAMETERS section below)\n\n", 'RED');
    $t->insert('end', $comp->{'description'});

    $t->insert('end', "PARAMETERS:\n  (optional parameters may be left blank, see DESCRIPTION section)\nCharacter type parameters usually require quoting, e.g. filename=\"name\"\n\n", 'RED');
    for $p (@{$comp->{'inputpar'}}) {
        $t->insert('end', "$p:", 'BLUE');
        my $entry = $t->Entry(-relief => 'sunken', -width => 10,
                              -textvariable => \$r->{'VALUE'}{$p},
                              -justify => 'right');
        $t->window('create', 'end', -window => $entry);
        my $unit = $comp->{'parhelp'}{$p}{'unit'};
        if($unit) {
            $t->insert('end', " [");
            $t->insert('end', $unit, 'RED');
            $t->insert('end', "]");
        }
        my $def = $comp->{'parhelp'}{$p}{'default'};
        if (defined($def)) {
          $t->insert('end', " (OPTIONAL, default $def)");
          $r->{'OPTIONAL'}{$p} = 1;
        } else { $r->{'OPTIONAL'}{$p} = 0; }
        $t->insert('end', "\n");
        $t->insert('end', $comp->{'parhelp'}{$p}{'text'})
            if $comp->{'parhelp'}{$p}{'text'};
        $t->insert('end', "\n\n");
    }
    $t->see("1.0");
    $t->markSet('insert', "1.0");

    my %delim = ('x' => ", ", 'y' => ", ", 'z' => "");
    for $j (qw/AT ROTATED/) {
        my $f3 = $dlg->Frame();
        $f3->pack(-fill => 'x');
        $f3->Label(-text => "$j", -fg => 'blue',
                   -anchor => 'w', -justify => 'left')->pack(-side => 'left');
        $f3->Label(-text => " (",
                   -anchor => 'w', -justify => 'left')->pack(-side => 'left');
        for $i (qw/x y z/) {
            my $entry = $f3->Entry(-relief => 'sunken', -width => 6,
                                   -textvariable => \$r->{$j}{$i},
                                   -justify => 'right');
            $entry->pack(-side => 'left');
            $f3->Label(-text => $delim{$i})->pack(-side => 'left');
        }
        $f3->Label(-text => ")  ")->pack(-side => 'left');
        $f3->Label(-text => "RELATIVE ",
                   -fg => 'blue')->pack(-side => 'left');
        my $entry2 = $f3->Entry(-relief => 'sunken', -width => 12,
                                -textvariable => \$r->{$j}{'relative'},
                                -justify => 'right');
        $entry2->pack(-side => 'left');
    }

    my $bot_frame = $dlg->Frame(-relief => "raised", -bd => 1);
    $bot_frame->pack(-side => "top", -fill => "both",
                     -ipady => 3, -ipadx => 3);
    my $selected;
    my $ok_cmd = sub { $selected = 'OK' };
    my $cancel_cmd = sub { $selected = 'CANCEL' };
    my $okbut = $bot_frame->Button(-text => "Ok", -command => $ok_cmd, -fg=>'GREEN');
    $okbut->pack(-side => "left", -expand => 1, -padx => 1, -pady => 1);
    my $cancelbut = $bot_frame->Button(-text => "Cancel",
                                       -command => $cancel_cmd, -fg=>'RED');
    $cancelbut->pack(-side => "left", -expand => 1, -padx => 1, -pady => 1);
    $dlg->protocol("WM_DELETE_WINDOW" => $cancel_cmd);
    $dlg->bind('<Escape>' => $cancel_cmd);
    $dlg->bind('<Return>' => $ok_cmd);

    my $old_focus = $dlg->focusSave;
    my $old_grab  = $dlg->grabSave;
    my $noexit    = 1;
    $dlg->Popup;
    $dlg->grab;
    while ($noexit) {
      $dlg->waitVariable(\$selected);
      $dlg->grabRelease;
      if ($selected eq 'OK') {
        my $r_at = $r->{'AT'};
        # Replace spaces in component instance name by underscores
        $r->{'INSTANCE'} =~ s!\ !_!g;
        if ($r->{'INSTANCE'} eq "") { # instance not defined !
            $dlg->messageBox(-message => "Instance name is not defined for component $comp->{'name'}. Please set it to a name of your own (e.g. My_Comp).",
                       -title => "$comp->{'name'}: No Instance Name",
                       -type => 'OK',
                       -icon => 'error');
            $selected = undef;
        } elsif ($r_at->{'x'} eq "" || $r_at->{'y'} eq "" || $r_at->{'z'} eq "" ) { # position not defined !
            $dlg->messageBox(-message => "Position AT is not defined for component $r->{'INSTANCE'} of type $comp->{'name'}. Please set the AT(x,y,z) values.",
                       -title => "$r->{'INSTANCE'}: No AT Position",
                       -type => 'OK',
                       -icon => 'error');
            $selected = undef;
        } elsif ($r_at->{'relative'} eq "") { # relative not defined !
            $dlg->messageBox(-message => "RELATIVE reference is not defined for component $r->{'INSTANCE'} of type $comp->{'name'}. Please set it to a component instance name (e.g. Origin, PREVIOUS or PREVIOUS(n).",
                       -title => "$r->{'INSTANCE'}: No Relative Reference",
                       -type => 'OK',
                       -icon => 'error');
            $selected = undef;
        } else { $noexit = 0; }
        # check of non optional parameters
        for $p (@{$comp->{'inputpar'}}) {
          if(!defined($r->{'VALUE'}{$p}) && $r->{'OPTIONAL'}{$p} == 0) {
            $dlg->messageBox(-message => "Parameter $p is left unset (non-optional). Please specify value.",
                       -title => "$r->{'INSTANCE'}:$p left unset",
                       -type => 'OK',
                       -icon => 'error');
            $selected = undef;
            $noexit = 1;
          }
        }
      } else { $noexit = 0; }
    }
    $dlg->destroy;
    &$old_focus;
    &$old_grab;
    return ($selected eq 'OK' ? $r : undef);
}

# Make a special Listbox class that invokes a user-specified callback
# each time an item is selected.
{
    package Tk::MyListbox;
    @Tk::MyListbox::ISA = qw/Tk::Listbox/; # Inherit from Tk::Listbox.
    Tk::Widget->Construct('MyListbox');
    sub ClassInit {
        my($cw, @args) = @_;
        $cw->SUPER::ClassInit(@args);
    }
    sub Populate {
        my($cw, @args) = @_;
        $cw->SUPER::Populate(@args);
    }
    my $selecthook;
    # Set the callback to invoke whenever an item is selected.
    sub selecthook {
        my ($w, $f) = @_;
        $selecthook = $f;
    }
    # Override the selectionSet() method, which gets called every time
    # a Listbox selection is made. This seems to be a private method,
    # hopefully it will not change in a later PerlTk version.
    sub selectionSet {
        my ($w,@args) = @_;
        my @r = $w->SUPER::selectionSet(@args);
        &$selecthook() if $selecthook; # Invoke user callback
        return @r;
    }
}

sub comp_select_dialog {
    my ($w, $clist, $cinfo) = @_;
    my $dlg = $w->Toplevel(-title => "Select component definition");
    $dlg->transient($dlg->Parent->toplevel);
    $dlg->withdraw;
    my $f = $dlg->Frame();
    $f->pack(-side => 'top');
    $f->Label(-text => "Available component definitions:", -fg=>'blue')->pack;
    my $list = $f->Scrolled('MyListbox', -width => 50, -height => 10,
                            -setgrid => 1, -scrollbars => 'osre');
    $list->pack(-expand => 'yes', -fill => 'y', -anchor => 'n');
    my @sorted = sort {compname($a) cmp compname($b)} @$clist;
    my @namelist = map compname($_), @sorted;
    $list->insert(0, @namelist);
    $list->activate(0);
    my $name = $f->Label(-text => "Name: ", -anchor => 'w', -fg=>'red');
    $name->pack(-fill => 'x');
    my $loc = $f->Label(-text => "Location: ", -anchor => 'w', -fg=>'red');
    $loc->pack(-fill => 'x');
    my $text = $f->Scrolled(qw/ROText -relief sunken -bd 2 -setgrid true
                            -height 10 -width 80 -scrollbars osoe/);
    $text->pack();
    $text->tagConfigure('SHORT', -foreground => 'blue');
    my $f1 = $f->Frame();
    $f1->pack(-fill => 'x');
    my $author = $f1->Label(-text => "Author: ",
                            -anchor => 'w', -justify => 'left', -fg=>'blue');
    $author->pack(-side => 'left');
    my $date = $f1->Label(-text => "Date: ",
                          -anchor => 'w', -justify => 'left', -fg=>'blue');
    $date->pack(-side => 'right');

    my $bot_frame = $dlg->Frame(-relief => "raised", -bd => 1);
    $bot_frame->pack(-side => "top", -fill => "both",
                     -ipady => 3, -ipadx => 3);
    my $selected;
    my $chosen_cmp;
    my $select_cmd = sub {
        # On some platforms, e.g. RedHat 9,
        # $list->curselection is not a scalar!
        my @sel = $list->curselection();
        my $cname = $sorted[$sel[0]];
        $chosen_cmp = $cname;
        my $info = fetch_comp_info($cname, $cinfo);
        $name->configure(-text => "Name: $info->{'name'}");
        $loc->configure(-text => "Location: $cname");
        $author->configure(-text =>
                           "Author: $info->{'identification'}{'author'}");
        $date->configure(-text => "Date: $info->{'identification'}{'date'}");
        $text->delete("1.0", "end");
        $text->insert("end", "$info->{'identification'}{'short'}\n\n","SHORT");
        $text->insert("end", $info->{'description'});
    };
    my $accept_cmd = sub { $selected = 'Ok'; };
    my $cancel_cmd = sub { $selected = 'Cancel'; };
    # Set up function to be called each time an item is selected.
    $list->selecthook($select_cmd);
    $list->bind('<Double-Button-1>' => $accept_cmd);
    $list->bind('<Return>' => $accept_cmd);
    my $okbut = $bot_frame->Button(-text => "Ok", -command => $accept_cmd, -fg=>'green');
    $okbut->pack(-side => "left", -expand => 1, -padx => 1, -pady => 1);
    my $cancelbut = $bot_frame->Button(-text => "Cancel",
                                       -command => $cancel_cmd, -fg=>'red');
    $cancelbut->pack(-side => "left", -expand => 1, -padx => 1, -pady => 1);
    $dlg->protocol("WM_DELETE_WINDOW" => $cancel_cmd);
    $dlg->bind('<Escape>' => $cancel_cmd);
    $dlg->bind('<Return>' => $accept_cmd);

    my $old_focus = $dlg->focusSave;
    my $old_grab = $dlg->grabSave;
    $list->focus;
    $dlg->Popup;
    $dlg->grab;
    $dlg->waitVariable(\$selected);
    my $selected_comp = ($selected eq 'Ok' ?
                         $chosen_cmp :
                         undef);
    $dlg->grabRelease;
    $dlg->destroy;
    &$old_focus;
    &$old_grab;
    return $selected_comp;
}

sub sitemenu_build {
    my ($w,$menu) = @_;
    my $sites;
    my $sitemenu = $menu->Menubutton(-text => $MCSTAS::mcstas_config{'PARTICLE'}.' site', -underline => 0);
    $sitemenu->pack(-side=>'left');

    # Scan each .instr file in the examples folder, find out which
    # site it belongs to...
    if (opendir(DIR,"$MCSTAS::sys_dir/examples/")) {
        my @instruments = readdir(DIR);
        closedir(DIR);
        next unless @instruments;
        @paths = map("$MCSTAS::sys_dir/examples/$_", grep(/\.(instr)$/, @instruments));
        @paths = sort @paths;
        my $j;
        my @added;   # Names of sites
        my @handles; # Menu handles
        my $CurrentSub;
        
        # SEARCH sites, read all instruments to get the list of sites
        for ($j=0 ; $j<@paths; $j++) {
            # What site is this one from?
            my $pid = open(READER,$paths[$j]);
            while(<READER>) {
                # Look for %INSTRUMENT_SITE:
                if (/%INSTRUMENT_SITE:\s*(\w*)/) {
                    # Check if that menu has been added?
                    my $k;
                    my $taken = 0;
                    for ($k=0; $k<@added; $k++) {
                        if ($added[$k] eq $1) { # found existing site sub-menu
                            $taken = 1;
                        }
                    }
                    if ($taken == 0) {          # add new site sub-menu
                        push @added, $1;
                    }
                }
            } # end while READER
        }
        
        # SORT site items and build sub-menus
        @added = sort @added;
        # build sub-menus
        for ($k=0; $k<@added; $k++) {
          $CurrentSub = $sitemenu->cascade(-label => $added[$k]);
          push @sites, $CurrentSub;
        }
        # add last item for Undefined site instruments
        push @added, "Undefined site";
        $UndefSite = $sitemenu->cascade(-label => "Undefined site");
        push @sites, $UndefSite; # will be last site item

        # STORE instruments 
        for ($j=0 ; $j<@paths; $j++) {
            # What site is this one from?
            my $pid = open(READER,$paths[$j]);
            my $cname="";  # real name of the instrument (DEFINE)
            my $site_tag=0;
            my $index=@added;
            while(<READER>) {
                # Look for real instrument name
                if (m!DEFINE\s+INSTRUMENT\s+([a-zA-Z0-9_]+)\s*(.*)!i) {
                  $cname = $1;
                }
                # Look for %INSTRUMENT_SITE:
                if (/%INSTRUMENT_SITE:\s*(\w*)/) {
                    # Check if that menu has been added?
                    my $k;
                    for ($k=0; $k<@added; $k++) {
                        if ($added[$k] eq $1) { # found existing site sub-menu
                            $index = $k;
                            $site_tag=1;
                        }
                    }
                }
            } # end while
	    if (!($index==@added)) {
	      # Add the instrument to the given menu.
	      my ($base, $dirname, $suffix);
	      ($base, $dirname, $suffix) = fileparse($paths[$j],".instr");
	      if ($cname ne "" && $cname ne $base) { $base = "$base ($cname)"; }
	      $CurrentSub = $sites[$index];
	      $CurrentSub->command(-label => "$base", 
				   -command => [ sub { sitemenu_runsub(@_)}, $paths[$j], $w]);
	    } else {
	      # Add the instrument to the given menu.
	      my ($base, $dirname, $suffix);
	      ($base, $dirname, $suffix) = fileparse($paths[$j],".instr");
	      if ($cname ne "" && $cname ne $base) { $base = "$base ($cname)"; }
	      $UndefSite->command(-label => "$base", 
				   -command => [ sub { sitemenu_runsub(@_)}, $paths[$j], $w]);
	    }
        }
    }
}

sub sitemenu_runsub {
    my ($path, $w) = @_;
    # Copy example instrument to current folder...
    my ($base, $dirname, $suffix) = fileparse($path,".instr");
    if (! copy("$path","./$base$suffix")) {
        $w->messageBox(-message => "Could not copy $base$suffix to .",
                       -title => "Select failed",
                       -type => 'OK',
                       -icon => 'error');
        return 0;
    }
    if (-e "./$base$suffix") {
  open_instr_def($w,"./$base$suffix");
    }
}

1;
