# Library of McStas gui functions
#
#   This file is part of the McStas neutron ray-trace simulation package
#   Copyright (C) 1997-2006, All rights reserved
#   Risoe National Laborartory, Roskilde, Denmark
#   Institut Laue Langevin, Grenoble, France
#
#   This program is free software; you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation; either version 2 of the License, or
#   (at your option) any later version.
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
# For calling mcplot properly in the case of Matlab/Scilab backend
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
    my $doseed;
    if($origsi->{'Seed'}) {
        $si{'Seed'} = $origsi->{'Seed'};
        $doseed = 1;
    } else {
        $si{'Seed'} = "";
        $doseed = 0;
    }
    $si{'Autoplot'} = 0 unless $si{'Autoplot'};
    $si{'Ncount'} = 1e6 unless $si{'Ncount'};
    $si{'gravity'}   = 0 unless $si{'gravity'};
    $si{'GravityWarn'}   = 0 unless $si{'GravityWarn'};
    $si{'Trace'} = 0 unless $si{'Trace'};
    $si{'NScan'} = 0 unless $si{'NScan'};
    $si{'Multi'} = 0 unless $si{'Multi'};
    $si{'Force'} = 0 unless $si{'Force'};
    $si{'mpi'}   = 0 unless $si{'mpi'};
    # 'Inspect' field for use of mcdisplay's built-in
    # neutron filter, filtering away all neutrons not
    # reaching a given component
    # PW 20030314
    $si{'Inspect'} = '' unless $si{'Inspect'};
    $si{'InsNum'} = 0 unless $si{'InsNum'};
    # Similarly, First and Last fields for selection of
    # component range to visualize..
    $si{'First'} = '' unless $si{'First'};
    $si{'Last'} = '' unless $si{'Last'};
    my $plotter = $MCSTAS::mcstas_config{'PLOTTER'};

    if    ($plotter =~ /PGPLOT|McStas/i) { $si{'Format'} = 0; }
    elsif ($plotter =~ /Matlab/i)     { $si{'Format'} = 1; }
    elsif ($plotter =~ /Scilab/i)     { $si{'Format'} = 2; }
    elsif ($plotter =~ /HTML/i)       { $si{'Format'} = 3; }

    my $name_instr = $ii->{'Instrument-source'};
    my $dlg = $win->DialogBox(-title => "Run simulation $name_instr",
                              -buttons => ["Start", "Cancel"]);
    my $data = component_information($ii->{'Instrument-source'});
    my $top_frame = $dlg->Frame(-relief => 'raised', -border => 1);
    $b = $dlg->Balloon(-state => 'balloon');
    $top_frame->pack(-fill => 'x');
    $top_frame->Label(-text => "Instrument source: $ii->{'Instrument-source'}",
          -anchor => 'w',
          -justify => 'left')->pack(-side => 'left');
    my $genhtml = $top_frame->Button(-text => "HTML docs", -width => 11,
                -command => sub {mcdoc_current($win)} )->pack(-side => 'right');
    $b->attach($genhtml, -balloonmsg => "Generate documentation\nfor this instrument");
    # Set up the parameter input fields.
    my @parms = @{$ii->{'Parameters'}};
    my $numrows = int ((@parms + 2)/3);
    if($numrows > 0) {
        my $choiceparam = $dlg->add('Label',
                  -text => "Instrument parameters $typehelp:",
                  -anchor => 'w',
                  -justify => 'left')->pack(-fill => 'x');
        $b->attach($choiceparam, -balloonmsg => "Specify instrument parameters\nscan ranges are 'MIN,MAX'");
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
            if ($si{'Params'}{$p} eq "" && defined($ii->{'Params'}{$p}))
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

    my $f0 = $opt_frame->Frame;
    $f0->pack(-anchor => 'w', -fill => 'x');
    my $browsedir = $f0->Label(-text => "Output to (dir):")->pack(-side => 'left');

    my $dir_entry = $f0->Entry(-relief => 'sunken',
                               -width=>30,
                               -justify => 'left',
                               -textvariable => \$si{'Dir'});
    $dir_entry->pack(-side => 'left');
    my $choiceforce = $f0->Checkbutton(-text => "force",-variable => \$si{'Force'})->pack(-side => 'left');
    $b->attach($choiceforce, -balloonmsg => "Force to overwrite existing directories");
    $f0->Button(-text => "Browse ...", -width => 11,
                -command => sub { my $d = get_dir_name($dlg, $si{'Dir'});
                                  $si{'Dir'} = $d if $d; } )->pack(-side => 'right');
    $b->attach($browsedir, -balloonmsg => "Select a directory where to store results\nLeave blank to save at instrument location");
    my $f1 = $opt_frame->Frame;
    $f1->pack(-anchor => 'w');
    my $choicencount = $f1->Label(-text => "Neutron count:")->pack(-side => 'left');
    my $ncount_entry = $f1->Entry(-relief => 'sunken',
                                  -width=>10,
                                  -justify => 'right',
                                  -textvariable => \$si{'Ncount'});
    $ncount_entry->pack(-side => 'left');
    $b->attach($choicencount, -balloonmsg => "Number of neutron events to generate\nKeep it reasonable for Trace/3D view (1e6)");
    my $gravity = $f1->Checkbutton(-text => "gravity (BEWARE)", -variable => \$si{'gravity'})->pack(-side => 'left');
    $b->attach($gravity, -balloonmsg => "Activates gravitation between and inside components\nExtended component must support gravitation (e.g. Guide_gravity)");
    if ($MCSTAS::mcstas_config{'HOSTFILE'} ne "") {
      if ($si{'mpi'} > 0) {
        my $mpinodes=$f1->Label(-text => "# MPI nodes: ")->pack(-side => 'left');
        $f1->Entry(-relief => 'sunken',
               -width=>10,
               -textvariable => \$si{'mpi'},
               -justify => 'right')->pack(-side => 'left');
        $b->attach($mpinodes, -balloonmsg => "Parallelisation");
      }
      if ($si{'ssh'} > 0) {
        my $sshnodes=$f1->Checkbutton(-text => "Distribute mcrun scans (grid)",
          -variable => \$si{'Multi'},
          -relief => 'flat')->pack(-anchor => 'w');
        $b->attach($sshnodes, -balloonmsg => "Scan step are distributed among a list of machines");
      }
    }
    my $ff1 = $opt_frame->Frame;
    $ff1->pack(-anchor => 'w');
    my $formatchoice = $ff1->Checkbutton(-text => "Plot results, Format: ",
                            -variable => \$si{'Autoplot'},
                            -relief => 'flat')->pack(-side => 'left');
    $b->attach($formatchoice, -balloonmsg => "Plot automatically result after simulation\nSelect format here or from Simulation/Configuration menu item");
    $ff1->Optionmenu (
      -textvariable => \$plotter,
      -variable     => \$si{'Format'},
      -options      => [
                        ['PGPLOT', 0 ],
                        ['Matlab', 1 ],
                        ['Scilab', 2 ],
                        ['HTML/VRML', 3]
                       ]
    )->pack();
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
    my $choice3d = $f3->Radiobutton(-text => "Trace (3D View)",
                     -variable => \$si{'Trace'},
                     -relief => 'flat',
                     -value => 1)->pack(-side => 'left');
    $b->attach($choice3d, -balloonmsg => "Draw instrument geometry (3D view)");
    my $choicesim = $f3->Radiobutton(-text => "Simulate",
                     -variable => \$si{'Trace'},
                     -relief => 'flat',
                     -value => 0)->pack(-side => 'left');
    $b->attach($choicesim, -balloonmsg => "Simulation mode");
    my $choicepnts = $f3->Label(-text => "# Scanpoints: ")->pack(-side => 'left');
    $f3->Entry(-relief => 'sunken',
               -width=>10,
               -textvariable => \$si{'NScan'},
               -justify => 'right')->pack(-side => 'left');
    $b->attach($choicepnts, -balloonmsg => "Number of scan steps\nor simulation repetitions");
    # Gui stuff for selection of 'inspect' parameter
    # PW 20030314
    my $f4 = $opt_frame->Frame;
    $f4->pack(-anchor => 'w', -side => 'top', -fill => 'x');
    my $choiceinspect = $f4->Label(-text => "Inspect component: ", -height => '2')->pack(-side => 'left');
    $b->attach($choiceinspect, -balloonmsg => "For Trace mode, only show neutrons reaching selected component");
    my($ListBox)=$f4->Scrolled('Listbox',-height => '1', -width => '40', -scrollbars => 'osoe', -exportselection => 'false')->pack(-side => 'right');
    # Selection of 'First' and 'Last' components to visualize
    my $f5 = $opt_frame->Frame;
    $f5->pack(-anchor => 'w', -side => 'top', -fill => 'x');
    my $choicefirst = $f5->Label(-text => "First component: ", -height => '2')->pack(-side => 'left');
    $b->attach($choicefirst, -balloonmsg => "For Trace mode, show instrument geometry from this component");
    my($ListBoxFirst)=$f5->Scrolled('Listbox',-height => '1', -width => '40', -scrollbars => 'osoe', -exportselection => 'false')->pack(-side => 'right');
    my $f6 = $opt_frame->Frame;
    $f6->pack(-anchor => 'w', -side => 'top', -fill => 'x');
    my $choicelast = $f6->Label(-text => "Last component: ", -height => '2')->pack(-side => 'left');
    $b->attach($choicelast, -balloonmsg => "For Trace mode, show instrument geometry up to this component");
    my($ListBoxLast)=$f6->Scrolled('Listbox',-height => '1', -width => '40', -scrollbars => 'osoe', -exportselection => 'false')->pack(-side => 'right');
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
    $si{'Seed'} = 0 unless $doseed;
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
      if ($si{'Format'} eq 0) {
        $plotter = 'McStas';
      } elsif ($si{'Format'} eq 1) {
        $plotter = 'Matlab';
      } elsif ($si{'Format'} eq 2) {
        $plotter = 'Scilab';
      } elsif ($si{'Format'} eq 3) {
        $plotter = 'HTML';
      }
      if ($MCSTAS::mcstas_config{'PLOTTER'} =~ /binary/i && $plotter =~ /Scilab|Matlab/i) { $plotter .= "_binary"; }
      if ($MCSTAS::mcstas_config{'PLOTTER'} =~ /scriptfile/i && $plotter =~ /Scilab|Matlab/i) { $plotter .= "_scriptfile"; }
      # finally set the PLOTTER
      $MCSTAS::mcstas_config{'PLOTTER'} = $plotter;
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
        single_plot("/xserv", $di->[$current_plot], 0);
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

    my @plot_cmd = ();
    if ($Config{'osname'} ne 'MSWin32') { # change spaces into \spaces
      $sim_file_name =~ s! !\ !g;
    }
    push @plot_cmd, $prefix;
    push @plot_cmd, "mcplot$suffix";
    # Should only be done if we are using PGPLOT
    # PW 20030314 - Matlab / Scilab handling below
    if ($MCSTAS::mcstas_config{'PLOTTER'} eq 0) {
        # Load PGPLOT dependent stuff...
        require "mcplotlib.pl";
        $current_plot = -1;        # Component index, or -1 -> overview.
        my $dlg = $win->DialogBox(-title => "McStas: Plot results",
                                  -buttons => ["Close"]);

        my $lf = $dlg->add('Frame');
        $lf->pack(-side => 'left');
        $lf->Label(-text => "Monitors and detectors",
                   -anchor => 'w')->pack(-fill => 'x');
        my $cl = $lf->Scrolled('Listbox',
                               -width => 25,
                               -height => 10,
                               -setgrid => 1,
                               -scrollbars => 'se');
        $cl->pack(-expand => 'yes', -fill => 'y', -anchor => 'w');
        $cl->bind('<Double-Button-1>' => sub { dialog_plot_single($cl,$di);
                                               $dlg->raise; } );
        $cl->insert(0, map "$_->{'Component'}: $_->{'Filename'}", @$di);
        $cl->activate(0);

        my $rf = $dlg->add('Frame');
        $rf->pack(-side => 'top');
        $rf->Label(-text => <<END,
Date: $si->{'Date'}
Instrument name: $ii->{'Name'}
Source: $ii->{'Instrument-source'}
Neutron count: $si->{'Ncount'}
Simulation file: $sim_file_name
END
                   -anchor => 'w',
                   -justify => 'left')->pack(-fill => 'x');
        $rf->Button(-text => "Plot",
                    -command => sub { dialog_plot_single($cl,$di);
                                      $dlg->raise; } )->pack;
        $rf->Button(-text => "Overview plot",
                    -command => sub {
                        overview_plot("/xserv", $di, 0);
                        $dlg->raise;
                        $current_plot = -1; }
                    )->pack;
        $rf->Button(-text => "B&W postscript",
                    -command => sub { dialog_hardcopy($dlg,
                                                      $di, "ps"); }
                    )->pack;
        $rf->Button(-text => "Colour postscript",
                    -command => sub { dialog_hardcopy($dlg,
                                                      $di, "cps"); }
                    )->pack;
        $rf->Button(-text => "Colour GIF",
                    -command => sub { dialog_hardcopy($dlg,
                                                      $di, "gif"); }
                    )->pack;
        $rf->Button(-text => "Colour PPM",
                    -command => sub { dialog_hardcopy($dlg,
                                                      $di, "ppm"); }
                    )->pack;
#     $lf->Button(-text => "Select from overview",
#                 -command => sub {
#                     my ($c, $idx) = overview_plot("/xserv", $di, 1);
#                     $cl->activate($idx);
#                     $current_plot = -1;}
#                 )->pack;

        overview_plot("/xserv", $di, 0);
        my $res = $dlg->Show;
        return ($res);
      } else {
        push @plot_cmd, $sim_file_name;
        my $cmd=join(' ',@plot_cmd);
        putmsg($cmdwin, "$cmd\n",'msg');
  if($Config{'osname'} eq "MSWin32") {
      system($cmd);
  } else {
      my $pid = fork();
      if(!defined($pid)) {
    $w->messageBox(-message =>
             "Failed to spawn plotter \"$cmd.",
             -title => "Plotter failed",
             -type => 'OK',
             -icon => 'error');
    return 0;
      } elsif ($pid > 0) {
    waitpid($pid, 0);
    return 1;
      } else {
    # Double fork to avoid having to wait() for the editor to
    # finish (or having it become a zombie). See man perlfunc.
    unless(fork()) {
        exec($cmd);
        # If we get here, the exec() failed.
        print STDERR "Error: exec() of $external_editor failed!\n";
        POSIX::_exit(1);        # CORE:exit needed to avoid Perl/Tk failure.
    }
    POSIX::_exit(0);                # CORE:exit needed to avoid Perl/Tk failure.
      }
  }
    }
}

sub preferences_dialog {
    # Choice of plotting backend
    # PW 20030314
    # Choice of internal editor
    # PW 20040527
    my ($win) = @_;
    my $dlg = $win->DialogBox(-title => "McStas: Configuration options",
                              -buttons => ["OK"]);
    $b = $dlg->Balloon(-state => 'balloon');
    my $lf = $dlg->Frame(-borderwidth => 2, -relief => 'ridge');
    my $rf = $dlg->Frame(-borderwidth => 2, -relief => 'ridge');
    my $buttons, $edit_buttons;
    my $plotter_id=0;
    my $plotter = $MCSTAS::mcstas_config{'PLOTTER'};

    $lf->pack(-side => 'left', -fill => 'both');
    my $plotopt = $lf->Label(-text => "Plotting options:", -anchor => 'w')->pack(-fill => 'x');
    $b->attach($plotopt, -balloonmsg => "Select output format/plotter");
    $buttons[0]=$lf->Radiobutton(-text => "PGPLOT (original mcdisplay)",
               -anchor => 'w', -value => "PGPLOT", -variable => \$plotter)->pack(-fill => 'x');
    $buttons[1]=$lf->Radiobutton(-text => "Matlab (requires Matlab)",
               -anchor => 'w', -value => "Matlab", -variable => \$plotter)->pack(-fill => 'x');
    $buttons[2]=$lf->Radiobutton(-text => "Matlab scriptfile",
               -anchor => 'w', -value => "Matlab_scriptfile", -variable => \$plotter)->pack(-fill => 'x');
    $buttons[3]=$lf->Radiobutton(-text => "Scilab (requires Scilab)",
               -anchor => 'w', -value => "Scilab", -variable => \$plotter)->pack(-fill => 'x');
    $buttons[4]=$lf->Radiobutton(-text => "Scilab scriptfile",
               -anchor => 'w', -value => "Scilab_scriptfile", -variable => \$plotter)->pack(-fill => 'x');
    $buttons[5]=$lf->Radiobutton(-text => "HTML/VRML document",
               -anchor => 'w', -value => "HTML", -variable => \$plotter)->pack(-fill => 'x');
    $buttons[6]=$lf->Checkbutton(-text => "Use binary files (faster)",
               -relief => 'flat', -variable => \$binary)->pack(-fill => 'x');
    $b->attach($buttons[6], -balloonmsg => "Binary files are usually much faster\nto import (Matlab/Scilab)");
    if ($plotter=~ /PGPLOT|McStas/i) {
      $plotter_id=0;
    } elsif ($plotter =~ /Matlab/i && $plotter =~ /scriptfile/i) {
      $plotter_id=2;
    } elsif ($plotter =~ /Matlab/i) {
      $plotter_id=1;
    } elsif ($plotter =~ /Scilab/i && $plotter =~ /scriptfile/i) {
      $plotter_id=4;
    } elsif ($plotter =~ /Scilab/i) {
      $plotter_id=3;
    } elsif ($plotter =~ /HTML/i || $plotter =~ /VRML/i) {
      $plotter_id=5;
    }
    $buttons[$plotter_id]->select;
    if ($binary == 1) { $buttons[6]->select; }

    $editor = $MCSTAS::mcstas_config{'EDITOR'};
    $rf->pack(-side => 'top', -fill => 'both');
    my $editorchoice = $rf->Label(-text => "Editor options:", -anchor => 'w')->pack(-fill => 'x');
    $b->attach($editorchoice, -balloonmsg => "Select editor to use to\ndisplay instrument descriptions");
    $edit_buttons[0]=$rf->Radiobutton(-text => "Simple built-in editor (McStas 1.7)",
               -anchor => 'w', -value => 0, -variable => \$editor)->pack(-fill => 'x');
    $edit_buttons[1]=$rf->Radiobutton(-text => "Advanced built-in editor",
               -anchor => 'w', -value => 1, -variable => \$editor,
               -state => ($MCSTAS::mcstas_config{'CODETEXT'} ne "no" ? 'normal' : 'disabled'))->pack(-fill => 'x');
    $edit_buttons[2]=$rf->Radiobutton(-text => "External editor ($MCSTAS::mcstas_config{'EXTERNAL_EDITOR'})",
               -anchor => 'w', -value => 2, -variable => \$editor)->pack(-fill => 'x');
    $edit_buttons[$editor]->select;
    $choicequote = $rf->Checkbutton(-text => "Surround strings with quotes",
               -relief => 'flat', -variable => \$quote)->pack(-fill => 'x');
    $b->attach($choicequote, -balloonmsg => "All string parameters will be surrounded with quotes\nThis option does not allow to pass variable names");
    if ($quote) { $choicequote->select; }

    my $res = $dlg->Show;
    # add binary flag to plotter
    if ($binary == 1 && $plotter =~ /Scilab|Matlab/i) { $plotter .= "_binary"; }
    # finally set the PLOTTER
    $MCSTAS::mcstas_config{'PLOTTER'} = $plotter;
    $MCSTAS::mcstas_config{'EDITOR'}  = $editor;

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
    my $okbut = $bot_frame->Button(-text => "Ok", -command => $ok_cmd);
    $okbut->pack(-side => "left", -expand => 1, -padx => 1, -pady => 1);
    my $cancelbut = $bot_frame->Button(-text => "Cancel",
                                       -command => $cancel_cmd);
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
    $f->Label(-text => "Available component definitions:")->pack;
    my $list = $f->Scrolled('MyListbox', -width => 50, -height => 10,
                            -setgrid => 1, -scrollbars => 'osre');
    $list->pack(-expand => 'yes', -fill => 'y', -anchor => 'n');
    my @sorted = sort {compname($a) cmp compname($b)} @$clist;
    my @namelist = map compname($_), @sorted;
    $list->insert(0, @namelist);
    $list->activate(0);
    my $name = $f->Label(-text => "Name: ", -anchor => 'w');
    $name->pack(-fill => 'x');
    my $loc = $f->Label(-text => "Location: ", -anchor => 'w');
    $loc->pack(-fill => 'x');
    my $text = $f->Scrolled(qw/ROText -relief sunken -bd 2 -setgrid true
                            -height 10 -width 80 -scrollbars osoe/);
    $text->pack();
    $text->tagConfigure('SHORT', -foreground => 'blue');
    my $f1 = $f->Frame();
    $f1->pack(-fill => 'x');
    my $author = $f1->Label(-text => "Author: ",
                            -anchor => 'w', -justify => 'left');
    $author->pack(-side => 'left');
    my $date = $f1->Label(-text => "Date: ",
                          -anchor => 'w', -justify => 'left');
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
        $text->insert("end", "$info->{'identification'}{'short'}\n\n","short");
        $text->insert("end", $info->{'description'});
    };
    my $accept_cmd = sub { $selected = 'Ok'; };
    my $cancel_cmd = sub { $selected = 'Cancel'; };
    # Set up function to be called each time an item is selected.
    $list->selecthook($select_cmd);
    $list->bind('<Double-Button-1>' => $accept_cmd);
    $list->bind('<Return>' => $accept_cmd);
    my $okbut = $bot_frame->Button(-text => "Ok", -command => $accept_cmd);
    $okbut->pack(-side => "left", -expand => 1, -padx => 1, -pady => 1);
    my $cancelbut = $bot_frame->Button(-text => "Cancel",
                                       -command => $cancel_cmd);
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
    my $sitemenu = $menu->Menubutton(-text => 'Neutron site', -underline => 0);
    $sitemenu->pack(-side=>'left');

    # Scan each .instr file in the examples folder, find out which
    # site it belongs to...
    if (opendir(DIR,"$MCSTAS::sys_dir/examples/")) {
        my @instruments = readdir(DIR);
        closedir(DIR);
        next unless @instruments;
        @paths = map("$MCSTAS::sys_dir/examples/$_", grep(/\.(instr)$/, @instruments));
        my $j;
        my @added; # Names of sites
        my @handles; # Menu handles
        my $index;
        my $CurrentSub;
  # Add subitem for instruments without cathegory
  push @added, "Undefined site";
  $CurrentSub = $sitemenu->cascade(-label => "Undefined site");
  push @sites, $CurrentSub;

        for ($j=0 ; $j<@paths; $j++) {
            # What site is this one from?
            my $pid = open(READER,$paths[$j]);
            my $cname="";  # real name of the instrument (DEFINE)
            my ($base, $dirname, $suffix);
            $base = "";
      my $site_tag=0;
            while(<READER>) {
                # Look for real instrument name
                if (m!DEFINE\s+INSTRUMENT\s+([a-zA-Z0-9_]+)\s*(.*)!i) {
                  $cname = $1;
                }
                # Look for %INSTRUMENT_SITE:
                if (/%INSTRUMENT_SITE:\s*(\w*)/) {
        # This one has a site tag
        $site_tag = 1;
                    # Check if that menu has been added?
                    my $k;
                    my $taken = 0;
                    for ($k=0; $k<@added; $k++) {
                        if ($added[$k] eq $1) {
                            $taken = 1;
                            $index = $k;
                            $CurrentSub = $sites[$k];
                        }
                    }
                    if ($taken == 0) {
                        push @added, $1;
                        $CurrentSub = $sitemenu->cascade(-label => $1);
                        push @sites, $CurrentSub;
                        $index = @added;
                    }
                }
            } # end while
            # Add the instrument to the given menu.
            ($base, $dirname, $suffix) = fileparse($paths[$j],".instr");
            if ($cname ne "" && $cname ne $base) { $base = "$base ($cname)"; }
      if ($site_tag == 1) {
    $CurrentSub->command(-label => "$base", -command => [ sub { sitemenu_runsub(@_)}, $paths[$j], $w]);
      } else {
    $CurrentSub = $sites[0]; # 'Undefined site' menu
    $CurrentSub->command(-label => "$base", -command => [ sub { sitemenu_runsub(@_)}, $paths[$j], $w]);
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
