#! /usr/bin/perl
#
# Implements a daemon type perl script, sending USR2 to running mcstas 
# process, plus running mcplot.
#
#
#   This file is part of the McStas neutron ray-trace simulation package
#   Copyright (C) 1997-2004, All rights reserved
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

use Tk;
use IPC::Open2;
use File::Basename;

my $win;
my $PID;
my @PIDs=();
my $Instrument = "h8_test.out";
my $Timeout = 300;
my $Datafile = "mcstas.sim";
my $Format = "McStas";
my $working = 0;
my $child = 0;

# Check if we have any input parms:
if (@ARGV==0) {
    # Set up main window:
    $win = new MainWindow;
    setup_window($win);
    MainLoop;
} elsif(@ARGV==4) {
    $PID = $ARGV[0];
    $Timeout = $ARGV[1];
    $Datafile = $ARGV[2];
    $Format = $ARGV[3];
    # Split in pieces
    my $DataFile = basename($Datafile);
    my $DataDir = dirname($Datafile);
    my $McPlot = 0;
    print "Monitoring PID $PID in intervals of $Timeout seconds\n";
    my $stop = 0;
    while ($stop == 0) {
	(kill 0, $PID) || (die "$PID has ended, terminating...\n");
	# Sleep for a while...
	sleep $Timeout;
	(kill 'USR2', $PID) || (die "Could not send USR2 to $PID\n");
	# Replotting in DataDir
	if ($McPlot == 0) {
	    chdir $DataDir || (die "Could not chdir to $DataDir\n");
	    $McPlot=open2(READER,WRITER,"mcplot -p$Format --daemon=$PID --wait=$Timeout $DataFile") || (die "Could not spawn mcplot!\n");
	    $McPlot = 1;
	}
    }
} else {
    die "I need 0 or four input parameters:\n1) PID of simulation\n2) re-saving interval in seconds\n3) Output file to plot\n4) Used format\nFurther input parameters will be passed to mcplot (e.g. -gif)";
}

sub setup_window {
    my ($w) = @_;
    my $f1 = $w->Frame();
    my $pid_list;
    $f1->pack(-fill => 'x');
    my $instr_lab = $f1->Label(-text => "Instrument to monitor:",
                               -anchor => 'w',
                               -justify => 'left');
    $instr_lab->pack(-side => 'left');
    my $instr_entry = $f1->Entry(-textvariable => \$Instrument,
				 -justify => 'right');
    $instr_entry->pack(-side => 'right');
    my $f2 = $w->Frame();
    my $inter_lab = $f2->Label(-text => "Datafile:",
                               -anchor => 'w',
                               -justify => 'left');
    $inter_lab->pack(-side => 'left');
    my $inter_entry = $f2->Entry(-textvariable => \$Datafile,
				 -justify => 'right');
    $inter_entry->pack(-side => 'right');
    $f2->pack(-fill => 'x');
    my $f3 = $w->Frame();
    my $inter_lab = $f3->Label(-text => "Data format:",
                               -anchor => 'w',
                               -justify => 'left');
    $inter_lab->pack(-side => 'left');
    my $inter_entry = $f3->Entry(-textvariable => \$Format,
				 -justify => 'right');
    $inter_entry->pack(-side => 'right');
    $f3->pack(-fill => 'x');
    my $f4 = $w->Frame();
    my $inter_lab = $f4->Label(-text => "Replot interval:",
                               -anchor => 'w',
                               -justify => 'left');
    $inter_lab->pack(-side => 'left');
    my $inter_entry = $f4->Entry(-textvariable => \$Timeout,
				 -justify => 'right');
    $inter_entry->pack(-side => 'right');
    $f4->pack(-fill => 'x');
    my $f5 = $w->Frame();
    my $pid_fetch = $f5->Button(-text => "Fetch PID's", -command => sub {
	@PIDs = get_pid();
	$PID->delete(0,'end');
	foreach my $pid (@PIDs) {
	    $PID->insert(0,$pid);
	}
    }
				);
    $pid_fetch->pack(-side => 'left');
    my $pid_label = $f5->Label(-text => "PID's:");
    $pid_label->pack(-side => 'left');
    $PID = $f5->Scrolled('Listbox',-height => '2', -scrollbars => 'osoe', -exportselection => 'false')->pack(-side => 'right');
    $PID->pack(-side => 'right');
    $f5->pack(-fill => 'x');
    my $f6 = $w->Frame();
    my $start = $f6->Button(-text => "Start", -command => sub {
	start_it();
    }
			    );
    $start->pack(-side => 'left');
    my $stop = $f6->Button(-text => "Stop", -command => sub {
	stop_it();
    }
			   );
    $stop->pack(-side => 'left');
    $f6->pack(-fill => 'x');
}

sub get_pid{
    my $USER=$ENV{'USER'};
    my $PIDdata=`ps -u $USER | grep $Instrument | grep -v grep | cut -b 1-5 `;
    $PIDdata =~ s/ //g;
        
    return split('\n',$PIDdata);
}

sub start_it{
    if ($working == 0) {
	# Check what PID to attack:
	my @pids = $PID->get(0,'end');
	my $selected = $PID->curselection()+1;
	print "Current selection is $selected\n";
	my $pid = @pids[$selected-1]+0;
	# Ensure that we have a PID:
	if ($pid>0 && (kill 0, $pid)) {
	    print "mcdaemon $pid $Timeout $Datafile $Format &\n";
	    $child=open(READER,"mcdaemon $pid $Timeout $Datafile $Format|");
	    print "child process is $child\n";
	    $working = 1;
	}
    }
}

sub stop_it{
    if ($working == 1) {
	print "Terminating $child\n";
	(kill 9, $child);
	$working = 0;
    }
}

sub status {
    my $PID = $_;
    return (kill 0, $pid);
}
