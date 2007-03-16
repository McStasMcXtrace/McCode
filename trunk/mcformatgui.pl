#!/usr/bin/perl
#     This file is part of the McStas neutron ray-trace simulation package
#     Copyright (C) 1997-2004, All rights reserved
#     Risoe National Laborartory, Roskilde, Denmark
#     Institut Laue Langevin, Grenoble, France
#
#     This program is free software; you can redistribute it and/or modify
#     it under the terms of the GNU General Public License as published by
#     the Free Software Foundation; either version 2 of the License, or
#     (at your option) any later version.
#
#     This program is distributed in the hope that it will be useful,
#     but WITHOUT ANY WARRANTY; without even the implied warranty of
#     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#     GNU General Public License for more details.
#
#     You should have received a copy of the GNU General Public License
#     along with this program; if not, write to the Free Software
#     Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
#
# mcformatgui.pl - perl-Tk gui for mcformat and mcconvert tools.
# 

use Cwd;
use IPC::Open2;
use File::Basename;
use File::Path;
use File::Copy;
use File::Spec;
use Time::localtime;
use Config;


# Determine the path to the McStas system directory. This must be done
# in the BEGIN block so that it can be used in a "use lib" statement
# afterwards.
BEGIN {
  # default configuration (for all high level perl scripts)
  if($ENV{"MCSTAS"}) {
    $MCSTAS::sys_dir = $ENV{"MCSTAS"};
  } else {
    if ($Config{'osname'} eq 'MSWin32') {
      $MCSTAS::sys_dir = "c:\\mcstas\\lib";
    } else {
      $MCSTAS::sys_dir = "/usr/local/lib/mcstas";
    }
  }
  $MCSTAS::perl_dir = "$MCSTAS::sys_dir/tools/perl";

  # custom configuration (this script)
  $MCSTAS::perl_modules = "$MCSTAS::perl_dir/modules";
}

use lib $MCSTAS::perl_dir;
use lib $MCSTAS::perl_modules;
require "mcstas_config.perl";

my $dodisplay = 0;
my $timeout = 5;
my $show_help = 0;
my $iformats = ['McStas','Scilab','Matlab'];
my $oformats;
my $runmodes;
my $iformat="McStas";
my $oformat="McStas";
my $runmode="Merge";
my $inputdir;
my $outputdir;
my $oformats_iMcStas = ['McStas','Scilab','Matlab','NeXus','IDL','HTML'];
my $oformats_iMatlab = ['Scilab'];
my $oformats_iScilab = ['Matlab'];
my $runmodes_iMcStas = ['Convert','Merge','Scan assembly','Scan Merge'];
my $runmodes_ilab = ['Convert'];

my $ext;
my $filename = "";
my $i;
my $continue;

Tkgui();

print "Ready to go:\n    $inputdir \@ $iformat format \n -> $outputdir \@ $oformat format\n Action: $runmode\n\n";

if ($iformat eq "McStas") {
    print "Input format is McStas, running mcformat to $runmode data. Output will go to $outputdir in $oformat...\n";
    # Emmanuel, here is the place to do your mcformat stuff
    
} elsif (($iformat eq "Matlab") || ($iformat eq "Scilab")) {
    print "Using mcconvert.pl for $iformat -> $oformat conversion...\n";
    system("mcconvert$MCSTAS::mcstas_config{'SUFFIX'} --format=$oformat --indir=$inputdir --outdir=$outputdir");
} else {
    die "Ooops, don't know what to do about inputformat $iformat! Exiting.\n";
}


sub Tkgui {
    use Tk;
    use Tk::Toplevel;
    use Tk::DirTree;
    $continue = 0;
    my $win = new MainWindow(-title => "McFormatGui: Handle McStas datasets");
    build_gui($win);
    MainLoop;
    if (!($continue == 1)) {
	exit;
    }
}

sub build_gui {
    # When mcdaemon is run without any input parms, we'll build a gui
    # to set the parameters.
    my ($win) = @_;
    my $topframe = $win->Frame(-relief => 'raised', -borderwidth => 2);
    $topframe->pack(-side => "top", -fill => "both", -ipady => 3, -ipadx => 3);
    $topframe->Label(-text => "Input dir(s):", -anchor => 'w',
				     -justify => "center", -width => 15)->pack(-side => "left");
    $topframe->Entry(-width => 80, -relief => "sunken",
				    -textvariable => \$inputdir)->pack(-side => "left");
    my $dirselect = $topframe->Button(-text => "Select", -command => sub {
        $inputdir = select_dir($inputdir);
	})->pack(-side => "left");
    
    my $top2frame = $win->Frame(-relief => 'raised', -borderwidth => 2);
    $top2frame->pack(-side => "top", -fill => "both", -ipady => 3, -ipadx => 3);
    $top2frame->Label(-text => "Output dir :", -anchor => 'w',
				     -justify => "center", -width => 15 )->pack(-side => "left");
    $top2frame->Entry(-width => 80, -relief => "sunken",
		      -textvariable => \$outputdir)->pack(-side => "left");
    my $dirselect = $top2frame->Button(-text => "Select", -command => sub {
        $outputdir = select_dir($outputdir);
	$outputdir = check_dir($win,$outputdir);
    }
)->pack(-side => "left");
    
    my $midframe = $win->Frame(-relief => 'raised', -borderwidth => 2);
    $midframe->pack(-side => "top", -fill => "both", -ipady => 3, -ipadx => 3);
    $midframe->Label(-text => "Input format: ", -anchor => 'w',
		     -justify => "center")->pack(-side => "left");
    my $iformat_crtl = $midframe->Optionmenu(-textvariable => \$iformat, -options => 
					 $iformats, -command => \&iformat_select, -variable => \$iform )->pack(-side => 'left');
    $midframe->Label(-text => "Output format format: ", -anchor => 'w',
		     -justify => "center")->pack(-side => "left");
    $oformats = $oformats_iMcStas;
    $oformat_ctrl = $midframe->Optionmenu(-textvariable => \$oformat, -options => 
					 $oformats)->pack(-side => 'left');
    $midframe->Label(-text => "Conversion mode: ", -anchor => 'w',
		     -justify => "center")->pack(-side => "left");
    $runmodes = $runmodes_iMcStas;
    $runmode_ctrl = $midframe->Optionmenu(-textvariable => \$runmode, -options => 
					 $runmodes)->pack(-side => 'left');
    
    my $bottomframe = $win->Frame(-relief => 'raised', -borderwidth => 2);
    $bottomframe->pack(-side => "top", -fill => "both", -ipady => 3, -ipadx => 3);
   
    $bottomframe->Button(-text => "Cancel", -command => sub {exit;})->pack(-side => "right", -anchor => "e");
    $bottomframe->Button(-text => "Ok", -command => sub {
	if ($inputdir && $outputdir) {
	    $outputdir = check_dir($win,$outputdir);
	    $continue=1; $win->destroy;
	} else {
	    $win->messageBox(
			     -message => "You must select a both an input and an output dir!", 
			     -title => "Problem:",
			     -type => 'ok',
			     -icon => 'error',
			     -default => 'ok');
	}
    })->pack(-side => "right", -anchor => "w");
}

sub check_dir {	
    my ($win, $output) = @_;
    if (-d $output && -e $output) {
	$output = File::Spec->catfile( $output, "converted" );;
	$win->messageBox(
			 -message => "For safety reasons I will create the subdir \n\n$output\n\n as final destination.\n\n ".
			 "This directory does not exist now but will be created at runtime.\n",
			 -title => "Notice:",
			 -type => 'ok',
			 -icon => 'info',
			 -default => 'ok');
    }
    return $output;
}

sub select_file {
    my ($w) = @_;
    my $file = $w->getOpenFile(-title => "Select sim file to monitor", -initialdir => getcwd());
    if ($file == 0) {
	$filename = $file;
    }
}

sub select_dir {
    my ($start_dir) = @_;
    my $top = new MainWindow;
    $top->withdraw;
    
    my $t = $top->Toplevel;
    $t->title("Choose dir to monitor:");
    my $ok = 0; 
    my $f = $t->Frame->pack(-fill => "x", -side => "bottom");
    
    my $curr_dir;
    
    if ($start_dir) {
	$curr_dir = $start_dir;
    } else { 
	$curr_dir = getcwd(); 
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

sub iformat_select {
    if ($iformat eq "McStas") {
	$oformat_ctrl->configure(-options => $oformats_iMcStas);
	$runmode_ctrl->configure(-options => $runmodes_iMcStas);
	$oformat = "McStas";
	$runmode = "Merge";
    } elsif ($iformat eq "Matlab") {
	$oformat = "Scilab";
	$runmode = "Convert";
	$oformat_ctrl->configure(-options => $oformats_iMatlab);
	$runmode_ctrl->configure(-options => $runmode_ilab);
    } elsif ($iformat eq "Scilab") {
	$oformat = "Matlab";
	$runmode = "Convert";
	$oformat_ctrl->configure(-options => $oformats_iScilab);
	$runmode_ctrl->configure(-options => $runmode_ilab);
    }
}

