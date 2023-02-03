#!/usr/bin/perl
#     This file is part of the McStas neutron ray-trace simulation package
#     Copyright (C) 1997-2004, All rights reserved
#     Risoe National Laborartory, Roskilde, Denmark
#     Institut Laue Langevin, Grenoble, France
#
#     This program is free software; you can redistribute it and/or modify
#     it under the terms of the GNU General Public License as published by
#     the Free Software Foundation; version 3 of the License.
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
# mcformatgui.pl - perl-Tk gui for mcformat tool.
#

use Cwd;
use IPC::Open2;
use File::Basename;
use File::Path;
use File::Copy;
use File::Spec;
use Time::localtime;
use Tk::Balloon;
use Config;
use FileHandle;


# Determine the path to the McStas system directory. This must be done
# in the BEGIN block so that it can be used in a "use lib" statement
# afterwards.
BEGIN {
    ENV_HEADER
}

use lib $MCSTAS::perl_dir;
use lib $MCSTAS::perl_modules;
require "mccode_config.perl";

# Overload with user's personal config
if ($ENV{"HOME"} && -e $ENV{"HOME"}."/.".$MCSTAS::mcstas_config{'MCCODE'}."/".$MCSTAS::mcstas_config{'VERSION'}."/mccode_config.perl") {
  print "mcformatgui: reading local $MCSTAS::mcstas_config{'MCCODE'} configuration from " . $ENV{"HOME"}."/.".$MCSTAS::mcstas_config{'MCCODE'}."/".$MCSTAS::mcstas_config{'VERSION'}."/mccode_config.perl\n";
  require $ENV{"HOME"}."/.".$MCSTAS::mcstas_config{'MCCODE'}."/".$MCSTAS::mcstas_config{'VERSION'}."/mccode_config.perl";
}

my $dodisplay = 0;
my $timeout = 5;
my $show_help = 0;
my $iformats = ['McStas','Matlab'];
my $oformats;
my $runmodes;
my $iformat="McStas";
my $oformat="McStas";
my $runmode="Merge";
my $inputdir;
my $outputdir;
my $oformats_iMcStas;
my $runmodes_iMcStas = ['Convert','Merge','Scan assembly','Scan Merge'];
my $runmodes_ilab = ['Convert'];
my $recordlog=0;

my $ext;
my $filename = "";
my $i;
my $continue;

my $iformat_crtl;
my $oformat_line="";

if ($MCSTAS::mcstas_config{'NEXUS'} ne "") {
   $oformats_iMcStas = ['McStas','Matlab','IDL','HTML','XML','Octave','Python','Scilab','NeXus'];
} else {
   $oformats_iMcStas = ['McStas','Matlab','IDL','HTML','XML','Octave','Python','Scilab'];
}

my $cmd;
my $logfile;
my $date = time();
$logfile = "mcformatgui_${date}.log";
Tkgui();
$iformat  = iformat_select($inputdir);
my $fid;

if ($recordlog) {
  $logfile = "mcformatgui_${date}.log";
  $fid = new FileHandle "> $logfile";
  if (defined $fid) {
    print $fid "# mcformatgui log file '$logfile'\n";
    print $fid "# Directories: $inputdir \@ $iformat format -> $outputdir \@ $oformat format.\n";
    print $fid "# Action: $runmode\n";

    print "mcformatgui: Recording $inputdir -> $outputdir operations into log file '$logfile'\n";
  }
}

if ($oformat_line !~ /binary/s) {
    print "Input format is McStas, running mcformat to $runmode data. Output will go to $outputdir in $oformat...\n";
    my $mode="";
    if ($oformat =~ /IDL/i) { $oformat="IDL_binary"; }
    if    ($runmode =~ /Scan assembly/i) { $mode="--scan-only"; }
    elsif ($runmode =~ /Scan Merge/i)    { $mode="--scan"; }
    elsif ($runmode =~ /merge/i)         { $mode="--merge"; }
    if ($mode =~ /scan/i && $oformat !~ /McStas/i) {
      my $ret = $w->messageBox(
          -message => "For a Scan Operation ($mode)\n
            The McStas/PGPLOT output format if prefered.\n
            Other formats will not display correctly.\n
            Press 'Yes' to export in McStas\n
            or 'No' to keep $oformat.",
          -title => "Scan operation: McStas prefered.",
          -type => 'YesNo',
          -icon => 'question',
          -default => 'yes');
        if (lc($ret) eq "yes") { $oformat="McStas"; }
    }
    $cmd="mcformat";
    if ($Config{'osname'} eq 'MSWin32') { $cmd .= ".$MCSTAS::mcstas_config{'EXE'}"; }
    $cmd.=" --format=$oformat --dir=$outputdir $inputdir $mode";
} else {
  print "mcformatgui: I do not have any appropriate method for conversion.\n";
  print "ERROR        Try mcformat command manually.\n";
  exit();
}
if ($recordlog && defined $fid) {
  print $fid "# Command: $cmd\n\n";
  $cmd .= ">> $logfile 2>&1 ";
  close($fid);
}
print "Executing: $cmd\n";
system("$cmd");

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
    my $b = $win->Balloon(-state => 'balloon');
    $topframe->pack(-side => "top", -fill => "both", -ipady => 3, -ipadx => 3);
    my $tmp1 = $topframe->Label(-text => "Input dir(s):", -anchor => 'w',
				     -justify => "center", -width => 15, -fg => 'blue')->pack(-side => "left");
	  $b->attach($tmp1, -balloonmsg => "Data directory to convert");
    $topframe->Entry(-width => 40, -relief => "sunken",
				    -textvariable => \$inputdir)->pack(-side => "left");
    my $dirselect = $topframe->Button(-text => "Select", -command => sub {
        $inputdir = select_dir($inputdir);
        $iformat  = iformat_select($inputdir);
     })->pack(-side => "left");
    $b->attach($dirselect, -balloonmsg => "Select an existing directory");

    my $top2frame = $win->Frame(-relief => 'raised', -borderwidth => 2);
    $top2frame->pack(-side => "top", -fill => "both", -ipady => 3, -ipadx => 3);
    $tmp2 = $top2frame->Label(-text => "Output dir  :", -anchor => 'w',
				     -justify => "center", -width => 15, -fg => 'blue')->pack(-side => "left");
		$b->attach($tmp2, -balloonmsg => "Target directory\nfor converted data");
    $top2frame->Entry(-width => 40, -relief => "sunken",
		      -textvariable => \$outputdir)->pack(-side => "left");
    my $dirselect = $top2frame->Button(-text => "Select", -command => sub {
        $outputdir = select_dir($outputdir);
        $outputdir = check_dir($win,$outputdir);
      })->pack(-side => "left");
    $b->attach($dirselect, -balloonmsg => "Select an existing directory\nor enter a new one");

    my $midframe = $win->Frame(-relief => 'raised', -borderwidth => 2);
    $midframe->pack(-side => "top", -fill => "both", -ipady => 3, -ipadx => 3);
    $tmp3=$midframe->Label(-text => "Output format: ", -anchor => 'w',
		     -justify => "center", -fg => 'blue')->pack(-side => "left");
		$b->attach($tmp3, -balloonmsg => "Format for converted data");
    $oformats = $oformats_iMcStas;
    $oformat_ctrl = $midframe->Optionmenu(-textvariable => \$oformat, -options =>
					 $oformats)->pack(-side => 'left');
    $tmp4=$midframe->Label(-text => "Conversion mode: ", -anchor => 'w',
		     -justify => "center", -fg => 'blue')->pack(-side => "left");
		$b->attach($tmp4, -balloonmsg => "Convert: convert files one by one\nMerge: convert files and merge equivalent ones (e.g. clusters/grids)\nScan assembly: convert files and gather them in scan series\nScan merge: same as assembly, but also merge equivalent files");
    $runmodes = $runmodes_iMcStas;
    $runmode_ctrl = $midframe->Optionmenu(-textvariable => \$runmode, -options =>
					 $runmodes)->pack(-side => 'left');

    my $bottomframe = $win->Frame(-relief => 'raised', -borderwidth => 2);
    $bottomframe->pack(-side => "top", -fill => "both", -ipady => 3, -ipadx => 3);
    my $recordlog = $bottomframe->Checkbutton(-text => "Record Log file",-variable => \$recordlog)->pack(-side => 'left');
    $b->attach($recordlog, -balloonmsg => "Records data handling operations into $logfile file");

    $bottomframe->Button(-text => "Cancel", -fg => 'red', -command => sub {exit;})->pack(-side => "right", -anchor => "e");
    $bottomframe->Button(-text => "Ok", -fg => 'green', -command => sub {
	if ($inputdir && $outputdir) {
	    $outputdir = check_dir($win,$outputdir);
	    $continue=1; $win->destroy;
	} else {
	    $win->messageBox(
			     -message => "You must select both an input and an output dir!",
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
	$output = File::Spec->catfile( $output, "$date" );
	$win->messageBox(
			 -message => "For safety reasons I will create the subdir \n\n$output\n\n as final destination.\n\n ".
			 "This directory does not exist now but will be created at runtime.\n",
			 -title => "Notice: Directory exists",
			 -type => 'ok',
			 -icon => 'info',
			 -default => 'ok');
    }
    return $output;
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
  my ($dir) = @_;
  my $file = $dir;

  # check input data directory
  if (-d $file) { # check if dir containing result file
    my $newfile = "$file/mcstas";
    if (-e "$newfile.m" || -e "$newfile.sci" || -e "$newfile.sim" || -e "$newfile.html" || -e "$newfile.nxs" || -e "$newfile.pro" || -e "$newfile.xml") {
      $file = $newfile; }
  }

  # look if there is only one file type and set iformat
  if (-e "$file.m")    { $iformat = "Matlab";  $file = "$file.m"; }
  if (-e "$file.sim")  { $iformat = "McStas";  $file = "$file.sim"; }
  if (-e "$file.html") { $iformat = "HTML";    $file = "$file.html"; }
  if (-e "$file.xml")  { $iformat = "XML";     $file = "$file.xml";  }
  if (-e "$file.pro")  { $iformat = "IDL";     $file = "$file.pro"; }
  if (-e "$file.nxs")  { $iformat = "NeXus";   $file = "$file.nxs"; }

  if (open $handle, $file) {
    while(<$handle>) {
      if(/Format\s*(.*?)\s*$/i) {
          $oformat_line = $1;
      }
    }
    close($fid);
    print "Input directory $dir presumably contains data in $iformat format.\n";
    if ($oformat_line =~ /binary/i) { print "       It contains binary blocks.\n"; }
  } else { print "mcformatgui: Warning: Could not open file '$file'. Conversion may fail.\n"; }

  return($iformat);
}
