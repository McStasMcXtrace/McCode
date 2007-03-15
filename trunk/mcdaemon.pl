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
# mcdaemon.pl - script to monitor / plot McStas output data on save (-USR2 or from
# saves by the Progress_bar component)
#

use Cwd;
use IPC::Open2;
use File::Basename;
use File::Path;
use File::Copy;
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
my $GFORMAT = "png";
my $ext;
my $filename = "";
my $i;

if (@ARGV == 0) { Tkgui(); }

for($i = 0; $i < @ARGV; $i++) {
    if(($ARGV[$i] eq "-d") || ($ARGV[$i] eq "--display")) {
        $dodisplay = 1;
    } elsif($ARGV[$i] =~ /--help|-h$/) {
        $show_help=1;
    } elsif(($ARGV[$i] =~ /^-t([-0-9+]+)$/) ||
            ($ARGV[$i] =~ /^--timeout=([-0-9+]+)$/)) {
        $timeout = $1;
    } elsif(($ARGV[$i] eq "-gif") || ($ARGV[$i] eq "-ps") ||
	    ($ARGV[$i] eq "-psc") || ($ARGV[$i] eq "-png") || ($ARGV[$i] eq "-ppm")) {
	$GFORMAT=$ARGV[$i];
	$GFORMAT=~ s!-!!g;

    } else {
	# This is filename stuff...
	$filename = $filename.$ARGV[$i];
    }
}

if ($show_help) {
     die "Usage: mcdaemon [options] file|dir
Where the possible options are:
-h        --help           Show this help
-d        --display        Display the plots generated at saves
                           (default is NOT to display)
-tSECS    --timeout=SECS   Minimum timeout between updating graphics
                           (default is 5 seconds)
Graphics selection options:
      (Also depending on properties of your PGPLOT installation)
-psc      Color Postscript (default)
-ps       BW Postscript
-gif      GIF format bitmap
-png      PNG format bitmap
-ppm      PPM format bitmap
    
";
     
}

print "\nOkay, mcdaemon is monitoring $filename using these options:\n";
print "(press <ctrl+c> to stop mcdaemon)\n";
print "Hardcopy format:    $GFORMAT\n";
print "Timeout:            $timeout\n";
if ($dodisplay == 1) {
    print "D";
} else {
    print "Not d";
}
print "isplaying results on screen\n\n";

# Make sure we have an absolute path defined here...
if (!($filename =~ /^\// || ($Config{'osname'} eq 'MSWin32' && ($filename =~ /^([a-zA-Z]):/ || /^\\/)))) {
    $filename = getcwd()."/".$filename;
}

my $there = 0;
my $dirthere = 0;
my @suffixlist = ('.sim','.m','.sci','.html'); 
# Currently only PGPLOT makes sense - having the other ones in the
# list is simply to be able to display an error message.

# First of all, if the file is there, work with that...
if (-e $filename && (! -d $filename)) {
    $there = 1;
    $dirthere = 1;
}
if (!$there) {
    my ($name,$dirname,$filesuf) = fileparse($filename,@suffixlist);
    # First, check if we were called with the actual .sim/.m/.sci...
    if ($filesuf) {
	print "Found suffix $filesuf\n";
    } else {
	print "$filename seems to be a directory\n";
	$dirname = $filename;
	$name = "mcstas";
    }

    $filename = "$dirname/$name";
    # First of all, the output dir must be there and of type directory
    if (!(-e $dirname)) {
	print "Output output $dirname does not exist yet, waiting here ...\n";
	while ($dirthere == 0) {
	    if (-e $dirname && -d $dirname) {
		$dirname = 1;
	    }
	    if (!$dirthere) {sleep $timeout;}
	}
    } else {
	print "Searching for plot file(s) in $dirname\n";
    }
    while ($there == 0) {
	my $j;
	for ($j=0; $j<@suffixlist; $j++) {
	    if (-e "$filename$suffixlist[$j]") {
		$there = 1;
		$name = "$name$suffixlist[$j]";
		$j = scalar(@suffixlist);
		$filename = "$dirname/$name";
		print "Found datafile $name in $dirname, continuing\n\n";
	    }
	}
	if (!$there) {sleep $timeout;}
    }
}

($name,$dirname,$filesuf) = fileparse($filename,@suffixlist);

my $PGstuff = 0;

if ($filesuf eq "sim") {
    # Nothing to do here, all is fine
} elsif (!(-e "$dirname/mcstas.sim")) {
    die "\n\nSorry, currently PGPLOT is the only supported plotter with mcdaemon.\n\n";
}

my $timestamp = (stat($filename))[9];
my $newtime;
my $counter = -1;
$ext = $GFORMAT;
$ext =~ s!c!!g;
while (1 == 1) {
    sleep $timeout;
    $newtime = (stat($filename))[9];
    if (! ($newtime == $timestamp) ) {
	$counter++;
	print "Output files accessed! - Replotting\n";
	system("mcplot -$GFORMAT $filename") || die "Problems spawning mcplot!\n";
	my $timestring = ctime($newtime);
	$timestring =~ s!\ !_!g;
	copy("$filename.$ext", "$dirname/mcstas_".$counter.".$ext") || 
	    die "Could not rename mcplot outputfile $filename.$ext";
	print "   (was renamed to mcstas_".$counter.".$ext)\n";
	$timestamp = (stat($filename))[9];
	$newtime = $timestamp;
	# If this is PGPLOT/Unix we should do another call to get X11 output
	# (Currently only works if the file we work on has .sim extension)
	# On Win32 we will ask the OS to display the graphic generated above.
	if ($dodisplay == 1) {
	    if ($Config{'osname'} eq 'MSWin32') {
		system("start $dirname/mcstas_".$counter.".$ext");
	    } else {
		system("mcplot -d $filename") || (die "Could not spawn mcplot!\n");
	    }
	}

    }
}

sub Tkgui {
    use Tk;
    use Tk::Toplevel;
    use Tk::DirTree;
    my $win = new MainWindow(-title => "McDaemon, monitor incoming McStas results");
    build_gui($win);
    MainLoop;
}

sub build_gui {
    # When mcdaemon is run without any input parms, we'll build a gui
    # to set the parameters.
    my ($win) = @_;
    #$win->Toplevel
    # Label and buttons, input dir / file
    my $topframe = $win->Frame(-relief => 'raised', -borderwidth => 2);
    $topframe->pack(-side => "top", -fill => "both", -ipady => 3, -ipadx => 3);
    $topframe->Label(-text => "Data:", -anchor => 'w',
				     -justify => "center")->pack(-side => "left");
    my $filelabel = $topframe->Entry(-width => 60, -relief => "sunken",
				    -textvariable => \$filename)->pack(-side => "left");
    my $fileselect = $topframe->Button(-text => "File", -command => [\&select_file, $win])->pack(-side => "left");
    my $dirselect = $topframe->Button(-text => "Dir", -command => [\&select_dir])->pack(-side => "left");
    my $midframe = $win->Frame(-relief => 'raised', -borderwidth => 2);
    $midframe->pack(-side => "top", -fill => "both", -ipady => 3, -ipadx => 3);
    $midframe->Label(-text => "Timeout (seconds): ", -anchor => 'w',
		     -justify => "center")->pack(-side => "left");
    my $timelabel = $midframe->Entry(-width => 4, -relief => "sunken",
				    -textvariable => \$timeout)->pack(-side => "left");
    $midframe->Label(-text => "Hardcopy format: ", -anchor => 'w',
		     -justify => "center")->pack(-side => "left");
    my $gformats = ['psc','ps','gif','png','ppm'];
    my $graphics = $midframe->Optionmenu(-textvariable => \$GFORMAT, -options => 
					 $gformats)->pack(-side => 'left');
    my $display = $midframe->Checkbutton(-text => "Display on screen?", -variable => \$dodisplay)->pack(-side => "left");
    my $bottomframe = $win->Frame(-relief => 'raised', -borderwidth => 2);
    $bottomframe->pack(-side => "top", -fill => "both", -ipady => 3, -ipadx => 3);
   
    $bottomframe->Button(-text => "Cancel", -command => sub {exit;})->pack(-side => "right", -anchor => "e");
    $bottomframe->Button(-text => "Ok", -command => sub {$win->destroy;})->pack(-side => "right", -anchor => "w");
}

sub select_file {
    my ($w) = @_;
    my $file = $w->getOpenFile(-title => "Select sim file to monitor", -initialdir => getcwd());
    if ($file == 0) {
	$filename = $file;
    }
}

sub select_dir {
    my $top = new MainWindow;
    $top->withdraw;
    
    my $t = $top->Toplevel;
    $t->title("Choose dir to monitor:");
    my $ok = 0; 
    my $f = $t->Frame->pack(-fill => "x", -side => "bottom");
    
    my $curr_dir = getcwd();
    
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
    
    if ($ok == 1) {
	$filename = $curr_dir;
    }
    $top->destroy;
}
