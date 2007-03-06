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
use Time::localtime;
use Config;

my $move_cmd;

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
      $move_cmd = 'move';
    } else {
      $MCSTAS::sys_dir = "/usr/local/lib/mcstas";
      $move_cmd = 'mv';
    }
  }
  $MCSTAS::perl_dir = "$MCSTAS::sys_dir/tools/perl";

  # custom configuration (this script)
  $MCSTAS::perl_modules = "$MCSTAS::perl_dir/modules";
}

use lib $MCSTAS::perl_dir;
use lib $MCSTAS::perl_modules;
require "mcstas_config.perl";

# Input filename needed:
if (@ARGV == 0) {
    print "As minimum, I need a McStas output file (e.g. a mcstas.sim) to monitor. \n";
    print "Possibly, also specify a 'waiting' interval in seconds (default 10 seconds)\nExiting.\n";
    exit;
}

my $filename = $ARGV[0];
my $timeout = 10;
if (@ARGV == 2) {
    $timeout = $ARGV[1];
}
if ($filename eq ".") {
    $filename = getcwd();
}

# Make sure we have an absolute path defined here...
if (!($filename =~ /^\// || ($Config{'osname'} eq 'MSWin32' && ($filename =~ /^([a-zA-Z]):/ || /^\\/)))) {
    $filename = getcwd()."/".$filename;
}

my $there = 0;
my $dirthere = 0;
my @suffixlist = ('.sim'); # Full list is ('.sim','.m','.sci','.html') - currently only PGPLOT makes sense;
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
	print "Called with dir input\n";
	$dirname = $filename;
	$name = "mcstas";
    }
    
    $filename = "$dirname/$name";
    # First of all, the output dir must be there and of type directory
    if (!(-e $dirname)) {
	print "Output directory $dirname does not exist yet, waiting here until it does...\n";
	while ($dirthere == 0) {
	    if (-e $dirname && -d $dirname) {
		$dirname = 1;
	    }
	    if (!$dirthere) {sleep $timeout;}
	} 
    } else {
	print "Directory there, looking for plot file(s)\n";
    }
    while ($there == 0) {
	my $j;
	for ($j=0; $j<@suffixlist; $j++) {
	    if (-e "$filename$suffixlist[$j]") {
		$there = 1;
		$name = "$name$suffixlist[$j]";
		$j = scalar(@suffixlist);
		$filename = "$dirname/$name";
		print "Found datafile $name in $dirname, working with that...";
	    } 
	}
	if (!$there) {sleep $timeout;}
    }
}

($name,$dirname,$filesuf) = fileparse($filename,@suffixlist);

if (!($filesuf eq "sim")) {
    print "Sorry, currently PGPLOT is the only supported plotter with mcdaemon.\n";
    exit 1;
}

my $timestamp = (stat($filename))[9];
my $newtime;
print "start time given is $timestamp\n";

while (1 == 1) {
    sleep $timeout;
    $newtime = (stat($filename))[9];
    if (! ($newtime == $timestamp) ) {
	print "Directory was accessed! - Replotting\n";
	my $GFORMAT = "ps";
	system("mcplot -$GFORMAT $filename") || print("Problems spawning mcplot!\n");
	my $timestring = ctime($newtime);
	$timestring =~ s!\ !_!g;
	system("$move_cmd $filename.$GFORMAT $dirname/mcstas_".$newtime.".$GFORMAT");
	$timestamp = (stat($filename))[9];
	$newtime = $timestamp;
	# If this is PGPLOT we should do another call to get X11 output
	# (Currently only works if the file we work on has .sim extension)
	if ($filename =~ /\.sim/) {
	    system("mcplot -d $filename") || (die "Could not spawn mcplot!\n");
	}
	
    }
}
