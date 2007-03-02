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
if (!($filename =~ /^\//)) {
    $filename = getcwd()."/".$filename;
}

my $there = 0;
if (!(-e $filename)) {
    print "File $filename does not exist yet, waiting quietly here until it does...\n";
    while ($there ==0 ) {
	sleep $timeout;
	if (-e $filename) {
	    print "$filename has appeared, continuing...\n";
	    $there = 1;
	}
    }
}
my $dirname = dirname($filename);
print "Working with $filename in dir $dirname\n";

my $timestamp = (stat($filename))[9];
my $newtime;
print "start time given is $timestamp\n";

while (1 == 1) {
    sleep $timeout;
    $newtime = (stat($filename))[9];
    if (! ($newtime == $timestamp) ) {
	print "Directory was accessed! - Replotting\n";
	my $GFORMAT = "png";
	system("mcplot -$GFORMAT $filename") || print("Problems spawning mcplot!\n");
	my $timestring = ctime($newtime);
	$timestring =~ s!\ !_!g;
	system("mv $filename.$GFORMAT $dirname/mcstas_".$newtime.".$GFORMAT");
	$timestamp = (stat($filename))[9];
	$newtime = $timestamp;
	# If this is PGPLOT we should do another call to get X11 output
	if ($filename =~ /\.sim/) {
	    system("mcplot -d $filename") || (die "Could not spawn mcplot!\n");
	}
	
    }
}
