#! /usr/bin/perl -w
#
# Implements perl interface for plotting McStas data output using PGPLOT,
# Matlab or Scilab
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

use FileHandle;

# Determine the path to the McStas system directory. This must be done
# in the BEGIN block so that it can be used in a "use lib" statement
# afterwards.

use Config;
BEGIN {
    $pg_avail=0;
    $temp_avail=0;
    foreach $inc (@INC) {
      my $where="$inc/PGPLOT.pm";
      if (-e $where) { $pg_avail=1; }
      $where="$inc/Temp.pm";
      if (-e $where) { $temp_avail=1; }
      $where="$inc/File/Temp.pm";
      if (-e $where) { $temp_avail=1; }
    }
    if ($temp_avail == 1) { require File::Temp; }
    
    if($ENV{"MCSTAS"}) {
      $MCSTAS::sys_dir = $ENV{"MCSTAS"};
      if ($Config{'osname'} eq 'MSWin32') {
	$MCSTAS::runscilab ="runscilab";
      } else {
	$MCSTAS::runscilab ="scilab";
      }
    } else {
      if ($Config{'osname'} eq 'MSWin32') {
        $MCSTAS::sys_dir = "c:\\mcstas\\lib";
        $MCSTAS::runscilab ="runscilab";
      } else {
        $MCSTAS::sys_dir = "/usr/local/lib/mcstas";
        $MCSTAS::runscilab ="scilab";
        # install atexit-style handler so that when we exit or die,
        # we automatically delete this temporary file
        # does not work as temp files is often removed before Scilab starts...
        # END { if (defined($tmp_file) && !($Config{'osname'} eq 'MSWin32')) { unlink($tmp_file) or die "mcplot: Couldn't unlink $tmp_file : $!"; } }
      }
    }
    $MCSTAS::perl_dir = "$MCSTAS::sys_dir/tools/perl";
    
}

use lib $MCSTAS::perl_dir;
require "mcstas_config.perl";

# ADD/MOD: E. Farhi Sep 21th, 2001 : handle -ps and -psc for automatic 
# print and exit
my ($default_ext);
my ($file, $files);
my $index =0;
my $passed_arg_str = "";
my $passed_arg_str_quit = "";
my $inspect = "";
my ($plotter);
my $nowindow = -1;
my $do_swap=0;


if ($Config{'osname'} eq 'MSWin32'){
  $nowindow = 0;
}
$plotter = defined($ENV{'MCSTAS_FORMAT'}) ?
                $ENV{'MCSTAS_FORMAT'} : "$MCSTAS::mcstas_config{'PLOTTER'}";
                
for($i = 0; $i < @ARGV; $i++) {
  $_ = $ARGV[$i];
  # Options specific to mcplot.
  if(/^-plot$/i) {
      $do_plot = 1;
  } elsif(/^-overview$/i) {
      $do_overview = 1;
  } elsif(/^-png$/i || /^-ps$/i || /^-psc$/i || /^-ppm$/i || /^-scg$/i || /^-fig$/i) {
      $passed_arg_str_quit .= "$_ ";
  } elsif(/^-p([a-zA-Z0-9_]+)$/ || /^--plotter=([a-zA-Z0-9_]+)$/ || /^--format=([a-zA-Z0-9_]+)$/) {
        $plotter = $1;	
  } elsif(/^-i([a-zA-Z0-9_]+)$/ || /^--inspect=([a-zA-Z0-9_]+)$/) {
      $inspect = $1;
  } elsif(/^\+nw$/i || /^\+tk$/i || /^\+java$/i) {
      $nowindow = 0;
  } elsif(/^-nw$/i || /^-nojvm$/i) {
      $nowindow = 1;
  } elsif(/^-swap$/i) {
      $do_swap = 1;
  } elsif(/^--help$/i || /^-h$/i || /^-v$/i) {
      print "mcplot [-ps|-psc|-gif] <simfile | detector_file>\n";
      print "       [-pPLOTTER] Output graphics using {PGPLOT,Scilab,Matlab}\n";
      print "                   The file extension will also set the PLOTTER\n";
      print "       [-overview] Show all plots in a single window\n";
      print "       [-plot]     Show all plots in separate window(s)\n";
      print "       [-iCOMP]    Only show monitors whos name match COMP\n";
      print "       [+nw]       Open {Scilab,Matlab} command window (with Tcl/Java)\n";
      print "       [-nw]       Open {Scilab,Matlab} command window (without Tcl/Java)\n";
      print "  Plots all monitor data from a simulation, or a single data file.\n";
      print "  When using -ps -psc -gif, the program writes the hardcopy file\n";
      print "  and then exits.\n";
      print "SEE ALSO: mcstas, mcdoc, mcplot, mcrun, mcgui, mcresplot, mcstas2vitess\n";
      print "DOC:      Please visit http://neutron.risoe.dk/mcstas/\n";
      exit;
  } elsif(/^-([a-zA-Z0-9_]+)$/) {
      $passed_arg_str_quit .= "-$1 ";
  } else {
      $files[$index] = $ARGV[$i];
      $index++;
  }
}

# Check value of $plotter variable, set 
# $MCSTAS::mcstas_config{'PLOTTER'}
# accordingly
if ($plotter =~ /PGPLOT|McStas|0/i) {
  $MCSTAS::mcstas_config{'PLOTTER'}=0;
} elsif ($plotter =~ /Matlab|1/i) {
  $MCSTAS::mcstas_config{'PLOTTER'}=1;
} elsif ($plotter =~ /Scilab|3/i) {
  $MCSTAS::mcstas_config{'PLOTTER'}=3;  
}

$plotter = $MCSTAS::mcstas_config{'PLOTTER'};

if ($do_plot)     { $passed_arg_str .= "-plot "; }
if ($do_overview) { $passed_arg_str .= "-overview "; }
if ($do_swap)     { $passed_arg_str .= "-swap "; }

if ($index == 0) { 
  $file = "mcstas"; 
} else { $file = $files[0]; }
$file = "$file/mcstas" if -d $file;

# look if there is only one file type
if (-e "$file.m" and not -e "$file.sci" and not -e "$file.sim") { $plotter = 1; }
if (-e "$file.sci" and not -e "$file.m" and not -e "$file.sim") { $plotter = 3; }
if (-e "$file.sim" and not -e "$file.m" and not -e "$file.sci") { $plotter = 0; }


if ($plotter eq 3 || $plotter eq 4) { $default_ext = ".sci"; }
elsif ($plotter eq 1 || $plotter eq 2) { $default_ext = ".m"; }
elsif ($plotter eq 0) { $default_ext = ".sim"; }

if ($file !~ m'\.[^/]*$' && $default_ext) { $file .= $default_ext; }  # ... and add default extension.

if ($file =~ m'\.sci$' || $file =~ m'\.sce$') { $plotter=3; }
if ($file =~ m'\.m$')   { $plotter=1; }
if ($file =~ m'\.sim$') { $plotter=0; }

if ($nowindow eq -1) {  # was not set in argument list
  if ($plotter eq 3) { $nowindow=0; } # Will try Tcl/Tk interface with Scilab
  else { $nowindow=1; }
}

# Added E. Farhi, March 2003. Selection of the plotter (pgplot, scilab, matlab)
if ($plotter eq 3 || $plotter eq 4) {
  # create a temporary scilab execution script
  if ($temp_avail == 1) { 
    ($fh, $tmp_file) = File::Temp::tempfile("mcplot_tmpXXXXXX", SUFFIX => '.sce');
    if (not defined $fh) { die "Could not open temporary Scilab script $tmp_file\n"; }
  } else {
    $tmp_file="mcplot_tmp000000.sce"; 
    $fh = new FileHandle "> $tmp_file";
    if (not defined $fh) { die "Could not open temporary Scilab script $tmp_file\n"; }
  }
  printf $fh "s = stacksize(); if s(1) < 1e7 then stacksize(1e7); end\n";
  printf $fh "getf('$MCSTAS::sys_dir/tools/scilab/mcplot.sci',-1);\n";
  printf $fh "s=mcplot('$file','$passed_arg_str $passed_arg_str_quit','$inspect');\n";
  printf $fh "mprintf('s=mcplot(''$file'',''$passed_arg_str $passed_arg_str_quit'',''$inspect'')\\n');\n";
  if ($passed_arg_str_quit) {
    printf $fh "quit\n";
  } else {
    printf $fh "if length(s)\n";
    printf $fh "mprintf('mcplot: Simulation data structure from file $file\\n');\n";
    printf $fh "mprintf('mcplot: is stored into variable s. Type in ''s'' at prompt to see it !\\n');\n";
    printf $fh "end\n";
  }
  printf $fh "global McPlotTempFile;\nMcPlotTempFile='$tmp_file';\n";
  close($fh);
  if ($nowindow) { system("$MCSTAS::runscilab -nw -f $tmp_file\n"); }
  else { system("$MCSTAS::runscilab -f $tmp_file\n"); }
  
} elsif ($plotter eq 1 || $plotter eq 2) {
  if ($nowindow) { $tosend = "matlab -nojvm -nosplash "; }
  else { $tosend = "matlab "; }
  $tosend .= "-r \"addpath('$MCSTAS::sys_dir/tools/matlab');addpath(pwd);s=mcplot('$file','$passed_arg_str $passed_arg_str_quit','$inspect');";
  $tosend .= "disp('s=mcplot(''$file'',''$passed_arg_str $passed_arg_str_quit'',''$inspect'')');";
      
  if ($passed_arg_str_quit) {
    $tosend .= "exit;\"\n";
  } else {
      $tosend .= "if length(s),";
      $tosend .= "disp('type: help mcplot for this function usage.');";
      $tosend .= "disp('mcplot: Simulation data structure from file $file');";
      $tosend .= "disp('mcplot: is stored into variable s. Type in ''s'' at prompt to see it !');";
      $tosend .= "end;\"\n";
    }
  system($tosend);
} elsif ($plotter eq 0) {
  # McStas original mcplot using perl/PGPLOT
  
  # Check if the PGPLOT module can be found, otherwise
  # disable traditional PGPLOT support - output error
  # message...
  # PW 20030320
  if ($pg_avail eq 0) {
    print STDERR "\n******************************************************\n";
    print STDERR "Default / selected PLOTTER is PGPLOT - Problems:\n\n";
    print STDERR "PGPLOT.pm not found on Perl \@INC path\n\nSolution:\n\n";
    print STDERR "1) Install pgplot + pgperl packages (Unix/Linux/Cygwin) \n";
    print STDERR "2) Rerun mcplot with -p/--plotter set to Scilab/Matlab \n";
    print STDERR "3) Modify $MCSTAS::perl_dir/mcstas_config.perl\n";
    print STDERR "   to set a different default plotter\n";
    print STDERR "4) Set your env variable MCSTAS_FORMAT to set the default\n";
    print STDERR "   data format and plotter\n";
    print STDERR "******************************************************\n\n";
    die "PGPLOT problems...\n";
  }
  
  # Attempt to locate pgplot directory if unset.
  $ENV{'PGPLOT_DIR'} = "/usr/local/pgplot" unless $ENV{'PGPLOT_DIR'};
  $ENV{'PGPLOT_DEV'} = "/xserv" unless $ENV{'PGPLOT_DEV'};

  require "mcfrontlib2D.pl";
  require "mcplotlib.pl";
  
  # ADD/MOD: E. Farhi/V. Hugouvieux Feb 18th, 2002 : handle detector files

  my ($sim_file) = $file;
  my ($instr_inf, $sim_inf, $datalist, $sim_error) = read_sim_file($file);
  if ($sim_error !~ "no error") {
    $file = mcpreplot($files);
    $tmp_file = $file;
    ($instr_inf, $sim_inf, $datalist, $det_error) = read_sim_file($file);
    $file = $files;
    
    if ($det_error !~ "no error") {
      print "'$sim_file':'$sim_error'";
      die   "'$tmp_file':'$det_error'";
    }
  }
  die "No data in simulation file '$file'"
      unless @$datalist;

  if ($passed_arg_str_quit =~ /-cps|-psc/i) {
    overview_plot("$file.ps/cps", $datalist, 0);
          die "Wrote postscript file '$file.ps' (cps)\n";
  } elsif ($passed_arg_str_quit =~ /-ps/) {
    overview_plot("$file.ps/ps", $datalist, 0);
          die "Wrote postscript file '$file.ps' (ps)\n";
  } elsif ($passed_arg_str_quit =~ /-gif/) {
    overview_plot("$file.gif/gif", $datalist, 0);
          die "Wrote GIF file '$file.gif' (gif)\n";
  } 

  print "Click on a plot for full-window view.\n" if @$datalist > 1;
  print "Type 'P' 'C' 'N' or 'G' (in graphics window) for hardcopy, 'Q' to quit.\n";

  for(;;) {
      my ($cc,$cx,$cy,$idx);
      # Do overview plot, letting user select a plot for full-screen zoom.    
      ($cc,$idx) = overview_plot("$ENV{'PGPLOT_DEV'}", $datalist, 1);
      last if $cc =~ /[xq]/i;        # Quit?
      if($cc =~ /[pcng]/i) {        # Hardcopy?
          my $ext="ps";
          my $dev = ($cc =~ /c/i) ? "cps" : "ps";
          if($cc =~ /g/i) { $dev = "gif"; $ext="gif"; }
	  if($cc =~ /n/i) { $dev = "png"; $ext="png"; }
          overview_plot("$file.$ext/$dev", $datalist, 0);
          print "Wrote file '$file.$ext' ($dev)\n";
          next;
      }
      # now do a full-screen version of the plot selected by the user.
      ($cc, $cx, $cy) = single_plot("/xserv", $datalist->[$idx], 1);
      last if $cc =~ /[xq]/i;        # Quit?
      if($cc =~ /[pcg]/i) {        # Hardcopy?
          my $ext="ps";
          my $dev = ($cc =~ /c/i) ? "cps" : "ps";
          if($cc =~ /g/i) { $dev = "gif"; $ext="gif"; }
          my $filename = "$datalist->[$idx]{'Filename'}.$ext";
          single_plot("$filename/$dev", $datalist->[$idx], 0);
          print "Wrote postscript file '$filename'\n";
      }        
  }
}
