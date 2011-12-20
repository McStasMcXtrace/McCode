#! /usr/bin/perl
#
# Implements perl interface for plotting McStas data output using PGPLOT,
# gnuplot, Matlab or Scilab
#
#   This file is part of the McStas neutron ray-trace simulation package
#   Copyright (C) 1997-2004, All rights reserved
#   Risoe National Laborartory, Roskilde, Denmark
#   Institut Laue Langevin, Grenoble, France
#
#   This program is free software; you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation; version 2 of the License.
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
use File::Basename;

# Determine the path to the McStas system directory. This must be done
# in the BEGIN block so that it can be used in a "use lib" statement
# afterwards.

use Config;
BEGIN {
  # default configuration (for all high level perl scripts)
  if($ENV{"MCSTAS"}) {
    $MCSTAS::sys_dir = $ENV{"MCSTAS"};
  } else {
    if ($0 =~ /m[xc]plot/i) {
      if ($Config{'osname'} eq 'MSWin32') {
	$MCSTAS::sys_dir = "c:\\mcstas\\lib";
      } else {
	$MCSTAS::sys_dir = "/usr/local/lib/mcstas";
      }
    } else {
      if ($Config{'osname'} eq 'MSWin32') {
	$MCSTAS::sys_dir = "c:\\mcxtrace\\lib";
      } else {
	$MCSTAS::sys_dir = "/usr/local/lib/mcxtrace";
      }
    }
  }
  $MCSTAS::perl_dir = "$MCSTAS::sys_dir/tools/perl";
  $MCSTAS::perl_modules = "$MCSTAS::perl_dir/modules";

  # custom configuration (this script)
  END {
    if (-f $tmp_file ) {
	if (!($plotter =~ /PGPLOT|McStas/i)) {
	    print "mcplot: Removing temporary $tmp_file (10 sec)\n";
	    sleep 10;
	}
	unlink($tmp_file) or die "mcplot: Couldn't unlink $tmp_file : $!";
      }
    }
}



use lib $MCSTAS::perl_dir;
use lib $MCSTAS::perl_modules;
require "mccode_config.perl";

# Overload with user's personal config
if ($ENV{"HOME"} && -e $ENV{"HOME"}."/.".$MCSTAS::mcstas_config{'MCCODE'}."/mccode_config.perl") {
  print "mcplot: reading local $MCSTAS::mcstas_config{'MCCODE'} configuration from " . $ENV{"HOME"}."/.".$MCSTAS::mcstas_config{'MCCODE'}."/mccode_config.perl\n";
  require $ENV{"HOME"}."/.".$MCSTAS::mcstas_config{'MCCODE'}."/mccode_config.perl";
}


# ADD/MOD: E. Farhi Sep 21th, 2001 : handle -ps and -psc for automatic
# print and exit
my ($default_ext);
our ($file, $files);
my $index =0;
my $passed_arg_str = "";
my $passed_arg_str_quit = "";
my $inspect = "";
our ($plotter);
my $nowindow = 0;
my $do_swap=0;
my $daemon=0;
my $wait=10;
my $logmode=0;
my $contourmode=0;
my $global_dev = -1;
my $global_spec ="";
our $tmp_file = "";

$plotter = $MCSTAS::mcstas_config{'PLOTTER'};

for($i = 0; $i < @ARGV; $i++) {
  $_ = $ARGV[$i];
  # Options specific to mcplot.
  if(/^-plot$/i) {
      $do_plot = 1;
  } elsif(/^-overview$/i) {
      $do_overview = 1;
  } elsif(/^-png$/i || /^-ps$/i || /^-cps$/i || /^-psc$/i || /^-ppm$/i || /^-scg$/i || /^-fig$/i) {
      $passed_arg_str_quit .= "$_ ";
  } elsif(/^-p([a-zA-Z0-9_]+)$/ || /^--plotter=([a-zA-Z0-9_]+)$/ || /^--format=([a-zA-Z0-9_]+)$/) {
        $plotter = $1;
  } elsif(/^-i([a-zA-Z0-9_]+)$/ || /^--inspect=([a-zA-Z0-9_]+)$/) {
      $inspect = $1;
  } elsif(/^-d$/ || /^--daemon$/) {
      $daemon = 1;
  } elsif(/^-w([0-9\.]+)$/ || /^--wait=([0-9\.]+)$/) {
      $wait = $1;
  } elsif(/^\+nw$/i || /^\+tk$/i || /^\+java$/i || /^--withwindow$/i) {
      $nowindow = 0;
  } elsif(/^-nw$/i || /^-nojvm$/i || /^--nowindow$/i) {
      $nowindow = 1;
  } elsif(/^-swap$/i) {
      $do_swap = 1;
  } elsif(/^-log/i) {
      $logmode = 1;
  } elsif(/^-contour/i) {
      $contourmode = 1;
  } elsif(/^--help$/i || /^-h$/i || /^-v$/i) {
      print "mcplot [-ps|-psc|-gif] <simfile | detector_file>\n";
      print "       [-pPLOTTER] Output graphics using {PGPLOT,gnuplot,Scilab,Matlab,HTML}\n";
      print "                   The file extension will also set the PLOTTER\n";
      print "       [-overview] Show all plots in a single window\n";
      print "       [-plot]     Show all plots in separate window(s)\n";
      print "       [-iCOMP]    Only show monitors whos name match COMP\n";
      print "       [+nw]       Open {Scilab,Matlab} command window (with Tcl/Java)\n";
      print "       [-nw]       Open {Scilab,Matlab} command window (without Tcl/Java)\n";
      print "       [-log]      Plot results in log10 scale\n";
      print "       [-contour]  Display matrix/images as contour plots\n";
      print "  Plots all monitor data from a simulation, or a single data file.\n";
      print "  When using -ps -psc -gif, the program writes the hardcopy file\n";
      print "  and then exits.\n";
      print "SEE ALSO: mcstas, mcdoc, mcplot, mcrun, mcgui, mcresplot, mcstas2vitess\n";
      print "DOC:      Please visit http://www.mcstas.org\n";
      exit;
  } elsif(/^-([a-zA-Z0-9_]+)$/) {
      $passed_arg_str_quit .= "-$1 ";
  } else {
      $files[$index] = $ARGV[$i];
      $index++;
  }
}

if ($do_plot)     { $passed_arg_str .= "-plot "; }
if ($do_overview) { $passed_arg_str .= "-overview "; }
if ($do_swap)     { $passed_arg_str .= "-swap "; }
if ($logmode)     { $passed_arg_str .= "-log "; }

if ($index == 0) {
  $file = "mcstas";
} else { $file = $files[0]; }
if (-d $file) { # check if dir containing result file
  my $newfile = "$file/mcstas";
  if (-e "$newfile.m" || -e "$newfile.sci" || -e "$newfile.sim" || -e "$newfile.html" || -e "$newfile.nxs") {
    $file = $newfile; }
}

# look if there is only one file type and set plotter to use
if (-e "$file.m" and not -e "$file.sci" and not -e "$file.sim" and not -e "$file.html") { $plotter = "Matlab"; }
if (-e "$file.sci" and not -e "$file.m" and not -e "$file.sim" and not -e "$file.html") { $plotter = "Scilab"; }
if (-e "$file.sim" and not -e "$file.m" and not -e "$file.sci" and not -e "$file.html" and not ($plotter =~ /gnuplot/i)) { $plotter = "PGPLOT"; }
if (-e "$file.html" and not -e "$file.m" and not -e "$file.sci" and not -e "$file.sim") { $plotter = "HTML";   }
if (-e "$file.nxs") { $plotter = "NeXus";   }

# set default extension from plotter
if    ($plotter =~ /Scilab/i) { $default_ext = ".sci"; }
elsif ($plotter =~ /Matlab/i) { $default_ext = ".m"; }
elsif ($plotter =~ /PGPLOT|McStas|gnuplot/i) { $default_ext = ".sim"; }
elsif ($plotter =~ /HTML/i) { $default_ext = ".html"; }
elsif ($plotter =~ /NeXus/i) { $default_ext = ".nxs"; }

# if no extension in file name, add default extension.
if ($file !~ m'\.[^/]*$' && $default_ext) { $file .= $default_ext; }

# set plotter from extension
if ($file =~ m'\.m$')    { $plotter = "Matlab"; }
if ($file =~ m'\.sci$' || $file =~ m'\.sce$') {$plotter = "Scilab"; }
if ($file =~ m'\.sim$' and not($plotter =~ /gnuplot/i))  { $plotter = "PGPLOT"; }
if ($file =~ m'\.html$') { $plotter = "HTML"; }
if ($file =~ m'\.nxs$') { $plotter = "NeXus"; }

# On Win32, chdir to directory containing base filename
if ($Config{'osname'} eq 'MSWin32') {
    my ($basename,$dirname)=fileparse($file);
    $file = $basename;
    if (-d $dirname) {
  chdir $dirname;
    }
}

# Added E. Farhi, March 2003. plotter (pgplot, scilab, matlab, html) -> $file
if ($plotter =~ /Scilab/i && $MCSTAS::mcstas_config{'SCILAB'} ne "no") {
  my $fh;
  # create a temporary scilab execution script
  if ($MCSTAS::mcstas_config{'TEMP'} ne "no") {
    require File::Temp;
    ($fh, $tmp_file) = File::Temp::tempfile("mcplot_tmpXXXXXX", SUFFIX => '.sce');
    if (not defined $fh) { $tmp_file=""; }
  }
  if ($tmp_file eq "") {
    $tmp_file="mcplot_tmp000000.sce";
    $fh = new FileHandle "> $tmp_file";
  }
  if (not defined $fh) { die "Could not open temporary Scilab script $tmp_file\n"; }
  autoflush $fh 1;
  # write the scilab script
  printf $fh "if execstr('stacksize(1e8);','errcatch') then execstr('stacksize(1e7);','errcatch'); end\n";
  printf $fh "exec('$MCSTAS::sys_dir/tools/scilab/mcplot.sci',-1);\n";
  printf $fh "global McPlotTempFile;\nMcPlotTempFile='$tmp_file';\n";
  printf $fh "s=mcplot('$file','$passed_arg_str');\n";
  printf $fh "mprintf('s=mcplot(''$file'',''$passed_arg_str'',)\\n');\n";
  if ($passed_arg_str_quit) {
    printf $fh "quit\n";
  } else {
    printf $fh "if length(s)\n";
    printf $fh "mprintf('mcplot: Simulation data structure from file $file\\n');\n";
    printf $fh "mprintf('        is stored into variable s. Type in ''s'' at prompt to see it !\\n');\n";
    printf $fh "end\n";
  }

  close($fh);
  if ($nowindow) { system("$MCSTAS::mcstas_config{'SCILAB'} -nw -f $tmp_file\n"); }
  else { system("$MCSTAS::mcstas_config{'SCILAB'} -f $tmp_file\n"); }

} elsif ($plotter =~ /Matlab/i && $MCSTAS::mcstas_config{'MATLAB'} ne "no") {
  my $tosend = "$MCSTAS::mcstas_config{'MATLAB'} ";
  if ($nowindow) { $tosend .= "-nojvm -nosplash "; }
  $tosend .= "-r \"addpath('$MCSTAS::sys_dir/tools/matlab');addpath(pwd);s=mcplot('$file','$passed_arg_str $passed_arg_str_quit','$inspect');";
  $tosend .= "disp('s=mcplot(''$file'',''$passed_arg_str $passed_arg_str_quit'',''$inspect'')');";

  if ($passed_arg_str_quit) {
    $tosend .= "exit;\"\n";
  } else {
      $tosend .= "if length(s),";
      $tosend .= "disp('type: help mcplot for this function usage.');";
      $tosend .= "disp('mcplot: Simulation data structure from file $file');";
      $tosend .= "disp('        is stored into variable s. Type in ''s'' at prompt to see it !');";
      $tosend .= "end;\"\n";
    }
  system($tosend);
} elsif ($plotter =~ /HTML|VRML/i && $MCSTAS::mcstas_config{'BROWSER'}) {
  system("$MCSTAS::mcstas_config{'BROWSER'} $file");
} elsif ($plotter =~ /HDF|NeXus/i && $MCSTAS::mcstas_config{'HDFVIEW'} ne "no") {
  system("$MCSTAS::mcstas_config{'HDFVIEW'} $file");
} elsif ($plotter =~ /gnuplot/i) {
  # McStas original mcplot format using GNUPLOT
  require "mcgnuplot.pl";
  gnuplotit($file);
} elsif ($plotter =~ /PGPLOT|McStas/i) {
  # McStas original mcplot using perl/PGPLOT

  require "mcfrontlib2D.pl";
  require "mcplotlib.pl";

  # ADD/MOD: E. Farhi/V. Hugouvieux Feb 18th, 2002 : handle detector files

  # daemon mode is for PGPLOT mode only - quick plot, then exit
  if (!($daemon eq 0)) {
          pgplotit();
	  exit 1;
  }  else {
      pgplotit();
  }

} else {
  if ($plotter =~ /PGPLOT|McStas/i && $MCSTAS::mcstas_config{'PGPLOT'} eq "no") {
    print STDERR "\n******************************************************\n";
    print STDERR "Default / selected PLOTTER is PGPLOT - Problems:\n\n";
    print STDERR "PGPLOT.pm not found on Perl \@INC path\n\nSolutions:\n\n";
    print STDERR "1) Install pgplot + pgperl packages (Unix/Linux/Cygwin) \n";
    print STDERR "2) Rerun mcplot with -p/--plotter set to Scilab/Matlab/VRML \n";
    print STDERR "3) Modify $MCSTAS::perl_dir/mccode_config.perl\n";
    print STDERR "   to set a different default plotter\n";
    print STDERR "4) Set your env variable MCSTAS_FORMAT to set the default\n";
    print STDERR "   data format and plotter\n";
    print STDERR "5) Convert your PGPLOT/McStas files into an other format\n";
    print STDERR "   using the mcformat tool\n";
    print STDERR "******************************************************\n\n";
  }
  print STDERR "Using default Browser $MCSTAS::mcstas_config{'BROWSER'} to view result file $file\n";
  system("$MCSTAS::mcstas_config{'BROWSER'} $file");
}


sub pgplotit {
  my ($sim_file) = $file;
  my ($instr_inf, $sim_inf, $datalist, $sim_error) = read_sim_file($file);
  die "No data in simulation file '$file'"
      unless @$datalist;
  if ($passed_arg_str_quit =~ /-cps|-psc/i) {
    $global_dev = get_device("$file.ps/cps");
    overview_plot($datalist, $passed_arg_str);
          die "Wrote color postscript file '$file.ps' (cps)\n";
  } elsif ($passed_arg_str_quit =~ /-ps/) {
    $global_dev = get_device("$file.ps/ps");
    overview_plot($datalist, $passed_arg_str);
          die "Wrote BW postscript file '$file.ps' (ps)\n";
  } elsif ($passed_arg_str_quit =~ /-ppm/) {
    $global_dev = get_device("$file.ppm/ppm");
    overview_plot($datalist, $passed_arg_str);
          die "Wrote PPM file '$file.ppm' (ppm)\n";
  } elsif ($passed_arg_str_quit =~ /-png/) {
    $global_dev = get_device("$file.png/png");
    overview_plot($datalist, $passed_arg_str);
          die "Wrote PNG file '$file.png' (png)\n";
  } elsif ($passed_arg_str_quit =~ /-gif/) {
    $global_dev = get_device("$file.gif/gif");
    overview_plot($datalist, $passed_arg_str);
          die "Wrote GIF file '$file.gif' (gif)\n";
  }

  if ($daemon eq 0) {
      print "Click on a plot for full-window view.\n" if @$datalist > 1;
      print "Press key for hardcopy (in graphics window), 'Q' to quit
  'P' BW postscript
  'C' color postscript
  'N' PNG file
  'M' PPM file
  'G' GIF file
  'L' Toggle log10 plotting mode
  'T' Toggle contour plotting mode
  'Q' quit\n";
  } else {
      $global_spec = "$ENV{'PGPLOT_DEV'}";
      $global_dev = get_device($global_spec);
      PGPLOT::pgask(0);
      overview_plot($datalist, $passed_arg_str);
  }
  my $global_open = 0;
  if ($daemon eq 0) {
      for(;;) {
	  if ($global_open == 0) {
	      if ($Config{'osname'} eq 'MSWin32') {
		  $global_open = 1;
	      }
	      $global_spec = "$ENV{'PGPLOT_DEV'}";
	      $global_dev = get_device($global_spec);
	      PGPLOT::pgask(0);
	  }
          my ($cc,$cx,$cy,$idx);
          if ($logmode == 1) { if ($passed_arg_str !~ /-log/i) { $passed_arg_str .= "-log "; } }
          else { $passed_arg_str =~ s|-log||; }
          if ($contourmode == 1) { if ($passed_arg_str !~ /-contour/i) { $passed_arg_str .= "-contour "; } }
          else { $passed_arg_str =~ s|-contour||; }
          # Do overview plot, letting user select a plot for full-screen zoom.
          ($cc,$idx) = overview_plot($datalist, "$passed_arg_str -interactive ");
          last if $cc =~ /[xq]/i;        # Quit?
          if($cc =~ /[pcngm]/i) {        # Hardcopy?
              my $ext="ps";
              my $dev = ($cc =~ /c/i) ? "cps" : "ps";
              if($cc =~ /g/i) { $dev = "gif"; $ext="gif"; }
              if($cc =~ /n/i) { $dev = "png"; $ext="png"; }
              if($cc =~ /m/i) { $dev = "ppm"; $ext="ppm"; }
	      system("$MCSTAS::mcstas_config{'PLOTCMD'}$MCSTAS::mcstas_config{'suffix'} --format=McStas $passed_arg_str -$dev $file");
              next;
          }
          elsif($cc =~ /[l]/i) {        # toggle log mode
            if ($logmode == 0) { $logmode=1; }
            else { $logmode=0; $passed_arg_str =~ s|-log||; }
            next;
          }
          elsif($cc =~ /[t]/i) {        # toggle contour plot mode
            if ($contourmode == 0) { $contourmode=1; }
            else { $contourmode=0; $passed_arg_str =~ s|-contour||; }
            next;
          }
          # now do a full-screen version of the plot selected by the user.
          ($cc, $cx, $cy) = single_plot($datalist->[$idx], "$passed_arg_str -interactive ");
          last if $cc =~ /[xq]/i;        # Quit?
          if($cc =~ /[pcngm]/i) {        # Hardcopy?
              my $ext="ps";
              my $dev = ($cc =~ /c/i) ? "cps" : "ps";
              if($cc =~ /g/i) { $dev = "gif"; $ext="gif"; }
              if($cc =~ /n/i) { $dev = "png"; $ext="png"; }
              if($cc =~ /m/i) { $dev = "ppm"; $ext="ppm"; }
              my $filename = "$datalist->[$idx]{'Filename'}";
	      print "Spawning plot of $filename \n";
	      system("$MCSTAS::mcstas_config{'PLOTCMD'}$MCSTAS::mcstas_config{'suffix'} --format=McStas $passed_arg_str $filename -$dev");
          }
          if($cc =~ /[l]/i) {        # toggle log mode
            if ($logmode == 0) { $logmode=1; }
            else { $logmode=0; $passed_arg_str =~ s|-log||; }
            next;
          }
          if($cc =~ /[t]/i) {        # toggle contour plot mode
            if ($contourmode == 0) { $contourmode=1; }
            else { $contourmode=0; $passed_arg_str =~ s|-contour||; }
            next;
          }
      }
  }
}
