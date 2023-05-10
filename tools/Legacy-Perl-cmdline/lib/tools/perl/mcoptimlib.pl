# Library of McStas runtime perl functions
#
#   This file is part of the McStas neutron ray-trace simulation package
#   Copyright (C) 1997-2006, All rights reserved
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
use Config;

use Math::Amoeba qw(MinimiseND);
require "mccode_config.perl";

# Overload with user's personal config
if ($ENV{"HOME"} && -e $ENV{"HOME"}."/.mcstas/mccode_config.perl") {
  require $ENV{"HOME"}."/.mcstas/mccode_config.perl";
}

our $optim_bestvalue=0;
our $optim_variables;
our @optim_datablock = ();
our @optim_first;
our @optim_youts=();

# must run optimization steps as scans in oder to extract Detector lines
# from output: (mcrun::do_scan) mcrun -N 1
sub minimize_function {
  my (@p) = @_; # array of instrument parameters at optimization step
  my $j;
  # function must return a value to be maximized
  $y = 0;

  my @scanned = ();
  my @minval = ();
  my @maxval = ();

  my $out = "$optim_iterations ";
  my @youts=();

  # create a copy of the global $scan_info within limits
  for($j = 0; $j < @{$scan_info->{VARS}}; $j++) {
    if    ($p[$j] > $scan_info->{MAX}[$j]) { $p[$j] = $scan_info->{MAX}[$j]; }
    elsif ($p[$j] < $scan_info->{MIN}[$j]) { $p[$j] = $scan_info->{MIN}[$j]; }
    $minval[$j]  = $p[$j];
    $maxval[$j]  = $p[$j];
    $scanned[$j] = $scan_info->{VARS}[$j];
    $out .= "$p[$j] "; # parameter values for this iteration step
  }

  # assemble $scan_info from optim_info and optimization step ($numpoints=1)
  my $optim_info = { VARS => \@scanned, MIN => \@minval, MAX => \@maxval };

  # execute single iteration step (do_scan). returns [params_val I err ... I err]
  ($datablock, $variables, @youts) = do_scan($optim_info);
  my @vars = split(" ", $variables);
  my @vals = split(" ", $datablock);

  # search optim_names in variables
  # get list of monitors to maximize and loop
  my $found_monitor=0;
  for ($j = @{$scan_info->{VARS}}; $j < @vars; $j += 2) {
    if ($optim_iterations == 0) { $optim_first[$j] = 0; }
    my $value = $vals[$j];
    if ($optim_first[$j] == 0 && $value != 0) {
      $optim_first[$j] = abs($value);
    }
    if ($optim_first[$j] > 0) {
      $value /= $optim_first[$j];
    }
    if ($optim_flag > 1) { # all monitors
      $y = $y + $value; # add all values for criteria
      $found_monitor++;
    } else { # selected monitors
      my $i;
      for($i = 0; $i < @optim_names; $i++) {
        my $this_name = $optim_names[$i];
        # add each value of monitor to $y
        if ($vars[$j] eq "$this_name" . "_I") {
          $y = $y + $value; # add corresponding value to found name for criteria
          $found_monitor++;
        }
      }
    }
    $out .= " $vals[$j] $vals[$j+1]";
  } # end for j
  die "optimization: ERROR: selected component is not a monitor\n" unless $found_monitor;
  $out .= " $y 0";
  push @optim_datablock, "$out\n";
  if ($optim_iterations == 0) {
    $optim_variables = "Point " . $variables;
    @optim_youts = @youts;
    $optim_variables .= " criteria null";
    push @optim_youts, " (criteria,null)";
  }
  if ($optim_iterations == 0 || $y >= $optim_bestvalue) {
    @optim_best      = @p;
    $optim_bestvalue = $y;
  }
  $optim_iterations++;
  
  # Handle writing of mcplot-compatible optim output file
  $numpoints = $optim_iterations;
  if (!$optfile) {
    if ($MCSTAS::mcstas_config{'TEMP'} ne "no") {
      require File::Temp;
      ($OPT, $optfile) = File::Temp::tempfile("mcoptim"."_XXXX", SUFFIX => '.dat'); # throw file handle
    } else {
      $optfile = int(rand(9999));
      $optfile = "mcoptim"."_$optfile.dat";
    }
  } else {
    $OPT = new FileHandle;
  }
  open($OPT, ">$optfile");
  autoflush $OPT 1;
  push @opt_out, "$optim_iterations $y 0\n";
  output_dat_header($OPT, "# ",$scan_info, '(criteria,null)', 'Point criteria null', $optfile);
  print $OPT "@opt_out\n";
  close($OPT);
  
  print "Optimization $optim_iterations criteria=$y\n";

  return -$y;
}

sub sighandler_optim {
  my $signame = shift;
  print "mcrun: Recieved signal $signame during optimization (iteration $optim_iterations).\n";
  if ($signame eq "INT" || $signame eq "TERM") {
    save_optimization(0);
    exit(0);
  } else {
    print "# Optimization history:\n";
    my $datablock = join(" ", @optim_datablock);  # mcoptimlib-> @optim_datablock, @optim_youts, $optim_variable
    print "# variables: $optim_variables\n";
    print "$datablock\n";
  }
}

# function to save optimization history
sub save_optimization {
  my ($final_call) = @_;
  my ($j);

  # constrain within limits
  for($j = 0; $j < @{$scan_info->{VARS}}; $j++) {
    if    ($optim_best[$j] > $scan_info->{MAX}[$j]) { $optim_best[$j] = $scan_info->{MAX}[$j]; }
    elsif ($optim_best[$j] < $scan_info->{MIN}[$j]) { $optim_best[$j] = $scan_info->{MIN}[$j]; }
  }

  # display result
  print "Iteration $optim_iterations parameters:\n";

  for($j = 0; $j < @{$scan_info->{VARS}}; $j++) {
    $i = $scan_info->{VARS}[$j]; # Index of variable to be scanned
    print "$params[$i]=$optim_best[$j] ";
  }
  print "\n";

  # reactivate $data_dir and relaunch minimize_function to fill directory
  # mcstas.sim is last iteration result
  if ($data_dir_saved) {
    $data_dir = $data_dir_saved;
    print "Generating optimized configuration in --dir=$data_dir\n";
    minimize_function(@optim_best); # this also sets scan-info by calling do_scan
  }
  # output optimization results as a scan
  $numpoints = $optim_iterations;
  my $prefix = "";
  if($data_dir) { $prefix = "$data_dir/"; }
  my $datfile = "mcstas.dat";
  my $simfile = $datfile;
  $simfile =~ s/\.dat$//;         # Strip any trailing ".dat" extension ...
  $simfile .= $format_ext;        # ... and add ".sim|m|sci" extension.
  my $datablock = join(" ", @optim_datablock);  # mcoptimlib-> @optim_datablock, @optim_youts, $optim_variable

  # Only initialize / use $DAT datafile if format is PGPLOT
  if ($MCSTAS::mcstas_config{'PLOTTER'} =~ /PGPLOT|McStas/i) {
    my $DAT = new FileHandle;
    open($DAT, ">${prefix}$datfile");
    output_dat_header($DAT, "# ", $scan_info, \@optim_youts, $optim_variables, $datfile);
    print $DAT "$datablock\n";
    close($DAT);
  } else {
    $datfile = $simfile;             # Any reference should be to the simfile
  }

  output_sim_file("${prefix}$simfile", $scan_info, \@optim_youts, $optim_variables, $datfile, $datablock);

  print "Optimization file: '${prefix}$datfile'\nOptimized parameters: $optim_variables\n";
  undef($data_dir);
  $numpoints = 1;
}

1;
