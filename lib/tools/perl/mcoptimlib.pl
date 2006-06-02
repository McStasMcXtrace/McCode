# Library of McStas runtime perl functions
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
use Config;

use Math::Amoeba qw(MinimiseND);
require "mcstas_config.perl";

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
    $optim_variables = "step " . $variables;
    @optim_youts = @youts;
    $optim_variables .= " criteria null";
    push @optim_youts, " (criteria,null)";
  }
  @optim_last = @p;
  $optim_iterations++;

  return -$y;
}

1;