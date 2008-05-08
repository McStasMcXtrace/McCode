#! /usr/bin/perl
#
# Generic scan of variables (obsolete). Rather use mcrun.
#
# Usage:
#  [perl] gscan.pl numpoint sim-exec file (VAL | VAL,VAL) ...
#
# Using mcrun.pl instead of this script is recommended
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

die "Usage:
  [perl] gscan.pl numpoint numneutrons sim-exec file PARM=(VAL|VAL,VAL) ..."
    unless $#ARGV >= 4;

($npoints, $numneutrons, $simprog, $simfile, @VALS) = @ARGV;

# Turn off buffering on stdout. This improves the use with the Unix
# "tee" program to create log files of scans.
$| = 1;

$i = 0;
$j = 0;
@parmname = ();
@values = ();
@scanned = ();
@minval = ();
@maxval = ();
while(@VALS) {
    $v = shift @VALS;
    if($v =~ /^([a-zA-Z0-9_]+)=(.+),(.+)$/) {
	# Variable to scan from min to max.
	$parmname[$i] = $1;
	$minval[$j] = $2;
	$maxval[$j] = $3;
	$scanned[$j] = $i;
	if($minval[j] != $maxval[j] && $npoints < 2) {
	    die "gscan: Cannot scan variable $parmname[$i] using only one data point";
	}
	$j++;
    } elsif($v =~ /^([a-zA-Z0-9_]+)=(.+)$/) {
	$parmname[$i] = $1;
	$values[$i] = $2;
    } else {
	die "gscan: Invalid parameter specification '$v'";
    }
    $i++;
}
$numvars = $i;

# Now do the scan.
open(OUT, ">$simfile");
$firsttime = 1;
$outputs = "";
@youts = ();
for($point = 0; $point < $npoints; $point++) {
    $out = "";
    for($j = 0; $j <= $#scanned; $j++) {
	$values[$scanned[$j]] =
	    ($maxval[$j] - $minval[$j])/($npoints - 1)*$point + $minval[$j];
	$out .= "$values[$scanned[$j]] ";
	$outputs .= "$parmname[$scanned[$j]] " if $firsttime
    }
    $cmd = "$simprog --ncount=$numneutrons";
    for($i = 0; $i < $numvars; $i++) {
	$cmd .= " $parmname[$i]=$values[$i]";
    }
    print "Running '$cmd'\n";
    open (SIM, "$cmd |") || die "gscan: Could not run mcstas simulation";
    $got_error = 0;
    while(<SIM>) {
	chomp;
	if(/Detector: ([^ =]+_I) *= *([^ =]+) ([^ =]+_ERR) *= *([^ =]+) ([^ =]+_N) *= *([^ =]+) *(?:"[^"]+" *)?$/) {
	    $sim_I = $2;
	    $sim_err = $4;
	    $sim_N = $6;
	    $out .= " $sim_I $sim_err $sim_N";
	    if($firsttime) {
		$outputs .= " $1 $3 $5";
		push @youts, "($1,$3,$5)";
	    }
	} elsif(m'^Error:') {
	    $got_error = 1;
	}
	print "$_\n";
    }
    close(SIM);
    die "gscan: Exit due to error returned by simulation program" if $got_error;
    print OUT "$out\n";
    $firsttime = 0;
}
close(OUT);
print "Output file: '$simfile'\nOutput parameters: $outputs\n";

# Write gscan.sim information file.
$infofile = "gscan.sim";
open(SIM, ">$infofile") || die "gscan: Failed to write info file '$infofile'";
$scannedvars = join ", ", (map { $parmname[$scanned[$_]]; } (0..$#scanned));
$xvars = join " ", (map { $parmname[$scanned[$_]]; } (0..$#scanned));
$yvars = join " ", @youts;
print SIM <<END;
begin data
  type: array_1d($npoints)
  title: 'Scan of $scannedvars'
  xvars: $xvars
  yvars: $yvars
  xlimits: $minval[0] $maxval[0]
  filename: $simfile
  params: $outputs
end data
END
close SIM;
