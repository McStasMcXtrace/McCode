#! /usr/bin/perl
#
# Generic scan of variables.
#
# Usage:
#  [perl] gscan.pl numpoint sim-exec file (VAL | VAL,VAL) ...
#

require "mcstas-scanlib.pl";

die "Usage: [perl] gscan.pl numpoint sim-exec file (VAL | VAL,VAL) ..."
    unless $#ARGV >= 3;

($npoints, $simprog, $simfile, @VALS) = @ARGV;

$i = 0;
$j = 0;
@values = ();
@scanned = ();
@minval = ();
@maxval = ();
while(@VALS) {
    $v = shift @VALS;
    if($v =~ /(.*),(.*)/) {
	# Variable to scan from min to max.
	$minval[$j] = $1;
	$maxval[$j] = $2;
	$scanned[$j] = $i;
	$j++;
    } else {
	$values[$i] = $v;
    }
    $i++;
}
$numvars = $i;

# Now do the scan.
open(OUT, ">$simfile");
for($point = 0; $point < $npoints; $point++) {
    $out = "";
    for($j = 0; $j <= $#scanned; $j++) {
	$values[$scanned[$j]] =
	    ($maxval[$j] - $minval[$j])/($npoints - 1)*$point + $minval[$j];
	$out .= "$values[$scanned[$j]] ";
    }
    $cmd = "echo";
    for($i = 0; $i < $numvars; $i++) {
	$cmd .= " $values[$i]";
    }
    $cmd .= " | $simprog";
    print "running '$cmd'\n";
    open (SIM, "$cmd |") || die "Could not run mcstas simulation";
    while(<SIM>) {
	if(m'I = ([0-9eE+-.]+) \(([0-9eE+-.]+) neutrons detected\)') {
	    $sim_I = $1;
	    $sim_N = $2;
	    $sim_err = ($sim_N ? $sim_I/sqrt($sim_N) : 1);
	} elsif(m'Set value of instrument parameter') {
	    next;
	}
	print;
    }
    close(SIM);
    print OUT "$out $sim_I $sim_err\n";
}
close(OUT);
