#! /usr/bin/perl
#
# Generic scan of variables.
#
# Usage:
#  [perl] gscan.pl numpoint sim-exec file (VAL | VAL,VAL) ...
#

die "Usage:
  [perl] gscan.pl numpoint numneutrons sim-exec file PARM=(VAL|VAL,VAL) ..."
    unless $#ARGV >= 4;

($npoints, $numneutrons, $simprog, $simfile, @VALS) = @ARGV;

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
	$j++;
    } elsif($v =~ /^([a-zA-Z0-9_]+)=(.+)$/) {
	$parmname[$i] = $1;
	$values[$i] = $2;
    } else {
	die "Invalid parameter specification '$v'";
    }
    $i++;
}
$numvars = $i;

# Now do the scan.
open(OUT, ">$simfile");
$firsttime = 1;
$outputs = "";
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
    print "running '$cmd'\n";
    open (SIM, "$cmd |") || die "Could not run mcstas simulation";
    $got_error = 0;
    while(<SIM>) {
	chomp;
	if(m'^Detector: ([^ =]+_I) *= *([^ =]+) ([^ =]+_ERR) *= *([^ =]+) *$') {
	    $sim_I = $2;
	    $sim_err = $4;
	    $out .= " $sim_I $sim_err";
	    $outputs .= " $1 $3" if $firsttime;
	} elsif(m'^Error:') {
	    $got_error = 1;
	}
	print "$_\n";
    }
    close(SIM);
    die "Exit due to error returned by simulation programn" if $got_error;
    print OUT "$out\n";
    $firsttime = 0;
}
close(OUT);
print "Output file: '$simfile'\nOutput parameters: $outputs\n";
