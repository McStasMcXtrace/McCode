#! /usr/bin/perl -w

# Determine the path to the McStas system directory. This must be done
# in the BEGIN block so that it can be used in a "use lib" statement
# afterwards.
BEGIN {
    if($ENV{"MCSTAS"}) {
	$MCSTAS::sys_dir = $ENV{"MCSTAS"};
    } else {
	$MCSTAS::sys_dir = "/usr/local/lib/mcstas";
    }
}
use lib $MCSTAS::sys_dir;

use FileHandle;
use strict;

require "mcrunlib.pl";

# Turn off buffering on stdout. This improves the use with the Unix
# "tee" program to create log files of scans.
autoflush STDOUT 1;

# Various parameters determined by the command line.
my ($sim_def, $force_compile, $data_dir, $data_file);
my $numpoints = 1;		# Number of points in scan (if any)
my @params = ();		# List of input parameters
my %vals;			# Hash of input parameter values
my @options = ();		# Additional command line options

# Name of compiled simulation executable.
my $out_file;
# Instrument information data structure.
my $instr_info;


# Add an input parameter assignment to the current set, overriding any
# previously assigned value.
sub set_inputpar {
    my ($par, $val) = @_;
    push @params, $par unless $vals{$par};
    $vals{$par} = $val;
}

# Set input parameter from "PAR=VAL" type string specification.
sub set_inputpar_text {
    my ($text) = @_;
    if($text =~ /^([a-zæøåA-ZÆØÅ0-9_]+)\=(.*)$/) {
	set_inputpar($1, $2);
    } else {
	die "Invalid input parameter specification '$text'";
    }
}

# Read input parameters from parameter file.
sub read_inputpar_from_file {
    my ($filename) = @_;
    open(IN, "<$filename") || die "Failed to open file '$filename'";
    while(<IN>) {
	for $p (split) {
	    set_inputpar_text($p);
	}
    }
}    

# Handle the command line arguments (options, input parameters,
# instrument definition name).
sub parse_args {
    my $i;
    for($i = 0; $i < @ARGV; $i++) {
	$_ = $ARGV[$i];
	# Options specific to mcrun.
	if(/^--force-compile$/ || /^-c$/) {
	    $force_compile = 1;
	} elsif(/^--numpoints\=(.*)$/ || /^-N(.+)$/) {
	    $numpoints = $1;
	} elsif(/^--numpoints$/ || /^-N$/) {
	    $numpoints = $ARGV[++$i];
	}
	# Standard McStas options needing special treatment by mcrun.
	elsif(/^--dir\=(.*)$/ || /^-d(.+)$/) {
	    $data_dir = $1;
	} elsif(/^--dir$/ || /^-d$/) {
	    $data_dir = $ARGV[++$i];
	}
	elsif(/^--file\=(.*)$/ || /^-f(.+)$/) {
	    $data_file = $1;
	} elsif(/^--file$/ || /^-f$/) {
	    $data_file = $ARGV[++$i];
	}
	# Standard McStas options passed through unchanged to simulations.
	elsif(/^--(seed|ncount)\=(.*)$/) {
	    push @options, "--$1=$2";
	} elsif(/^-([sn])(.+)$/) {
	    push @options, "-$1$2";
	} elsif(/^--(seed|ncount)$/) {
	    push @options, "--$1=$ARGV[++$i]";
	} elsif(/^-([sn])$/) {
	    push @options, "-$1$ARGV[++$i]";
	} elsif(/^--(ascii-only|help|info|trace|no-output-files)$/) {
	    push @options, "--$1";
	} elsif(/^-([ahit])$/) {
	    push @options, "-$1";
	}
	# Non-option arguments.
	elsif(/^-/) {		# Unrecognised option
	    die "Unknown option \"$_\"";
	} elsif(/^([a-zæøåA-ZÆØÅ0-9_]+)\=(.*)$/) {
	    push @params, [$1,$2];
	} else {			# Name of simulation definition
	    if($sim_def) {
		die "Only a single instrument definition may be given";
	    } else {
		$sim_def = $_;
	    }
	}
    }
    die "No instrument definition name given" unless $sim_def;
    die "Number of points must be at least 1" unless $numpoints >= 1;
}

# Check the input parameter specifications for variables to scan.
sub check_input_params {
    my $i = 0;
    my $j = 0;
    my @scanned = ();
    my @minval = ();
    my @maxval = ();
    my $v;
    for $v (@params) {
	if($v->[1] =~ /^(.+),(.+)$/) {
	    # Variable to scan from min to max.
	    $minval[$j] = $1;
	    $maxval[$j] = $2;
	    $scanned[$j] = $i;
	    if($minval[$j] != $maxval[$j] && $numpoints == 1) {
		die "Cannot scan variable $v->[0] using only one data point.
Please use -N to specify the number of points.";
	    }
	    $j++;
	} elsif($v->[1] =~ /^(.+)$/) {
	    # Constant variable (no action).
	} else {
	    die "Invalid parameter specification '$v->[1]' for parameter $v->[0]";
	}
	$i++;
    }
    return { VARS => \@scanned, MIN => \@minval, MAX => \@maxval };
}

sub exec_sim {
    my @cmdlist = ($out_file, @options, map("$_->[0]=$_->[1]", @params));
    # ToDo: This is broken in that it does not show the proper quoting
    # that would be necessary if the user actually wantet to run the
    # command manually. (Note that the exec() call is correct since it
    # does not need any quoting).
    print join(" ", @cmdlist), "\n";
    exec @cmdlist;
}

# Handle the case of a single run of the simulation (no -N option).
sub do_single {
    push @options, "--dir=$data_dir" if $data_dir;
    push @options, "--file=$data_file" if $data_file;

    print "Running simulation '$out_file' ...\n";
    exec_sim() || die "Failed to run simulation";
}

# Do a scan: Run a series of simulations, varying one or more input
# parameters.
sub do_scan {
    my ($info) = @_;
    my $simfile = "";
    # Create the output directory if requested.
    if($data_dir) {
	if(mkdir($data_dir, 0777)) {
	    $simfile .= "$data_dir/";
	} else {
	    die "Error: unable to create directory '$data_dir'.\n(Maybe the directory already exists?)";
	}
    }
    if($data_file) {
	die "The -f/--file option is not yet supported by mcrun";
    }
    $simfile .= "mcstas.dat";
    open(OUT, ">$simfile");
    my $firsttime = 1;
    my $outputs = "";
    my @youts = ();
    my $point;
    for($point = 0; $point < $numpoints; $point++) {
	my $out = "";
	my $j;
	for($j = 0; $j < @{$info->{VARS}}; $j++) {
	    my $i = $info->{VARS}[$j]; # Index of variable to be scanned
	    $params[$i]->[1] =
		($info->{MAX}[$j] - $info->{MIN}[$j])/($numpoints - 1)*$point +
		    $info->{MIN}[$j];
	    $out .= "$params[$i]->[1] ";
	    $outputs .= "$params[$i]->[0] " if $firsttime
	    }
	my $got_error = 0;
	my $pid = open(SIM, "-|");
	die "Failed to spawn simulation command" unless defined($pid);
	if($pid) {		# Parent
	    while(<SIM>) {
		chomp;
		if(/Detector: ([^ =]+_I) *= *([^ =]+) ([^ =]+_ERR) *= *([^ =]+) ([^ =]+_N) *= *([^ =]+) *(?:"[^"]+" *)?$/) { # Quote hack -> ") {
		    my $sim_I = $2;
		    my $sim_err = $4;
		    my $sim_N = $6;
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
	} else {		# Child
	    open(STDERR, ">&STDOUT") || die "Can't dup stdout";
	    exec_sim();
	}
	my $ret = close(SIM);
	die "Exit due to error returned by simulation program"
	    if $got_error || (! $ret && ($? != 0 || $!));
	print OUT "$out\n";
	$firsttime = 0;
    }
    close(OUT);
    print "Output file: '$simfile'\nOutput parameters: $outputs\n";

    # Write gscan.sim information file.
    my $infofile = "gscan.sim";
    open(OUT, ">$infofile") || die "Failed to write info file '$infofile'";
    my $scannedvars = join ", ", map($params[$_][0], @{$info->{VARS}});
    my $xvars = join " ", map($params[$_][0], @{$info->{VARS}});
    my $yvars = join " ", @youts;
    print OUT <<END;
begin data
  type: array_1d($numpoints)
  title: 'Scan of $scannedvars'
  xvars: $xvars
  yvars: $yvars
  xlimits: $info->{MIN}[0] $info->{MAX}[0]
  filename: $simfile
  params: $outputs
end data
END
    close(OUT);
}

		    ##########################
		    # Start of main program. #
		    ##########################

parse_args();			# Parse command line arguments
my $scan_info = check_input_params(); # Get variables to scan, if any
$out_file = get_out_file($sim_def, $force_compile);
exit(1) unless $out_file;
$instr_info = get_sim_info($out_file);

# Make sure that the current directory appears first in the path;
# contrary to normal use, this is what the user expects here.
$ENV{PATH} = $ENV{PATH} ? ".:$ENV{PATH}" : ".";

if($numpoints == 1) {
    do_single();
} else {
    do_scan($scan_info);
}
