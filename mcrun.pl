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

my ($sim_def, $force_compile, $data_dir, $data_file);
my $out_file;
my $numpoints = 1;
my @params = ();
my @options = ();

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
    } elsif(/^--(ascii-only|help|info|trace)$/) {
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

$out_file = get_out_file($sim_def, $force_compile);
exit(1) unless $out_file;

push @options, "--dir=$data_dir" if $data_dir;
push @options, "--file=$data_file" if $data_file;

# Make sure that the current directory appears first in the path;
# contrary to normal use, this is what the user expects here.
$ENV{PATH} = $ENV{PATH} ? ".:$ENV{PATH}" : ".";

print "Running simulation '$out_file' ...\n";
my @cmdlist = ($out_file, @options, map("$_->[0]=$_->[1]", @params));
print join(" ", @cmdlist), "\n";
exec @cmdlist;
