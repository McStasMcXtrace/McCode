#! /usr/bin/perl -w

use FileHandle;

use lib "/usr/local/lib/mcstas";
use lib $ENV{"MCSTAS"};

require "mcstas_config.perl";

# Fetch first argument: simulation definition.
$sim_def = shift @ARGV;
die "No simulation specified" unless $sim_def;
if(! (-e $sim_def) && (-e "$sim_def.instr")) {
    $sim_def .= ".instr";
}
die "Simulation definition '$sim_def' not found" unless -r $sim_def;
die "File '$sim_def' looks like a binary file" if -B $sim_def;

if($sim_def =~ /(.*)\.instr$/) {
    $base_name = $1;
} elsif($sim_def =~ /(.*)\.c$/) {
    die "You must give the name of the instrument definition, not the C file";
} elsif($sim_def =~ /(.*)\.out$/) {
    die "You must give the name of the instrument definition,
not the compiled executable";
} else {
    $base_name = $sim_def;
}
$c_name = "$base_name.c";
$out_name = "$base_name.out";

# Translate into C if newer than previous C version.
$sim_age = -M $sim_def;
$c_age = -e $c_name ? -M $c_name : -1;
$out_age = -e $out_name ? -M $out_name : -1;
if($c_age < 0 || $c_age > $sim_age) {
    print "Translating instrument definition into C ...\n";
    $cmd = "mcstas -t -o \"$c_name\" \"$sim_def\"";
    print "$cmd\n";
    $exit_val = (system($cmd)) >> 8;
    if($exit_val) {
	print STDOUT "** Error exit **\n";
	exit 1;
    }
    $c_age = -M $c_name;
    $out_age = -1;		# Force recompilation.
}
die "Could not translate simulation into C" unless -e $c_name;

# Translate into C if newer than previous C version.
if($out_age < 0 || $out_age > $c_age) {
    print "Compiling simulation ...\n";
    $cmd = "$mcstas_config{CC} $mcstas_config{CFLAGS} " .
	"-o \"$out_name\" \"$c_name\" -lm";
    print "$cmd\n";
    $exit_val = (system($cmd)) >> 8;
    die "** Error exit **" if $exit_val;
}
die "Could not compile simulation" unless -e $out_name;

print "Running simulation ...\n";
exec ($out_name, @ARGV);
