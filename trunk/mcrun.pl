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

require "mcrunlib.pl";

# Fetch first argument: simulation definition.
$sim_def = shift @ARGV;

$out_file = get_out_file($sim_def);
exit(1) unless $out_file;

# Make sure that the current directory appears first in the path;
# contrary to normal use, this is what the user expects here.
$ENV{PATH} = $ENV{PATH} ? ".:$ENV{PATH}" : ".";

print "Running simulation '$out_file' ...\n";
exec ($out_file, @ARGV);
