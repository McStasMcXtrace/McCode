#! /usr/bin/perl -w

use FileHandle;

use lib "/usr/local/lib/mcstas";
use lib $ENV{"MCSTAS"};

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
