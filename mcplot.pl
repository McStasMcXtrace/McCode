#! /usr/bin/perl -w

use FileHandle;
use PDL;
use PDL::Graphics::PGPLOT;
use PGPLOT;

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

require "mcfrontlib.pl";
require "mcplotlib.pl";


my ($file) = @ARGV;
$file = "mcstas.sim" unless $file;
$file = "$file/mcstas.sim" if -d $file;
my ($instr_inf, $sim_inf, $datalist) = read_sim_file($file);
die "No data in simulation file '$file'"
    unless @$datalist;

print "Click on a plot for full-window view.\n" if @$datalist > 1;
print "Type 'P' (in graphics window) for hardcopy, 'Q' to quit.\n";

for(;;) {
    my ($cc,$cx,$cy,$idx);
    # Do overview plot, letting user select a plot for full-screen zoom.
    ($cc,$idx) = overview_plot("/xserv", $datalist, 1);
    last if $cc =~ /[xq]/i;	# Quit?
    if($cc =~ /[pc]/i) {	# Hardcopy?
	my $dev = ($cc =~ /c/i) ? "cps" : "ps";
	overview_plot("mcstas.ps/$dev", $datalist, 0);
	print "Wrote postscript file 'mcstas.ps'\n";
	next;
    }
    # now do a full-screen version of the plot selected by the user.
    ($cc, $cx, $cy) = single_plot("/xserv", $datalist->[$idx], 1);
    last if $cc =~ /[xq]/i;	# Quit?
    if($cc =~ /[pc]/i) {	# Hardcopy?
	my $dev = ($cc =~ /c/i) ? "cps" : "ps";
	my $filename = "$datalist->[$idx]{'Component'}.ps";
	single_plot("$filename/$dev", $datalist->[$idx], 0);
	print "Wrote postscript file '$filename'\n";
    }	
}
