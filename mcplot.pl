#! /usr/bin/perl -w

use FileHandle;
use PDL;
use PDL::Graphics::PGPLOT;
use PGPLOT;

if($ENV{"MCSTAS"}) {
    use lib $ENV{"MCSTAS"};
} else {
    use lib "/usr/local/lib/mcstas";
}

require "mcfrontlib.pl";
require "mcplotlib.pl";


my ($file) = @ARGV;
$file = "mcstas.sim" unless $file;
my ($instr_inf, $sim_inf, $datalist) = read_sim_file($file);

for(;;) {
    my ($cc,$cx,$cy,$idx);
    # Do overview plot, letting user select a plot for full-screen zoom.
    ($cc,$idx) = overview_plot("/xserv", $datalist, 1);
    last if $cc =~ /[xq]/i;	# Quit?
    if($cc =~ /p/i) {		# Hardcopy?
	overview_plot("mcstas.ps/cps", $datalist, 0);
	print "Wrote postscript file 'mcstas.ps'\n";
	next;
    }
    # now do a full-screen version of the plot selected by the user.
    ($cc, $cx, $cy) = single_plot("/xserv", $datalist->[$idx], 1);
    last if $cc =~ /[xq]/i;	# Quit?
    if($cc =~ /p/i) {		# Hardcopy?
	my $filename = "$datalist->[$idx]{'Component'}.ps";
	single_plot("$filename/cps", $datalist->[$idx], 0);
	print "Wrote postscript file '$filename'\n";
    }	
}
