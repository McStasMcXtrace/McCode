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
    $MCSTAS::perl_dir = "$MCSTAS::sys_dir/tools/perl"
}
use lib $MCSTAS::perl_dir;

require "mcfrontlib.pl";
require "mcplotlib.pl";

# ADD/MOD: E. Farhi Sep 21th, 2001 : handle -ps and -psc for automatic 
# print and exit
my ($print_color, $print_bw, $print_gif);
my ($file, $files);
my ($index);
$index = 0;
for($i = 0; $i < @ARGV; $i++) {
        $_ = $ARGV[$i];
  # Options specific to mcplot.
        if(/^-psc$/ || /^-c$/) {
            $print_color = 1;
        } elsif(/^-ps$/ || /^-p$/) {
            $print_bw = 1;
        } elsif(/^-gif$/ || /^-g$/) {
            $print_gif = 1;
  } elsif(/^--help$/ || /^-h$/ || /^-v$/) {
      print "mcplot [-ps|-psc|-gif|-v] <simfile | detector_file>\n";
      print "  Plots all monitor data from a simulation, or a single data file.\n";
      print "  When using -ps -psc -gif, the program writes the hardcopy file\n";
      print "  and then exits.\n";
      print "SEE ALSO: mcstas, mcdisplay, mcrun, mcresplot, mcstas2vitess, mcgui\n";
      exit;
        } else {
      $files[$index] = $ARGV[$i];
      $index++;
  }
}
# ADD/MOD: E. Farhi/V. Hugouvieux Feb 18th, 2002 : handle detector files
if ($index == 0) { 
  $file = "mcstas.sim"; 
} else { $file = $files[0]; }
$file = "$file/mcstas.sim" if -d $file;
my ($sim_file) = $file;
my ($instr_inf, $sim_inf, $datalist, $sim_error) = read_sim_file($file);
if ($sim_error !~ "no error") {
  $file = mcpreplot($files);
  my ($det_file) = $file;
  ($instr_inf, $sim_inf, $datalist, $det_error) = read_sim_file($file);
  if ($det_error !~ "no error") {
    print "'$sim_file':'$sim_error'";
    die   "'$det_file':'$det_error'";
  }
}
die "No data in simulation file '$file'"
    unless @$datalist;
    
if ($print_color) {
  overview_plot("$file.ps/cps", $datalist, 0);
        die "Wrote postscript file '$file.ps' (cps)\n";
} elsif ($print_bw) {
  overview_plot("$file.ps/ps", $datalist, 0);
        die "Wrote postscript file '$file.ps' (ps)\n";
} elsif ($print_gif) {
  overview_plot("$file.gif/gif", $datalist, 0);
        die "Wrote GIF file '$file.gif' (gif)\n";
} 

print "Click on a plot for full-window view.\n" if @$datalist > 1;
print "Type 'P' 'C' or 'G' (in graphics window) for hardcopy, 'Q' to quit.\n";

for(;;) {
    my ($cc,$cx,$cy,$idx);
    # Do overview plot, letting user select a plot for full-screen zoom.    
    ($cc,$idx) = overview_plot("/xserv", $datalist, 1);
    last if $cc =~ /[xq]/i;        # Quit?
    if($cc =~ /[pcg]/i) {        # Hardcopy?
        my $ext="ps";
        my $dev = ($cc =~ /c/i) ? "cps" : "ps";
        if($cc =~ /g/i) { $dev = "gif"; $ext="gif"; }
        overview_plot("$file.$ext/$dev", $datalist, 0);
        print "Wrote postscript file '$file.$ext' ($dev)\n";
        next;
    }
    # now do a full-screen version of the plot selected by the user.
    ($cc, $cx, $cy) = single_plot("/xserv", $datalist->[$idx], 1);
    last if $cc =~ /[xq]/i;        # Quit?
    if($cc =~ /[pcg]/i) {        # Hardcopy?
        my $ext="ps";
        my $dev = ($cc =~ /c/i) ? "cps" : "ps";
        if($cc =~ /g/i) { $dev = "gif"; $ext="gif"; }
        my $filename = "$datalist->[$idx]{'Filename'}.$ext";
        single_plot("$filename/$dev", $datalist->[$idx], 0);
        print "Wrote postscript file '$filename'\n";
    }        
}
