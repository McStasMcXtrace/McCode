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

# ADD/MOD: E. Farhi Sep 21th, 2001 : handle -ps and -psc for automatic 
# print and exit
my ($file, $files, $path);
my $pg_devname = "xserv";
my ($index);
$index = 0;
for($i = 0; $i < @ARGV; $i++) {
	$_ = $ARGV[$i];
  # Options specific to mcplot.
	if(/^-psc$/ || /^-c$/ || /^-cps$/) {
	    $pg_devname = "cps";
	} elsif(/^-ps$/ || /^-p$/) {
	    $pg_devname = "ps";
	} elsif(/^-gif$/ || /^-g$/) {
	    $pg_devname = "gif";
  } elsif(/^-html$/) {
	    $pg_devname = "html";
  } elsif(/^--help$/ || /^-h$/ || /^-v$/) {
      print "mcplot [-ps|-psc|-gif|-h] <single simfile | detector_files>\n";
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
if (-d $file) { $path=$file; $file = "$file/mcstas.sim";}
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
my $filename = 'mcstas';    
if (scalar(@$datalist) == 1)
{ $filename = "$datalist->[0]{'Filename'}"; }
if ($path) { $filename = "$path/$filename"; }

if ($pg_devname =~ "html") {
  # write html page showing simulation results
  
  # output instrument/simulation info
  
  # if simulation program exists in current directory or in $path then
  # call mcdisplay to create view
  # create command line for mcdisplay
  my @command = ();
  my $out_name = $instr_inf->{'Instrument-source'};
  if($out_name =~ /(.*)\.instr$/) {
	  $out_name = $1;
  $out_name = "$out_name.out";
  if ($path) {
    if (-f "$path/$out_name") { $out_name = "$path/$out_name"; }
  }
  if (-f $out_name) {
    push @command, "mcdisplay -gif --multi";
	  push @command, "$out_name";
	  for (@{$instr_inf->{'Parameters'}}) {
      my $param = $_;
      my $val = $sim_inf->{'Params'}{$_};
      if($val =~ /^\s*,\s*$/) { $val = $1; }
	      push @command, "$_=$val";
	  }
    exec join(" ",@command);
    if ($path) { exec "mv mcdisplay.gif $path/"); }
  }
  # now create gif of all monitors
  # creates gif of each monitor
  # make link to data and image, and add stats to html
  # image should have as [alt] the detector info
  
}

if ($pg_devname =~ "cps") {
  overview_plot("$filename.ps/cps", $datalist, 0);
	die "Wrote file '$filename.ps' (cps)\n";
} elsif ($pg_devname !~ "xserv") {
  overview_plot("$filename.$pg_devname/$pg_devname", $datalist, 0);
	die "Wrote file '$filename.$pg_devname' ($pg_devname)\n";
}

print "Click on a plot for full-window view.\n" if @$datalist > 1;
print "Type 'P' (in graphics window) for hardcopy, 'Q' to quit.\n";

for(;;) {
    my ($cc,$cx,$cy,$idx);
    if (scalar(@$datalist) != 1) {
      # Do overview plot, letting user select a plot for full-screen zoom.    
      ($cc,$idx) = overview_plot("/xserv", $datalist, 1);
      last if $cc =~ /[xq]/i;	# Quit?
      if($cc =~ /[pc]/i) {	# Hardcopy?
        my $dev = ($cc =~ /c/i) ? "cps" : "ps";
        overview_plot("$filename.ps/$dev", $datalist, 0);
        print "Wrote postscript file 'mcstas.ps' ($dev)\n";
        next;
      }
    } else { $idx=0; }
    # now do a full-screen version of the plot selected by the user.
    ($cc, $cx, $cy) = single_plot("/xserv", $datalist->[$idx], 1);
    last if $cc =~ /[xq]/i;	# Quit?
    if($cc =~ /[pc]/i) {	# Hardcopy?
      my $dev = ($cc =~ /c/i) ? "cps" : "ps";
      my $filename = "$datalist->[$idx]{'Filename'}.ps";
      if ($path) { $filename = "$path/$filename"; }
      single_plot("$filename/$dev", $datalist->[$idx], 0);
      print "Wrote postscript file '$filename'\n";
    }	
}
