#! /usr/bin/perl -w
#
# Main perl script for running scans (subsequent simulations) with McStas
#
#
#   This file is part of the McStas neutron ray-trace simulation package
#   Copyright (C) 1997-2003, All rights reserved
#   Risoe National Laborartory, Roskilde, Denmark
#   Institut Laue Langevin, Grenoble, France
#
#   This program is free software; you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation; either version 2 of the License, or
#   (at your option) any later version.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
#
#   You should have received a copy of the GNU General Public License
#   along with this program; if not, write to the Free Software
#   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

# Config module needed for various platform checks.
# PW, 20030702
use Config;
use IPC::Open3;
use Net::Ping;

# Determine the path to the McStas system directory. This must be done
# in the BEGIN block so that it can be used in a "use lib" statement
# afterwards.
BEGIN {
    if($ENV{"MCSTAS"}) {
        $MCSTAS::sys_dir = $ENV{"MCSTAS"};
    } else {
      if ($Config{'osname'} eq 'MSWin32') {
        $MCSTAS::sys_dir = "c:\\mcstas\\lib";
      } else {
        $MCSTAS::sys_dir = "/usr/local/lib/mcstas";
      }
    }
    $MCSTAS::perl_dir = "$MCSTAS::sys_dir/tools/perl"
    $MCSTAS::perl_modules = "$MCSTAS::perl_dir/modules";
}
use lib $MCSTAS::perl_dir;

use FileHandle;
use strict;

require "mcrunlib.pl";
require "mcstas_config.perl";
if (!($Config{'osname'} eq "MSWin32")) {
    if (-e $MCSTAS::perl_modules) {
	# Load extra unix-only dependencies
	require "mcstas_unix.pl";
    }
}

# Turn off buffering on stdout. This improves the use with the Unix
# "tee" program to create log files of scans.
autoflush STDOUT 1;

# Various parameters determined by the command line.
my ($sim_def, $force_compile, $data_dir, $data_file);
my $ncount = 1e6;               # Number of neutron histories in one simulation
my $numpoints = 1;              # Number of points in scan (if any)
my @params = ();                # List of input parameters
my %vals;                       # Hash of input parameter values
my @options = ();               # Additional command line options (mcstas)
my @ccopts = ();                # Additional command line options (cc)
my ($plotter);                  # sim/data file format
my $format_ext;                 # sim file extension
my $format_assign;              # assignation symbol ':', '='
my $format_start_value;         # symbol before field value
my $format_end_value;           # symbol after field value
my $format_prefix;              # symbol for line prefix
my $format_limprefix;           # symbol for 'x' / 'xy' limits definition
my $exec_test=0;                # flag for McStas package test execution
my $slave = 0;                  # 'slave' hostname for running remotely
my $slavedir="";                # temporary dir on remote machine
my $start=-1;                   # first scan number to run in slave mode
my $end=-1;                     # last scan number to run in slave mode
my $multi=0;                    # multi machine mode
my @hostlist = ();              # list of remote machines to run on...

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
    if($text =~ /^([a-z0-9_]+)\=(.*)$/) {
        set_inputpar($1, $2);
    } else {
        die "mcrun: Invalid input parameter specification '$text'";
    }
}

# Read input parameters from parameter file.
sub read_inputpar_from_file {
    my ($filename) = @_;
    open(IN, "<$filename") || die "Failed to open file '$filename'";
    while(<IN>) {
        my $p;
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
        if(/^--force-compile$/ || /^-c$/ || /^--force$/) {
            $force_compile = 1;
        } elsif(/^--param\=(.*)$/ || /^-p(.+)$/) {
            read_inputpar_from_file($1);
        } elsif(/^--param$/ || /^-p$/) {
            read_inputpar_from_file($ARGV[++$i]);
        } elsif(/^--numpoints\=(.*)$/ || /^-N(.+)$/) {
            $numpoints = $1;
        } elsif(/^--numpoints$/ || /^-N$/) {
            $numpoints = $ARGV[++$i];
        } elsif(/^--ncount\=(.*)$/ || /^-n(.+)$/) {
            $ncount = $1;
            push @options, "--ncount=$ncount";
        } elsif(/^--ncount$/ || /^-n$/) {
            $ncount = $ARGV[++$i];
            push @options, "--ncount=$ncount";
        } elsif (/^--start\=(.*)$/) {
	    $start=$1;
        } elsif (/^--end\=(.*)$/) {
	    $end=$1;
        } elsif (/^--slave\=(.*)$/) {
	    $slave=$1;
	} elsif (/^--slavedir\=(.*)$/) {
	    $slavedir="$1/";
	} elsif (/^--multi/ || /^-M/) {
	    if ($Config{'osname'} eq 'MSWin32') {
		print STDOUT "Sorry, --multi is not supported on windows!\n";
	    } else {
		$multi=1;
	    }
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
        elsif(/^--(seed)\=(.*)$/) {
            push @options, "--$1=$2";
        } elsif(/^-([s])(.+)$/) {
            push @options, "-$1$2";
        } elsif(/^--(seed)$/) {
            push @options, "--$1=$ARGV[++$i]";
        } elsif(/^-([s])$/) {
            push @options, "-$1$ARGV[++$i]";
        } elsif(/^--(format)$/ || /^--(plotter)$/) {
            $plotter = $ARGV[$i];
        } elsif(/^--(format)\=(.*)$/ || /^--(plotter)\=(.*)$/) {
            $plotter = $2;
        } elsif(/^--test$/) {
            $exec_test="compatible";
        } elsif(/^--(test)\=(.*)$/) {
            $exec_test="$1";
        } elsif(/^--(data-only|help|info|trace|no-output-files|gravitation)$/) {
            push @options, "--$1";
        } elsif(/^-([ahitg])$/) {
            push @options, "-$1";
        }
        # Non-option arguments.
        elsif(/^-/) {                # Unrecognised option
            push @ccopts, "$_";
        } elsif(/^([a-zA-Z0-9_]+)\=(.*)$/) {
            set_inputpar($1, $2);
        } else {                        # Name of simulation definition
            if($sim_def) {
                die "mcrun: Only a single instrument definition may be given ($sim_def, $_)";
            } else {
                $sim_def = $_;
            }
        }
    }
    if ($data_dir && $slavedir) {
	$data_dir="$slavedir$data_dir";
    }
    my $cc = defined($ENV{'MCSTAS_CC'}) ?
        $ENV{'MCSTAS_CC'} : $MCSTAS::mcstas_config{CC};
    my $cflags = defined($ENV{'MCSTAS_CFLAGS'}) ?
        $ENV{'MCSTAS_CFLAGS'} : $MCSTAS::mcstas_config{CFLAGS};
    my $mcstas_format = $plotter;
    if ($mcstas_format =~ /0/) { $mcstas_format = "McStas PGPLOT"; }
    elsif ($mcstas_format =~ /1/ || $mcstas_format =~ /2/) { $mcstas_format = "Matlab"; }
    elsif ($mcstas_format =~ /3/ || $mcstas_format =~ /4/) { $mcstas_format = "Scilab"; }
      
    if ($cflags eq "") { $cflags = "default, e.g. -O"; }
    if ($cc eq "") { $cc = "default, e.g. gcc"; }
    if ($mcstas_format eq "") { $mcstas_format = "default, e.g. Scilab"; }
    
    # Check if this is a multi-machine run
    if ($multi == 1) {
	# Assume that this is unix...
	# Check for ssh 
	my $ssh_path;
	my $pid = open(READ, "which ssh|");
	while (<READ>) {
	    $ssh_path=$_;
	    chomp $ssh_path;
	}
	if (!-e $ssh_path) {
	    print STDERR "You have no ssh, --multi disabled...\n";
	    $multi = 0;
	}
	# Check that something is available in the .mcstas-hosts
	my $HOME = $ENV{'HOME'};
	my $hostfile = "$HOME/.mcstas-hosts";
	if (! -e $hostfile) {
	    print STDERR "You have no .mcstas-hosts in your homedir!--multi disabled...\n\n";
	    $multi = 0;
	}
	else {
	    print STDERR "Pinging mcstas-hosts 1 per sec. since you requested --multi...\n";
	    # Read the host file...
	    $pid = open(HOSTFILE,$hostfile);
	    my $host;
	    while ($host = <HOSTFILE>) {
		chomp $host;
		# Remove spaces if any...
		$host =~ s! !!g;
		if (! $host eq '') {
		    my $p = Net::Ping->new();
		    my $response = 0;
		    $response= $p->ping($host, 1);
		    if ($response == 1) {
			push @hostlist, $host;
		    } else {
			print STDERR "Not spawning to host $host: not responding\n";
		    }
		}
	    }
	}
    }
    
    
    die "Usage: mcrun [-cpnN] Instr [-sndftgahi] params={val|min,max}
  mcrun options:
   -c        --force-compile  Force rebuilding of instrument.
   -p=FILE   --param=FILE     Read parameters from file FILE.
   -n COUNT  --ncount=COUNT   Set number of neutrons to simulate.
   -N NP     --numpoints=NP   Set number of scan points.
  Instr options:
   -s SEED   --seed=SEED      Set random seed (must be != 0)
   -n COUNT  --ncount=COUNT   Set number of neutrons to simulate.
   -d DIR    --dir=DIR        Put all data files in directory DIR.
   -f FILE   --file=FILE      Put all data in a single file.
   -t        --trace          Enable trace of neutron through instrument.
   -g        --gravitation    Enable gravitation for all trajectories.
   -a        --data-only      Do not put any headers in the data files.
   --no-output-files          Do not write any data files.
   -h        --help           Show help message.
   -i        --info           Detailed instrument information.
   --test={compatible|full}   Execute McStas selftest and generate report
   --format=FORMAT            Output data files using format FORMAT.
                              (format list obtained from <instr>.out -h)
   -M        --multi          Spawn simulations to multiple machine grid.
                              See the documentation for more info. 
                              --multi Not supported on Win32.

This program both runs mcstas with Instr and the C compiler to build an
independent simulation program. The following environment variables may be 
specified for building the instrument:
  MCSTAS        Location of the McStas and component library 
                  ($MCSTAS::sys_dir).
  MCSTAS_CC     Name of the C compiler               ($cc)
  MCSTAS_CFLAGS Options for compilation              ($cflags)
  MCSTAS_FORMAT Default FORMAT to use for data files ($mcstas_format)
SEE ALSO: mcstas, mcdoc, mcplot, mcrun, mcgui, mcresplot, mcstas2vitess
DOC:      Please visit http://neutron.risoe.dk/mcstas/
** No instrument definition name given\n" unless $sim_def || $exec_test;
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
        if($vals{$v} =~ /^(.+),(.+)$/) {
            # Variable to scan from min to max.
            $minval[$j] = $1;
            $maxval[$j] = $2;
            $scanned[$j] = $i;
            if($minval[$j] != $maxval[$j] && $numpoints == 1) {
                die "mcrun: Cannot scan variable $v using only one data point.
Please use -N to specify the number of points.";
            }
            $j++;
        } elsif($vals{$v} =~ /^(.+)$/) {
            # Constant variable (no action).
        } else {
            die "mcrun: Invalid parameter specification '$vals{$v}' for parameter $v";
        }
        $i++;
    }
    return { VARS => \@scanned, MIN => \@minval, MAX => \@maxval };
}

sub exec_sim {
    my (@options) = @_;
    my @cmdlist = ($out_file, @options, map("$_=$vals{$_}", @params));
    my $cmd;
    # ToDo: This is broken in that it does not show the proper quoting
    # that would be necessary if the user actually wanted to run the
    # command manually. (Note that the exec() call is correct since it
    # does not need any quoting).
    print join(" ", @cmdlist), "\n";
    if (!$slave == 0) {
	if ($slave eq "localhost") {
	    print STDERR "This is a local slave run\n";
	    $cmd = "echo cd $slavedir && ./@cmdlist";
	} else {
	    print STDERR "This is a remote slave run at $slave\n";
	    $cmd = "ssh $slave \"cd $slavedir && ./@cmdlist\"";
	}
	exec $cmd;
    } else {
	exec @cmdlist;
    }
}

# Handle the case of a single run of the simulation (no -N option).
sub do_single {
    push @options, "--dir=$data_dir" if $data_dir;
    push @options, "--file=$data_file" if $data_file;

    print "Running simulation '$out_file' ...\n";
    exec_sim(@options) || die "mcrun: Failed to run simulation";
}


# Handle output of header information for .dat files and .sim information files.
sub do_instr_header {
    my ($prefix, $OUT) = @_;
    print $OUT join("", map("$prefix$_", @{$instr_info->{'RAW'}}));
}

# Write Matlab/Scilab function header to datafile
sub do_instr_init {
    my ($OUT) = @_;
    if ($plotter =~ /Matlab/i) {
        print $OUT <<ENDCODE
function mcstas = get_mcstas(p)
% Embedded function for building 'mcplot' compatible structure
% PW, RISOE, 20030701
%
% import data using s=mcstas('plot');
if nargout == 0 | nargin > 0, p=1; else p=0; end
ENDCODE
    } else {
        print $OUT <<ENDCODE
function mcstas = get_mcstas(p)
// Embedded function for building 'mcplot' compatible structure
// PW, RISOE, 20030701
//
// import data using exec('mcstas.sci',-1); s=get_mcstas('plot');
mode(-1); //silent execution
if argn(2) > 0, p=1; else p=0; end
ENDCODE
   }
    
    
}

# Write Matlab/Scilab embedded plotting functions to datafile
sub do_sim_tail{
    my ($OUT) = @_;
    if ($plotter =~ /Matlab/i) {
        print $OUT <<ENDCODE

function d=mcplot_inline(d,p)
% local inline function to plot data
if ~p, return; end;
eval(['l=[',d.xylimits,'];']); S=size(d.data);
t1=['[',d.parent,'] ',d.filename,': ',d.title];t = strvcat(t1,['  ',d.variables,'=[',d.values,']'],['  ',d.signal],['  ',d.statistics]);
disp(t);
if ~isempty(findstr(d.type,'1d')), return; end
figure; 
if ~isempty(findstr(d.type,'1d'))
    d.x=linspace(l(1),l(2),S(1)); 
    h=errorbar(d.x,d.data,d.errors);
end
xlabel(d.xlabel); ylabel(d.ylabel); title(t); axis tight;set(gca,'position',[.18,.18,.7,.65]); set(gcf,'name',t1);grid on;
if ~isempty(findstr(d.type,'2d')), colorbar; end

% end of datafile...
ENDCODE
    } else {
        print $OUT <<ENDCODE

endfunction

function d=mcplot_inline(d,p)
// local inline func to plot data
if ~p, return; end;
execstr(['l=[',d.xylimits,'];']); S=size(d.data);
t1=['['+d.parent+'] '+d.filename+': '+d.title];t = [t1;['  '+d.variables+'=['+d.values+']'];['  '+d.signal];['  '+d.statistics]];
mprintf('%s\\n',t(:));
if ~length(strindex(d.type,'1d')),return;
else
  w=winsid();if length(w),w=w(\$)+1; else w=0; end
  xbasr(w); xset('window',w);
  if length(strindex(d.type,'1d'))
    d.x=linspace(l(1),l(2),S(1));
    mcplot_errorbar(d.x,d.data,d.errors);
    if p == 2, t = t1; end
    xtitle(t,d.xlabel,d.ylabel)
  end
end
xname(t1);
endfunction // mcplot_inline

function mcplot_errorbar(x,y,e)
// function for creating simple errorbar plots...
  // first, estimate plot range
  xmin=min(x);
  xmax=max(x);
  ymin=min(y-e);
  ymax=max(y+e);
  plot2d(x,y,rect=[xmin ymin xmax ymax]);
  errbar(x,y,e,e);
endfunction // mcplot_errorbar

ENDCODE
    } 
}


sub do_sim_header {
    my ($prefix, $OUT) = @_;
    my $date = localtime(time());
    my $format_member=".";
    my $param_field  ="Param";
    if ($plotter =~ /McStas|PGPLOT/i) { $format_member=": "; }
    print $OUT "${prefix}Date$format_assign $format_start_value$date$format_end_value\n";
    print $OUT "${prefix}Ncount$format_assign $format_start_value$ncount$format_end_value\n";
    print $OUT "${prefix}Numpoints$format_assign $format_start_value$numpoints$format_end_value\n";
    if ($plotter =~ /Scilab|3|4|Matlab|1|2/i) { $param_field = "parameters"; }
    # Scilab needs initalisation of Param here...
    if ($plotter =~ /Scilab|3|4/i) { 
      print $OUT "${prefix}$param_field=0; ${prefix}$param_field=struct(); \n";
    }
    if ($plotter =~ /Scilab|3|4|Matlab|1|2/i) { 
      print $OUT "${prefix}${param_field}${format_member}class = 'parameters';\n";
      print $OUT "${prefix}${param_field}${format_member}name  = 'parameters';\n";
      print $OUT "${prefix}${param_field}${format_member}parent= '$sim_def';\n";
    }
    my $paramtext = join("\n", map("${prefix}$param_field$format_member$_ = $format_start_value$vals{$_}$format_end_value", @params));
    print $OUT $paramtext, "\n";
}

sub do_data_header {
    my ($pr, $OUT, $info, $youts, $variables, $datfile, $datablock) = @_;
    my $scannedvars;
    my $xvars;
    my $xvar_list;
    my $yvars = join " ", @$youts;
    my $xlabel;
    my $min;
    my $max;
    if (@{$info->{VARS}} == 0) { $xlabel = "Point number"; $scannedvars="Point"; $xvars="Point"; $min=1; $max=$numpoints; }
    else { 
      $xlabel = $params[$info->{VARS}[0]]; 
      $scannedvars = join ", ", map($params[$_], @{$info->{VARS}}); 
      $xvars = join " ", map($params[$_], @{$info->{VARS}}); 
      push @$xvar_list, map($params[$_], @{$info->{VARS}});
      $min = $info->{MIN}[0]; $max = $info->{MAX}[0];
    }
    # Here, we need special handling of the Matlab/Scilab output cases, since 
    # each monitor dataset must be defined in its own class 'data' structure for mcplot
    # But - only to be done if this is not a 'slave' run
    if ($plotter =~ /McStas|PGPLOT|0/i) {
      if (($slave eq 0) || ($slave eq 'localhost')) {
        print $OUT <<END
${pr}type$format_assign ${format_start_value}multiarray_1d($numpoints)$format_end_value
${pr}title$format_assign ${format_start_value}Scan of $scannedvars$format_end_value
${pr}xvars$format_assign ${format_start_value}$xvars$format_end_value
${pr}yvars$format_assign ${format_start_value}$yvars$format_end_value
${pr}xlabel$format_assign '$xlabel'
${pr}ylabel$format_assign 'Intensity'
${pr}${format_limprefix}limits$format_assign ${format_start_value}$min $max$format_end_value
${pr}filename$format_assign ${format_start_value}$datfile$format_end_value
${pr}variables$format_assign ${format_start_value}$variables$format_end_value
END
      }
    } elsif ($plotter =~ /Matlab|Scilab|1|2|3|4/i) {
	if (($slave eq 0) || ($slave eq 'localhost')) {
          if (! $datablock eq "") {
	      print $OUT "DataBlock=[$datablock];\n";
	      if ($slave eq 'localhost') {
		print $OUT "$format_prefix\nInsert blocks here!\n";
	      }
              # Now loop across all the youts...
              my $idx = scalar(@$xvar_list) + 1;
              foreach my $y_pair (@$youts) {
                  # Cut out the relevant descriptor
                  my $start_char = index($y_pair, "(") + 1;
                  my $end_char = index($y_pair, ",") - $start_char;
                  my $y_label = substr($y_pair, $start_char, $end_char);
                  if ($plotter =~ /Scilab/i) {
                      print $OUT "$pr$y_label=0; $pr$y_label=struct();\n";
                  }
                  print $OUT "$pr$y_label.class='data';\n";
                  print $OUT "$pr$y_label.data  =[DataBlock(:,$idx)];\n";
      print $OUT "$pr$y_label.errors=[DataBlock(:,$idx+1)];\n";
                  $idx = $idx+2;
                  print $OUT "$pr$y_label.parent='$y_label';\n";
      print $OUT "$pr$y_label.Source='$sim_def';\n";
      print $OUT "$pr$y_label.ratio ='$ncount';\n";
                  print $OUT "$pr$y_label.values='';\n";
                  print $OUT "$pr$y_label.signal='';\n";
                  print $OUT "$pr$y_label.statistics='';\n";
                  my $pr_label = "$pr$y_label.";
                  # Fill the struct
                  print $OUT <<ENDCODE
${pr_label}type$format_assign ${format_start_value}array_1d($numpoints)$format_end_value
${pr_label}title$format_assign ${format_start_value}Scan of $scannedvars$format_end_value
${pr_label}xvars$format_assign ${format_start_value}$xvars$format_end_value
${pr_label}yvars$format_assign ${format_start_value}$y_label$format_end_value
${pr_label}xlabel$format_assign '$xlabel';
${pr_label}ylabel$format_assign 'Intensity';
${pr_label}${format_limprefix}limits$format_assign ${format_start_value}$min $max$format_end_value
${pr_label}filename$format_assign ${format_start_value}$y_label$format_end_value
${pr_label}variables$format_assign ${format_start_value}$y_pair$format_end_value
${pr}$y_label=mcplot_inline(${pr}$y_label,p);

ENDCODE

            }
        print $OUT "clear Datablock;\n";
        }
      } else { # slave run, simply dump the datablock...
	  print $OUT "DataBlock=[DataBlock;$datablock];\n";      
      }
    }
}

# Write <NAME>.sim information file, to be read by mcplot.
sub output_sim_file {
    my ($filename, $info, $youts, $variables, $datfile, $datablock) = @_;
    my $SIM = new FileHandle;
    my $loc_prefix = "";
    my $instr_name = $sim_def;
    my $i = $info->{VARS}[0]; # Index of variable to be scanned
    my $minvar = $info->{MIN}[0] ? $info->{MIN}[0] : 0;
    my $maxvar = $info->{MAX}[0] ? $info->{MAX}[0] : 0;
    my $namvar = defined($i) && $params[$i] ? $params[$i] : "nothing";
    
    $instr_name =~ s/\.instr$//;        # Strip any trailing ".instr" extension ...
    open($SIM, ">$filename") ||
        die "mcrun: Failed to write info file '$filename'";
    if (($slave eq 0) || ($slave eq 'localhost')) {
      if ($plotter =~ /McStas|PGPLOT/i) {
        do_instr_header("", $SIM);
        print $SIM "\nbegin simulation\n";
        $loc_prefix = "";
      } elsif ($plotter =~ /Matlab/i) {   
        do_instr_init($SIM);
        do_instr_header("% ", $SIM);
        print $SIM "\nsim.class='simulation';\n";
        print $SIM "sim.name='$filename';\n";
        $loc_prefix = "sim.";
      } else {
        do_instr_init($SIM);
        do_instr_header("// ", $SIM);
        print $SIM "\nsim = struct(); sim.class='simulation';\n";
        print $SIM "sim.name='$filename';\n";
        $loc_prefix = "sim.";
      }
      do_sim_header($loc_prefix, $SIM);
      if ($plotter =~ /McStas|PGPLOT/i) {
        print $SIM "end simulation\n\nbegin data\n";
      } elsif ($plotter =~ /Matlab/i) {
        print $SIM "\nmcstas.sim = sim; clear sim;\n";
        print $SIM "mcstas.File= '$filename';\n";
        print $SIM "mcstas.instrument.Source= '$sim_def';\n";
        print $SIM "mcstas.instrument.parent= 'mcstas';\n";
        print $SIM "mcstas.instrument.class = 'instrument';\n";
        print $SIM "mcstas.instrument.name  = '$instr_name';\n";
        print $SIM "mcstas.instrument.Parameters  = '...';\n";
        print $SIM "\ndata.class = 'superdata';\n";
        print $SIM "data.name = 'Scan of $namvar';\n";
        print $SIM "data.scannedvar = '$namvar';\n";
        print $SIM "data.numpoints  = $numpoints;\n";
        print $SIM "data.minvar  = $minvar;\n";
        print $SIM "data.maxvar  = $maxvar;\n";
        $loc_prefix = "data.";
      } else {
        print $SIM "\nmcstas = struct(); mcstas.sim = 0; mcstas.sim = sim; clear sim;\n";
        print $SIM "mcstas.File= '$filename';\n";
        print $SIM "instrument = struct();\n";
        print $SIM "instrument.Source= '$sim_def';\n";
        print $SIM "instrument.parent= 'mcstas';\n";
        print $SIM "instrument.class = 'instrument';\n";
        print $SIM "instrument.name  = '$instr_name';\n";
        print $SIM "instrument.Parameters  = '...';\n";
        print $SIM "mcstas.instrument = 0; mcstas.instrument = instrument; clear instrument;\n";
        print $SIM "\ndata = struct(); data.class = 'superdata';\n";
        print $SIM "data.name = 'Scan of $namvar';\n";
        print $SIM "data.scannedvar = '$namvar';\n";
        print $SIM "data.numpoints  = $numpoints;\n";
        print $SIM "data.minvar  = $minvar;\n";
        print $SIM "data.maxvar  = $maxvar;\n";
        $loc_prefix = "data.";
      }
    }
    do_data_header($loc_prefix, $SIM, $info, $youts, $variables, $datfile, $datablock);
    if (($slave eq 0) || ($slave eq 'localhost')) {
      if ($plotter =~ /McStas|PGPLOT/i) {
        print $SIM "end data\n";
      } elsif ($plotter =~ /Matlab/i) {
        print $SIM "\nmcstas.sim.data = data; clear data;\n";
        do_sim_tail($SIM);
      } else {
        print $SIM "\nmcstas.sim.data = 0; mcstas.sim.data = data; clear data;\n";
        do_sim_tail($SIM);
      }
    }
    close($SIM);
}

# Output header information for mcrun .dat scan file.
sub output_dat_header {
    my ($OUT, $format_prefix, $info, $youts, $variables, $datfile) = @_;
    print $OUT "${format_prefix}Instrument-source: '$instr_info->{'Instrument-source'}'\n";
    do_sim_header($format_prefix, $OUT);
    do_data_header($format_prefix, $OUT, $info, $youts, $variables, $datfile, "");
}

# Do a scan: Run a series of simulations, varying one or more input
# parameters.
sub do_scan {
    my ($info) = @_;
    # Create the output directory if requested.
    my $prefix = "";
    if($data_dir) {
        if ($slave eq 0) {
	    if(mkdir($data_dir, 0777)) {
		$prefix = "$data_dir/";
	    } else {
		die "Error: unable to create directory '$data_dir'.\n(Maybe the directory already exists?)";
		print $MCSTAS::mcstas_config{'PLOTTER'}; # unreachable code, avoids warning for mcstas_config
	    }
        } else {
	    $prefix = "$data_dir/";
	    if (!($slave eq "localhost")) {
		system("ssh $slave mkdir $prefix") || print STDOUT "dir there already...";
	    }
	    system("mkdir $prefix") || print STDOUT "dir there already..." ;
	}
    }
    # Use user-specified output file name, with a default of "mcstas.dat".
    my $datfile = ($data_file || "mcstas.dat");
    # Add a default '.dat' extension if no other extension given.
    $datfile .= ".dat" unless $datfile =~ m'\.[^/]*$'; # Quote hack ';
    my $simfile = $datfile;
    $simfile =~ s/\.dat$//;        # Strip any trailing ".dat" extension ...
    $simfile .= $format_ext;        # ... and add ".sim|m|sci" extension.
    my $DAT;
    # Only initialize / use $DAT datafile if format is PGPLOT
    if ($plotter =~ /PGPLOT|McStas|0/i) {
      $DAT = new FileHandle;
      open($DAT, ">${prefix}$datfile");
      autoflush $DAT 1;                # Preserves data even if interrupted.
    } else {
      $datfile = $simfile;             # Any reference should be to the simfile
    }
    my $firsttime = 1;
    my $variables = "";
    my @youts = ();
    my $point;
    my @lab_datablock = ();          # Storing of scan data in variable
                                     # for saving datablock in matlab/scilab
    my $datablock = "";              # 'sim' file.
    # Initialize the @lab_datablock according to 'format'
    if ($start == -1) {
	$start = 0;
    }
    if ($end == -1) {
	$end = $numpoints;
    }
    for($point = 0; $point < $numpoints; $point++) {
	if (($point >= $start) && ($point <= $end)) {
	    my $out = "";
	    my $j;
	    for($j = 0; $j < @{$info->{VARS}}; $j++) {
		my $i = $info->{VARS}[$j]; # Index of variable to be scanned
		$vals{$params[$i]} =
		    ($info->{MAX}[$j] - $info->{MIN}[$j])/($numpoints - 1)*$point +
                    $info->{MIN}[$j];
		$out .= "$vals{$params[$i]} ";
		$variables .= "$params[$i] " if $firsttime
		}
	    if (@{$info->{VARS}} == 0) { $out .= "$point "; $variables .= "Point " if $firsttime; }
	    # Decide how to handle output files.
	    my $output_opt =
		$data_dir ? "--dir=$data_dir/$point" : "--no-output-files";
	    my $got_error = 0;
	    my $pid;
	    if ($Config{'osname'} eq 'MSWin32') {
                # Win32 needs all possible parameters here, since we can not open(SIM,"-|");
                my @cmdlist = ($out_file, @options, map("$_=$vals{$_}", @params), $output_opt, "--format=$plotter");
                $pid = open(SIM, "@cmdlist |");
	    } else {
                $pid = open(SIM, "-|");
	    }
	    die "Failed to spawn simulation command" unless defined($pid);
	    if($pid) {                # Parent
		while(<SIM>) {
		    chomp;
		    if(/Detector: ([^ =]+_I) *= *([^ =]+) ([^ =]+_ERR) *= *([^ =]+) ([^ =]+_N) *= *([^ =]+) *(?:"[^"]+" *)?$/) { # Quote hack -> ") {
			my $sim_I = $2;
			my $sim_err = $4;
			my $sim_N = $6;
			$out .= " $sim_I $sim_err";
			if($firsttime) {
			    $variables .= " $1 $3";
			    push @youts, "($1,$3)";
			}
		    } elsif(m'^Error:') {
			$got_error = 1;
		    }
		    print "$_\n";
		}
	    } else {                # Child
		open(STDERR, ">&STDOUT") || die "Can't dup stdout";
		exec_sim(@options, $output_opt);
	    }
	    
        my $ret = close(SIM);
        die "mcrun: Exit due to error returned by simulation program" if $got_error || (! $ret && ($? != 0 || $!));
              if ($firsttime eq 1) {
                if ($plotter =~ /PGPLOT|McStas|0/i) {
		    if (($slave eq 0) || ($slave eq 'localhost')) {
			output_dat_header($DAT, "# ", $info, \@youts, $variables, $datfile);
		    }
                }
              } else {
                push @lab_datablock, "\n";
              }
        if ($plotter =~ /PGPLOT|McStas|0/i) {
                print $DAT "$out\n";
              }
        push @lab_datablock, "$out";
        $firsttime = 0;
	}
    }
    if ($plotter =~ /PGPLOT|McStas|0/i) {
      close($DAT);
    }
    $datablock = join(" ", @lab_datablock);
    output_sim_file("${prefix}$simfile", $info, \@youts, $variables, $datfile, $datablock);
    
    print "Output file: '${prefix}$datfile'\nOutput parameters: $variables\n";
}

# Do a multi scan on several machines...
sub do_scan_multi {
    my ($info) = @_;
    # Create the output directory if requested.
    my $prefix = "";
    if($data_dir) {
        if(mkdir($data_dir, 0777)) {
            $prefix = "$data_dir/";
        } else {
	    if ($slave eq 0) {
		die "Error: unable to create directory '$data_dir'.\n(Maybe the directory already exists?)";
		print $MCSTAS::mcstas_config{'PLOTTER'}; # unreachable code, avoids warning for mcstas_config
	    }
        }
    }
    my ($tmpdir,$j,$k,$output);
    open(READ,"mktemp |");
    while (<READ>) {
	$tmpdir = $_;
	chomp $tmpdir;
    }
    # On some systems, we will have to remove the $tmpdir, since it is a file...
    if (-e $tmpdir) {
	my_system("rm -f $tmpdir","Problem removing $tmpdir");
    }
    # Add local machine as a slave...
    @hostlist = ("localhost", @hostlist);
    print STDOUT "hosts are: @hostlist\n";
    my $hosts = @hostlist;
    # Determine how many scan points to send to each slave
    my $perslave = int($numpoints/$hosts);
    my $extra = $hosts*($numpoints/$hosts - $perslave);
    my @scanmin = ();
    my @scanmax = ();
    my @pids = ();
    my $taken = 0;
    my $Max= 0 ;
    for ($j=0; $j<@hostlist; $j++) {
	if ($j < $numpoints) {
	    $scanmin[$j]=$taken;
	    if ($j<$extra) {
		$scanmax[$j]=$taken+$perslave;
	    } else {
		$scanmax[$j]=$taken+$perslave-1; 
	    }
	    $taken=$scanmax[$j]+1;
	    $pids[$j]=Proc::Simple->new();
	} else {
	    if ($Max == 0) { 
		print STDOUT "Removing host $hostlist[$j] and beyond, no more scan points\n";
		$Max = $j;
	    }
	}
    }
    if (!($Max == 0)) {
	my @tmp = @hostlist;
	@hostlist = ();
	for ($j=0; $j<$Max; $j++) {
	    $hostlist[$j] = $tmp[$j];
	}
    }
    $hosts = @hostlist;
    my $stop = 0;
    my $output_opt = $data_dir ? "--dir=$data_dir" : "--no-output-files";
    my @extras = (@options, map("$_=$vals{$_}", @params), $output_opt, "--format=$plotter");
    $out_file = get_out_file($sim_def, $force_compile, @ccopts); 
    # Create local tmpdir...
    my_system("mkdir $tmpdir/","dir there already?");
    print STDOUT "Spawning child mcrun's...\n";
    for ($j=0; $j<@hostlist; $j++) {
	# Create local folders too...
	my_system("mkdir $tmpdir/$hostlist[$j]","dir there already...");
	if ($j == 0) { # localhost...
	    my_system("cp $out_file $tmpdir/$hostlist[$j]","");
	    my_system("cp $sim_def $tmpdir/$hostlist[$j]",""); 
	} else {
	    my_system("ssh $hostlist[$j] mkdir $tmpdir && ssh $hostlist[$j] mkdir $tmpdir/$hostlist[$j]","");
	    my_system("scp $out_file $hostlist[$j]:$tmpdir/$hostlist[$j] ","");
	    my_system("scp $sim_def $hostlist[$j]:$tmpdir/$hostlist[$j] ","");
	}
	$pids[$j]->start("mcrun -f $hostlist[$j].dat -N$numpoints --slave=$hostlist[$j] --slavedir=$tmpdir/$hostlist[$j]  $out_file --start=$scanmin[$j] --end=$scanmax[$j] @extras > $tmpdir/log.$hostlist[$j]");
    }
    print STDOUT "Waiting for child processes to end...\n";
    my $running;
    while ($stop < $hosts) {
	for ($j=0; $j<$hosts; $j++) {
	    if (!$pids[$j] == 0) {
		$running = $pids[$j]->poll(); 
		if ($running == 0) {
		    print STDERR "Process at $hostlist[$j] terminated, copying back relevant data...\n";
		    if ($hostlist[$j] eq "localhost") {
			if ($data_dir) {
			    my_system("cp -rp $tmpdir/localhost $prefix/ ","");
			}
		    } else {
			if ($data_dir) {
			    my_system("scp -rp $hostlist[$j]:$tmpdir/$hostlist[$j]/ $prefix/ ","");
			}
		    }
		    if ($data_dir) {
			my $PW = getcwd();
			my_system("find $PW/$prefix$hostlist[$j]/$prefix -type d -not -name $prefix -exec cp -rp \\{\\} $prefix \\;","Problem copying slave dirs..");
			my_system("cp $tmpdir/$hostlist[$j]/$prefix/$hostlist[$j].* $prefix/","Problem copying slave files");
		    }
		    $stop++;
		    $pids[$j]=0;
		}
	    }
	}
    }
    open(MCSTAS,">${prefix}mcstas${format_ext}") || die ("could not open file ${prefix}mcstas${format_ext}");
    open(LOCAL,"<${prefix}localhost${format_ext}") || die ("could not open file ${prefix}mcstas${format_ext}");

    # Now, create a mcstas.sim/sci/m to pick up the pieces...
    if ($plotter =~ /McStas|PGPLOT/i) {
	# Use the localhost.sim as sim file (has all needed info - but must have 
        # filename: localhost.dat replaced by mcstas.dat
	while(<LOCAL>) {
	    s/\bfilename: localhost.dat\b/filename: mcstas.dat/;
	    print MCSTAS;
	}
	# Dat's are simply to be catted together. 
	my_system("touch ${prefix}mcstas.dat","problem creating mcstas.dat");
	for ($j=0; $j<$hosts; $j++) {
	    my_system("cat $prefix$hostlist[$j].dat >> ${prefix}mcstas.dat","Problem adding host data");
	    my_system("rm $prefix$hostlist[$j].dat","Problem removing host data");
	}
    } else {
	# With Matlab / Scilab, a special "Insert blocks here!" line marks where to
	# enlarge the DataBlock section with Matlab/Scilab syntax and data from the
	# host files...
	while(<LOCAL>) { # Self closing
	    if (/^\w*Insert blocks here!/) {
		print STDERR "Inserting files...\n";
		for ($j=1; $j<@hostlist; $j++) { # The 1 important here, localhost data there already from localhost$format_ext
		    print MCSTAS "\n\n$format_prefix Insertion from ${prefix}$hostlist[$j]${format_ext}\n\n";
		    open(HOST,"<${prefix}$hostlist[$j]${format_ext}") || print STDERR "could not open file ${prefix}$hostlist[$j]${format_ext}";
		    while(<HOST>) { # Self closing
			print MCSTAS;
		    }
		}
	    } else { #everything else printed directly to mcstas.${format_ext}
		print MCSTAS;
	    }
	}
    }
    close(MCSTAS);
    
    # Create global logfile
    my_system("touch ${prefix}mcstas_multi.log","Error creating multi logfile\n");
    print STDOUT "Done creating the files...\n";
    # Clean up time, remve everything with relation to the hostnames...
    # Also collect the logfiles
    for ($j=0; $j<@hostlist; $j++) {
	open(READ,"rm -rf ${prefix}$hostlist[$j]* |");
	while (<READ>) {
	    $output = $_;
	    print STDERR "$output\n";
	}
	my_system("echo \"###########################\" >> ${prefix}mcstas_multi.log");
	my_system("echo logfile from $hostlist[$j]: >> ${prefix}mcstas_multi.log");
	my_system("cat $tmpdir/log.$hostlist[$j] >> ${prefix}mcstas_multi.log");
	my_system("rm $tmpdir/log.$hostlist[$j]");
	if ($j>0) { # Other than localhost
	    open(READ,"ssh $hostlist[$j] rm -rf $tmpdir |");
	    while (<READ>) {
		$output = $_;
		print STDERR "$output\n";
	    }
	}
    }
    my_system("rm -rf $tmpdir","problem removing temporary dir");
    print STDOUT "Terminating --multi run, data in ${prefix}mcstas${format_ext}\n";
}

sub my_system {
    # Very simple error checking system call.
    my ($cmd, $err) = @_;
    my $output;
    my $WRITE = new FileHandle;
    my $READ = new FileHandle;
    my $ERR = new FileHandle;
    open3($WRITE,$READ,$ERR,"$cmd ") || die "$cmd: $err\n";
    while (<$READ>) {
	$output = $_;
	print STDERR "$output\n";
    } 
    if (<$ERR>) {
	die "$cmd: $err\n";
    }
}

                    ##########################
                    # Start of main program. #
                    ##########################

# set default plotter
$plotter = defined($ENV{'MCSTAS_FORMAT'}) ?
                $ENV{'MCSTAS_FORMAT'} : "$MCSTAS::mcstas_config{'PLOTTER'}";

parse_args();                         # Parse command line arguments

# Check value of $plotter variable
if ($plotter =~ /0/i) { 
  if ($plotter =~ /binary|float|double/i) {
    $plotter =~ s/binary//; # PGPLOT binary format is invalid. solving.
    $plotter =~ s/float//;
    $plotter =~ s/double//;
  }
  $plotter="McStas"; }
elsif ($plotter =~ /1|2/i) { $plotter="Matlab"; }
elsif ($plotter =~ /3|4/i) { $plotter="Scilab"; }
# assign output format for scans
if ($plotter =~ /PGPLOT|McStas/i) {
  $format_ext        = ".sim";
  $format_assign     =":";    
  $format_start_value="";
  $format_end_value  =""; 
  $format_prefix     ="# ";  
  $format_limprefix  ="x";
} elsif ($plotter =~ /Matlab/i) {
  $format_ext        = ".m";
  $format_assign     ="=";    
  $format_start_value="'";
  $format_end_value  ="';"; 
  $format_prefix     ="% ";  
  $format_limprefix  ="xy";
} elsif ($plotter =~ /Scilab/i) {
  $format_ext        = ".sci";
  $format_assign     ="=";
  $format_start_value="'";
  $format_end_value  ="';";
  $format_prefix     ="// ";
  $format_limprefix  ="xy";
}

# add format option to cmd stack
push @options, "--format='$plotter'";

# Make sure that the current directory appears first in the path;
# contrary to normal use, this is what the user expects here.
$ENV{PATH} = $ENV{PATH} ? ".:$ENV{PATH}" : ".";

if ($exec_test) {
  my $status;
  $status = do_test(sub { print "$_[0]\n"; }, $force_compile, $plotter);
  if (defined $status) { 
    print STDERR "$status"; 
    exit(1);
  } else {
    exit(0);
  }
}

my $scan_info = check_input_params(); # Get variables to scan, if any
$out_file = get_out_file($sim_def, $force_compile, @ccopts);
exit(1) unless $out_file;
exit(1) if $ncount == 0;

$instr_info = get_sim_info("$out_file","--format='$plotter'");

if($numpoints == 1) {
    do_single();
} else {
    if ($multi == 1) {
	print STDERR "Doing multi...\n";
	do_scan_multi($scan_info);
    } else {
	do_scan($scan_info);
    }
}

