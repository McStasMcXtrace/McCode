#! /usr/bin/perl 
#
# Main perl script for running scans (subsequent simulations) with McStas
#
#
#   This file is part of the McStas neutron ray-trace simulation package
#   Copyright (C) 1997-2008, All rights reserved
#   Risoe National Laborartory, Roskilde, Denmark
#   Institut Laue Langevin, Grenoble, France
#
#   This program is free software; you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation; version 3 of the License.
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

# Determine the path to the McStas system directory. This must be done
# in the BEGIN block so that it can be used in a "use lib" statement
# afterwards.
BEGIN {
    ENV_HEADER
}

use constant { true => 1, false => 0 };
use lib $MCSTAS::perl_dir;
use lib $MCSTAS::perl_modules;
require "mccode_config.perl";
use POSIX qw(_exit);

# Overload with user's personal config
if ($ENV{"HOME"} && -e $ENV{"HOME"}."/.".$MCSTAS::mcstas_config{'MCCODE'}."/".$MCSTAS::mcstas_config{'VERSION'}."/mccode_config.perl") {
  print "mcrun: reading local $MCSTAS::mcstas_config{'MCCODE'} configuration from " . $ENV{"HOME"}."/.".$MCSTAS::mcstas_config{'MCCODE'}."/".$MCSTAS::mcstas_config{'VERSION'}."/mccode_config.perl\n";
  require $ENV{"HOME"}."/.".$MCSTAS::mcstas_config{'MCCODE'}."/".$MCSTAS::mcstas_config{'VERSION'}."/mccode_config.perl";
}

use FileHandle;
use strict;

require "mcrunlib.pl";

# Turn off buffering on stdout. This improves the use with the Unix
# "tee" program to create log files of scans.
autoflush STDOUT 1;

# Various parameters determined by the command line.
my ($sim_def, $force_compile, $data_file);
our $data_dir=undef;            # where to store data: undef=--no-output-files
our $no_output_files;
my $ncount = 1e6;               # Number of neutron histories in one simulation
our $numpoints = 1;             # Number of points in scan (if any)
our @params = ();               # List of input parameters
my %vals;                       # Hash of input parameter values
my @options = ();               # Additional command line options (mcstas)
my @ccopts = ();                # Additional command line options (cc)
our $format_ext;                # sim file extension
my $format_assign;              # assignation symbol ':', '='
my $format_start_value;         # symbol before field value
my $format_end_value;           # symbol after field value
my $format_prefix;              # symbol for line prefix
my $format_limprefix;           # symbol for 'x' / 'xy' limits definition
my $exec_test=0;                # flag for McStas package test execution
our $slave = 0;                 # 'slave' hostname for running remotely
my $multi=0;                    # multi machine mode
my @hostlist = ();              # list of remote machines to run on...
my $mpi_enabled = false;        # run with mpi?
my $mpi = 0;                    # how many nodes used with MPI? 0 implies auto.
my $cflags = 1;                 # true if we use CFLAGS, else no CFLAGS is used

our @optim_names = ();          # list of monitor names to optimize
our $optim_flag=0;              # 0: normal scan, 1: optim some monitors, 2: optim all
our @guess;                     # starting set of parameters (centers of scans)
our @scale;                     # ranges of parameters (ratio from center)
our $optim_iterations=0;        # total number of function calls in optim
our @optim_best;
our $max_iteration=20;
our $data_dir_saved = undef;
my  $optim_prec=1e-3;
our $optfile;                   # Default filename for storing of optim history
my  $seed=0;                      # Used for communicating a seed down a --test run

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
    if($text =~ /^([A-Za-z0-9_]+)\=(.*)$/) {
        set_inputpar($1, $2);
    } else {
        die "mcrun: Invalid input parameter specification '$text'";
    }
}

# Read input parameters from parameter file.
sub read_inputpar_from_file {
    my ($filename) = @_;
    open(IN, "<$filename") || die "mcrun: Failed to open parameter file '$filename'";
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
        } elsif(/^--ncount$/ || /^-n$/) {
            $ncount = $ARGV[++$i];
	      } elsif (/^--multi\=(.*)$/ || /^--grid\=(.*)$/) {
            $multi=$1;
        } elsif (/^--multi/ || /^--grid/) {
	        if ($multi == 0) {
                  $multi=2;   # default to dual core/cpu machines
	        }
        } elsif (/^--multi\=(.*)$/ || /^--grid\=(.*)$/) {
            $multi=$!;
        } elsif (/^--host\=(.*)$/ || /^--machine\=(.*)$/ || /^--slave\=(.*)$/) {
        	  $slave=$1;
        } elsif (/^--mpi$/) {
            $mpi_enabled = true;
            $mpi = 2;   # default to dual core/cpu machines
        } elsif (/^--mpi\=(.*)$/) {
            $mpi_enabled = true;
            $mpi = $1;
        } elsif (/^--machines\=(.*)$/) {
            $MCSTAS::mcstas_config{'HOSTFILE'} = $1;
        } elsif (/^--optim\=(.*)$/) {
            $optim_flag = 1;
            push @optim_names, "$1";
        } elsif (/^--optim$/) {
            $optim_flag = 2; # optimize all monitors
        } elsif (/^--optim-prec\=(.*)$/) {
            $optim_prec = $1;
        } elsif (/^--optim-file\=(.*)$/) {
					$optfile = $1;
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
	    $seed=$2;
        } elsif(/^-([s])(.+)$/) {
            push @options, "-$1$2";
        } elsif(/^--(seed)$/) {
            push @options, "--$1=$ARGV[++$i]";
	    $seed=$ARGV[++$i];
	} elsif(/^-([s])$/) {
            push @options, "-$1$ARGV[++$i]";
        } elsif(/^--(format)$/ || /^--(plotter)$/) {
            $MCSTAS::mcstas_config{'PLOTTER'} = $ARGV[$i];
        } elsif(/^--(format)\=(.*)$/ || /^--(plotter)\=(.*)$/) {
            $MCSTAS::mcstas_config{'PLOTTER'} = $2;
        } elsif(/^--test$/) {
            $exec_test=1;
        } elsif(/^--no-cflags$/) {
            $cflags=0;
	 } elsif(/^--(no-output-files)$/) {
	    $no_output_files=1;
	    push @options, "--$1";
        } elsif(/^--(data-only|help|info|trace|gravitation)$/) {
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
    
    # Escape backslashes in Windows paths
    if ($Config{'osname'} eq 'MSWin32') {
      $sim_def =~ s{\\}{\\\\}g;
    }
    
    # If no data dir is explicitly given, generate dirname from time stamp
    if ((!($no_output_files)) && (!($data_dir))) {
      $data_dir = ${sim_def};
      $data_dir =~ s/\.instr//;
      $data_dir .= '_' . POSIX::strftime("%Y%m%d_%H%M%S", localtime);
      print "*** No directory given - placing data in $data_dir ***\n";
    }
    
    # If data dir '.' is explicitly given, undef it to dump files in current dir
    if ($data_dir eq "." || $data_dir eq "./" || $data_dir eq ".\\") {
	print "*** NOTE: Placing your data in . potentially overwriting files ***\n";
	$data_dir=undef;
    }
    
    # tests for grid/mpi support
    if ($mpi_enabled || $multi >= 1) {
      if (! -e $MCSTAS::mcstas_config{'HOSTFILE'}) {
        print STDERR "$MCSTAS::mcstas_config{'RUNCMD'}: No MPI/grid machine list. Running locally.
  Define ".$ENV{"HOME"}."/.".$MCSTAS::mcstas_config{'MCCODE'}."/hosts
  or $MCSTAS::sys_dir/tools/perl/hosts
  or use option --machines=<file>\n";
        $MCSTAS::mcstas_config{'HOSTFILE'} = "";
      }
      if ($mpi_enabled && ($MCSTAS::mcstas_config{'MPICC'} eq "no"
                       || $MCSTAS::mcstas_config{'MPIRUN'} eq "no")) {
        print STDERR "$MCSTAS::mcstas_config{'RUNCMD'}: You have no mpicc/mpirun available, --mpi disabled...\n";
        $mpi_enabled = 0;
      }
    }

    # Adapt parameters to MPI (if used) which overrides grid.
    if ($mpi_enabled && $MCSTAS::mcstas_config{MPICC} ne "no") {
      $multi = 0;
    }

    if ($multi >= 1) {  # grid: test hosts
    		# grid requires a data_dir for output
        if (!$data_dir) { die "$MCSTAS::mcstas_config{'RUNCMD'}: distributed computation requires data_dir directory for storage\n" ; }
    }
    if ($multi >= 1 && $MCSTAS::mcstas_config{'HOSTFILE'} ne "") {
        require Net::Ping;
        # Check that something is available in the .mcstas-hosts
        print STDERR "Pinging $MCSTAS::mcstas_config{'HOSTFILE'} 1 per sec. since you requested --multi...\n"
	  unless (!$MCSTAS::mcstas_config{'GRID_PING'});
        # Read the host file...
        my $pid = open(HOSTFILE,$MCSTAS::mcstas_config{'HOSTFILE'});
        my $host;
        while ($host = <HOSTFILE>) {
            chomp $host;
            # Remove spaces if any...
            $host =~ s! !!g;
            if ($host ne '' && $host !~ /^\s*#/) {  # not empty or comment line
	      if ($MCSTAS::mcstas_config{'GRID_PING'}) {
                my $p = Net::Ping->new();
                my $response = 0;
                $response= $p->ping($host, 1);
                if ($response == 1) {
                    push @hostlist, $host;
                } else {
                    print STDERR "mcrun: Not spawning to host $host: not responding\n";
                }
	      } else {
		push @hostlist, $host;
	      }
            }
        }
        close(HOSTFILE);
    }

    my $cc     = $MCSTAS::mcstas_config{CC};
    my $mcstas_cflags = $MCSTAS::mcstas_config{CFLAGS};

    do {
      my $usage = << "ENDCOM";
"Usage: $MCSTAS::mcstas_config{'RUNCMD'} [-cpnN] Instr [-sndftgahi] params={val|min,max|min,guess,max}
$MCSTAS::mcstas_config{'RUNCMD'} options:
   -c        --force-compile  Force rebuilding of instrument.
   -p FILE   --param=FILE     Read parameters from file FILE.
   -n COUNT  --ncount=COUNT   Set number of neutrons to simulate.
   -N NP     --numpoints=NP   Set number of scan points.
             --grid=NB_CPU    Spawn simulations to multiple machine/cores grid.
             --multi=NB_CPU     see the documentation for more info.
   --mpi     --mpi=NB_CPU     Spread simulation over NB_CPU machines using MPI
   --machines=MACHINES        Read machine names from file MACHINES (MPI/grid)
   --slave=HOST               Execute simulation on distant HOST (SSH grid)
   --optim=COMP               Add COMP to the list of monitors to maximize
                                (optimization criteria, requires Math::Amoeba)
   --optim                    Maximize all monitors
   --optim-prec=PREC          Relative requested accuracy of criteria (1e-3)
   --optim-file=FILENAME      Defines filename for storing optim results.
                                (Defaults to \"mcoptim_XXXX.dat\")
   --test                     Execute $MCSTAS::mcstas_config{'PKGNAME'} selftest and generate report
   --no-cflags                Does not use CFLAGS for faster compilation
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
   --format=FORMAT            Output data files using format FORMAT.
                              (format list obtained from <instr>.out -h)
ENDCOM
    $_=$MCSTAS::mcstas_config{'MCCODE'};
    if (/^mcstas/) {
      $usage .= << "ENDMC";
\"This program both runs mcstas with Instr and the C compiler to build an
independent simulation program. The following environment variables may be
specified for building the instrument:
  MCSTAS        Location of the McStas and component library
                  ($MCSTAS::sys_dir).
  MCSTAS_CC     Name of the C compiler               ($cc)
  MCSTAS_CFLAGS Options for compilation              ($mcstas_cflags)
  MCSTAS_FORMAT Default FORMAT to use for data files ($MCSTAS::mcstas_config{'PLOTTER'})
SEE ALSO: mcstas, mcdoc, mcplot, mcdisplay, mcgui, mcresplot, mcstas2vitess
DOC:      Please visit http://www.mcstas.org/
** No instrument definition name given
ENDMC
    } elsif (/^mcxtrace/) {
      $usage .= << "ENDMX";
\"This program both runs mcxtrace with Instr and the C compiler to build an
independent simulation program. The following environment variables may be
specified for building the instrument:
  MCXTRACE        Location of the McXtrace and component library
                  ($MCSTAS::sys_dir).
  MCXTRACE_CC    Name of the C compiler               ($cc)
  MCXTRACE_CFLAGS Options for compilation              ($mcstas_cflags)
  MCXTRACE_FORMAT Default FORMAT to use for data files ($MCSTAS::mcstas_config{'PLOTTER'})
SEE ALSO: mcxtrace, mxdoc, mxplot, mxdisplay, mxgui
DOC:      Please visit http://www.mcxtrace.org/
** No instrument definition name given
ENDMX
    }
    die "$usage\n";
  } unless $sim_def || $exec_test;

if ($numpoints < 1) { $numpoints=1; }
}

# Check the input parameter specifications for variables to scan.
sub check_input_params {
    my $i = 0;
    my $j = 0;
    my @scanned = ();
    my @minval = ();
    my @maxval = ();
    my @guessval=();
    my $v;
    for $v (@params) {
        if($vals{$v} =~ /^(.+),(.+),(.+)$/) {
            # Variable to scan from min to max, with a guess value for optimization.
            $minval[$j] = $1;
            $maxval[$j] = $3;
            $guessval[$j] = $2;
            $scanned[$j] = $i;
            if($minval[$j] != $maxval[$j] && $numpoints == 1 && $optim_flag == 0) {
                die "mcrun: Cannot scan variable $v using only one data point.
Please use -N to specify the number of points.";
            }
            $j++;
        } elsif($vals{$v} =~ /^(.+),(.+)$/) {
            # Variable to scan from min to max.
            $minval[$j] = $1;
            $maxval[$j] = $2;
            $guessval[$j] = ($1 + $2)/2;
            $scanned[$j] = $i;
            if($minval[$j] != $maxval[$j] && $numpoints == 1 && $optim_flag == 0) {
                die "mcrun: Cannot scan variable $v using only one data point.
Please use -N to specify the number of points.";
            }
            $j++;
        } elsif($vals{$v} =~ /^(.+)$/) {
            # Constant variable (no action)
        } else {
            die "mcrun: Invalid parameter specification '$vals{$v}' for parameter $v";
        }
        $i++;
    }
    return { VARS => \@scanned, MIN => \@minval, MAX => \@maxval, GUESS => \@guessval };
}

# start single simulation: exec_sim(@cmd)
# called either for a single (from main) or a scan (from do_scan)
# may distribute a simulation on a set of machines, as if it was launched locally
sub exec_sim {
  my ($datadir) = @_;
  if (!$multi && $slave eq 0) {
  	exec_sim_local($datadir);	# single sim on local host
  } elsif (!$multi && $slave ne 0) {
  	exec_sim_host($datadir);	# single sim on defined host $slave
	} else {
	  require Proc::Simple;
	  require File::Temp; # for tempdir
		# distribute single simulation over $multi machines
    my @opt = @options;
    if ($MCSTAS::mcstas_config{'HOSTFILE'} eq "" || $MCSTAS::mcstas_config{'SSH'} eq "no") {
      @hostlist = ("localhost");	# uses only local host if no SSH/machine list
    }
    # The number of requested nodes is $multi
    print STDOUT "Distributing simulation $out_file over $multi nodes of @hostlist into $datadir\n";

    # Create tmp dir on local host
    my $griddir;
    if ($MCSTAS::mcstas_config{'TEMP'} ne "no") {
      $griddir = File::Temp::tempdir( "$datadir"."_grid_XXXXX", CLEANUP=>1); # will be removed on master node at exit
    } else {
      $griddir = int(rand(99999));
			$griddir = "$datadir"."_grid_$griddir";
    }
    my $date0 = localtime(time());

    my @pids     =();
    my @datadirs =();
    my @hostnames=();
    my $host_index=0;
    my $hosts = @hostlist;  # number of available nodes
    my $j;
    my $hostncount=int($ncount/$multi);
    for ($j=0; $j<$multi; $j++) {
      my @opt = @options;
      if ($force_compile) {
        push @opt, "--force-compile"; # transfer request for recompilation on all nodes
      }
      if ($ncount) { $pids[$j]     =Proc::Simple->new(); }
      $hostnames[$j]=$hostlist[$host_index];
      $datadirs[$j] ="$griddir" . "_$j";
      if ($j == $multi-1) { $hostncount=$ncount-$j*int($ncount/$multi); }
      # ask mcrun to launch on slaves
      # further option: threads so that we can manage passwords to avoid RSA/DSA keys
      push @opt, map("$_=$vals{$_}", @params);
      push @opt, "--format=PGPLOT";
      push @opt, "--ncount=$hostncount";
      my $cmd="$MCSTAS::mcstas_config{'RUNCMD'} --slave=$hostnames[$j] $out_file --dir=$datadirs[$j]  @opt > $griddir/$hostnames[$j]_$j.log";
      if ($ncount) {
        $pids[$j]->Proc::Simple::start($cmd);  # asynchronous exec
      }
      print STDERR "Process $j at $hostnames[$j] started.\n";
      print "$cmd\n";
      $host_index++;
      if ($host_index >= $hosts) { $host_index=0; } # loop on host names
    } # end for
    # wait for children to end
    print STDERR "Waiting for child processes to end...\n";
    my $stop=0;
    # wait loop
    while ($stop < $multi) {  # wait until all hosts have finished
      sleep(1); # avoid using CPU for nothing
      for ($j=0; $j<$multi; $j++) {
        if ($pids[$j] && $pids[$j]->Proc::Simple::poll() == 0 || !$ncount) {
          print STDERR "Process $j at $hostnames[$j] terminated.\n";
          $stop++;
          $pids[$j]=0;
        }
      }
    }
    # merge data sets from $griddir into $data_dir (force mode as data_dir already exists from mcrun)
    my $cmd="mcformat";
    if ($Config{'osname'} eq 'MSWin32') {
      # Only apply suffix on Win32
      $cmd .= ".$MCSTAS::mcstas_config{'EXE'}";
    }
    $cmd .= " --merge --force --dir=$griddir --format=$MCSTAS::mcstas_config{'PLOTTER'} ";
    for ($j=0; $j<$multi; $j++) {
      $cmd .= "$datadirs[$j] ";
    }
    $cmd .= "> $griddir/mcformat.log";
    print STDERR "Merging grid results: $cmd\n";
    if ($ncount) { my_system($cmd,""); }
    # clean up local datadirs
    for ($j=0; $j<$multi; $j++) {
      File::Path::rmtree("$datadirs[$j]");
    }
    # move mcformat merged data set one level up and delete temporary dirs
    File::Path::rmtree("$datadir");
    unlink("$datadir");
    rename("$griddir/$datadirs[0]",$datadir);

    # build up catenated log files into the merged data set dir
    open(WRITE,">>$datadir/mcstas.log") || die "Simulation failed (can not merge results from $datadir)";
    my $date1 = localtime(time());
    print WRITE "################################################################################\n";
    print WRITE "# Log file $datadir/mcstas.log generated by McStas/mcrun\n";
    print WRITE "# Start Date: $date0\n";
    print WRITE "# End   Date: $date1\n";
    print WRITE "# Current instrument: $sim_def\n";
    print WRITE "# Current results:    $data_dir\n";
    print WRITE "# Distributed simulation log using $multi nodes\n";
    for ($j=0; $j<=$multi; $j++) { # last step is merge log
      my $log;
      print WRITE "################################################################################\n";
      if ($j==$multi) {
        $log="$griddir/mcformat.log";
        print WRITE "# logfile $log from merge (mcformat)\n";
      }
      else {
        $log="$griddir/$hostnames[$j]_$j.log";
        copy($log,"$datadir/$hostnames[$j]_$j.log");
        print WRITE "# logfile $hostnames[$j]_$j.log from $hostnames[$j] node $j\n";
      }
      open(READ, $log);
      while (<READ>) {
          print WRITE "$_";
          if ($j==$multi) { print STDOUT  "$_"; } # send mcformat result to stdout
      }
      close(READ);
    }
    close(WRITE);

    # removal of $griddir is done automatically
    exit(0);
  }
}

# start a simulation on localhost, with MPI support
sub exec_sim_local {
  my ($datadir) = @_;
  my @opt = @options;
  my @cmd=();

  if ($data_file)    { push @opt, "--file=$data_file" ; }
  if ($datadir)      { push @opt, "--dir=$datadir"; }

  # add format option to cmd stack
  push @opt, "--format=$MCSTAS::mcstas_config{'PLOTTER'}";
  push @opt, "--ncount=$ncount";

  # ToDo: This is broken in that it does not show the proper quoting
  # that would be necessary if the user actually wanted to run the
  # command manually. (Note that the exec() call is correct since it
  # does not need any quoting).
  if ($mpi_enabled && $MCSTAS::mcstas_config{'MPIRUN'} ne "no") {
    push @cmd, "$MCSTAS::mcstas_config{'MPIRUN'}";
    my $localonly = 1;
    if ($MCSTAS::mcstas_config{'HOSTFILE'} ne "") {
      push @cmd, " -machinefile $MCSTAS::mcstas_config{'HOSTFILE'}";
      $localonly = 0;
    }
    if(!($mpi eq "auto")) {
      if($mpi >= 0) {
        push @cmd, " -np $mpi";
      }
    } else {
      print STDERR "Using system default number of mpirun -np processes\n";
    }
  }
  push @cmd, $out_file;
  push @cmd, @opt;
  push @cmd, map("$_=$vals{$_}", @params);
  print "@cmd\n";

  # execute full command line
  if ($ncount) {
    exec join(' ',@cmd);  #may call exec as nothing has to be done afterwards
  } else {
    exit(-1);
    # Simulation not executed (ncount=0)
  }
}

# send a simulation to host $slave, retrieve data and remove tmp dir
sub exec_sim_host {
	my ($datadir) = @_;
  my @opt = @options;
	use Sys::Hostname;

	# test if this is local host
	my $mastername = hostname;  # Sys::Hostname call
	if ($slave =~ /localhost/ || $slave =~ $mastername) { # localhost...
		exec_sim_local($datadir);
		# never returns (exept termination of mcrun) as exec_sim_local calls exec
	} else {
		require File::Temp; # for tempfile
		if (!$datadir) { die "mcrun: exec_sim_host: require data_dir directory for storage\n" ; }
		push @opt, "--dir=$slave";
		push @opt, "--ncount=$ncount";

    # add format option to cmd stack
    push @opt, "--format=$MCSTAS::mcstas_config{'PLOTTER'}";
		my $tmpname;

		# make distant tmpdir (ssh)
		if ($MCSTAS::mcstas_config{'TEMP'} ne "no") {
		  my $fh;
			($fh, $tmpname) = File::Temp::tempfile("$MCSTAS::mcstas_config{'RUNCMD'}_$slave"."_XXXXX", UNLINK => 1); # throw file handle
			unlink $fh;
		} else {
		  $tmpname = int(rand(99999));
			$tmpname = "$MCSTAS::mcstas_config{'RUNCMD'}_$slave"."_$tmpname";
		}
		print STDERR "Sending simulation $sim_def and local data files to $slave:$tmpname\n";
		host_ssh($slave, "mkdir $tmpname");
		# send content of current directory (data files)
		host_scp("*",  "$slave:$tmpname/");
		# make sure we send instrument in tmpdir (scp)
		host_scp($sim_def,  "$slave:$tmpname/$sim_def");
		# compile locally if required (update C code)
		my $v;
		($out_file, $v) = get_out_file($sim_def, $force_compile, $mpi, $cflags, @ccopts);
		if ($v->{'cc_cmd'} eq "") {
		  print STDERR "Failed to compile instrument $sim_def on slave $slave. Using master executable $out_file.\n";
		  $force_compile=0;
		}
		if ($force_compile) {
	    # force compilation on node
	    print STDERR "Compiling on node $slave: $v->{'cc_cmd'}\n";
	    host_scp($v->{'c_name'},  "$slave:$tmpname/$v->{'c_name'}");
	    host_ssh($slave,  "cd $tmpname && $v->{'cc_cmd'}"); # create instr.out
		} else {
		  host_scp($out_file, "$slave:$tmpname/$out_file");   # use master exec
		}
		print STDERR "Starting computation on node $slave\n";
		# start sim (ssh). This creates e.g. $tmpname/$datadir
		my @cmd = ($out_file, @opt, map("$_=$vals{$_}", @params));
		print STDERR "ssh $slave \"cd $tmpname && ./@cmd\"\n";
		host_ssh($slave, "cd $tmpname && ./@cmd");
		# retrieve data in $datadir or $pwd (scp) recursively
		print STDERR "Retrieving simulation $datadir data files from $slave:$tmpname\n";
		host_scp("$slave:$tmpname/$slave",  $datadir ? "$datadir" : ".", 1);
		# remove tmpdir on host (ssh)
		host_ssh($slave, "rm -rf $tmpname");
		exit(0);
	}
}

# send a command to a host (does not die if failed)
sub host_ssh {
	my ($host,$cmd) = @_;
	# further option: Net::SSH         (libnet-ssh)
	if ($ncount) {
	  my_system("$MCSTAS::mcstas_config{'SSH'} $host \"$cmd\"","# ssh $host \"$cmd\"");
	} else {
	  print STDERR "# $MCSTAS::mcstas_config{'SSH'} $host \"$cmd\"\n";
	}
}

# send/receive a file to/from a host (does not die if failed)
sub host_scp {
	my ($orig,$dest,$recursive) = @_;
	# further option: Net::SCP::Expect (libnet-scp-expect)
	if ($ncount) {
	  if (defined($recursive) && $recursive == 1) {
	    my_system("$MCSTAS::mcstas_config{'SCP'} -Crp $orig $dest","# scp $orig $dest");
	  } else {
	    my_system("$MCSTAS::mcstas_config{'SCP'} -Cp $orig $dest","# scp $orig $dest");
	  }
	} else {
	  print STDERR "# $MCSTAS::mcstas_config{'SCP'} -Crp $orig $dest\n";
	}
}

# Handle output of header information for .dat files and .sim information files.
sub do_instr_header { # dies if failed
    my ($prefix, $OUT) = @_;
    print $OUT join("", map("$prefix$_", @{$instr_info->{'RAW'}}));
}

# Write Matlab function header to datafile
sub do_instr_init {
    my ($OUT, $filename) = @_;
    my $funcname=$filename;
    if ($funcname =~ m'^mcstas\.[^/]*$') { $funcname = "mcstas"; }
    else { $funcname =~ s/\./_/; }
    if ($MCSTAS::mcstas_config{'PLOTTER'} =~ /Matlab/i) {
        print $OUT <<ENDCODE
function mcstas = get_mcstas(p)
% Embedded function for building 'mcplot' compatible structure
% Matlab with text headers function
% PW, DTU, 2013
%
% import data using $filename; s=get_mcstas('plot');
if nargout == 0 | nargin > 0, p=1; else p=0; end
ENDCODE
    }


}

# Write Matlab embedded plotting functions to datafile
sub do_sim_tail{
    my ($OUT) = @_;
    if ($MCSTAS::mcstas_config{'PLOTTER'} =~ /Matlab/i) {
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
    }
}


sub do_sim_header {
    my ($prefix, $OUT) = @_;
    my $date = localtime(time());
    my $format_member=".";
    my $param_field  ="Param";
    if ($MCSTAS::mcstas_config{'PLOTTER'} =~ /McStas|PGPLOT/i) { $format_member=": "; }
    print $OUT "${prefix}Date$format_assign $format_start_value$date$format_end_value\n";
    print $OUT "${prefix}Ncount$format_assign $format_start_value$ncount$format_end_value\n";
    print $OUT "${prefix}Numpoints$format_assign $format_start_value$numpoints$format_end_value\n";
    if ($MCSTAS::mcstas_config{'PLOTTER'} =~ /Matlab/i) { $param_field = "parameters"; }
    
    if ($MCSTAS::mcstas_config{'PLOTTER'} =~ /Matlab/i) {
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
    my $yvars;
    if(!(ref($youts) eq 'ARRAY')){
      $yvars = $youts;
    } else {
      $yvars = join " ", @$youts;
    }
    my $xlabel;
    my $min;
    my $max;
    if (@{$info->{VARS}} == 0 || $optim_flag) {
      $xlabel = "Point number"; $scannedvars="Point"; $xvars="Point"; $min=1; $max=$numpoints;
      if ($optim_flag) { push @$xvar_list, $xvars; }
    } else {
      $xlabel = $params[$info->{VARS}[0]];
      $scannedvars = join ", ", map($params[$_], @{$info->{VARS}});
      $xvars = join " ", map($params[$_], @{$info->{VARS}});
      push @$xvar_list, map($params[$_], @{$info->{VARS}});
      $min = $info->{MIN}[0]; $max = $info->{MAX}[0];
    }
    # Here, we need special handling of the Matlab output cases, since
    # each monitor dataset must be defined in its own class 'data' structure for mcplot
    if ($MCSTAS::mcstas_config{'PLOTTER'} =~ /McStas|PGPLOT/i) {
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
    } elsif ($MCSTAS::mcstas_config{'PLOTTER'} =~ /Matlab/i) {
        if (! $datablock eq "") {
          print $OUT "DataBlock=[$datablock];\n";
          # Now loop across all the youts...
          my $idx = scalar(@$xvar_list) + 1;
          foreach my $y_pair (@$youts) {
            # Cut out the relevant descriptor
            my $start_char = index($y_pair, "(") + 1;
            my $end_char = index($y_pair, ",") - $start_char;
            my $y_label = substr($y_pair, $start_char, $end_char);

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
          } # foreach
          print $OUT "clear Datablock;\n";
        }
    }
}

# Write <NAME>.sim information file, to be read by mcplot.
sub output_sim_file {
    my ($filename, $info, $youts, $variables, $datfile, $datablock) = @_;
    my $SIM = new FileHandle;
    my $loc_prefix = "";
    my $instr_name = $sim_def;
    my $i;
    my $namvar;
    if (@{$info->{VARS}} == 0 || $optim_flag) { $namvar = "Point number"; }
    else {
      $i = $info->{VARS}[0]; # Index of variable to be scanned
      $namvar = defined($i) && $params[$i] ? $params[$i] : "nothing";
    }
    my $minvar = $info->{MIN}[0] ? $info->{MIN}[0] : 0;
    my $maxvar = $info->{MAX}[0] ? $info->{MAX}[0] : 0;

    $instr_name =~ s/\.instr$//;        # Strip any trailing ".instr" extension ...
    open($SIM, ">$filename") ||
        die "mcrun: Failed to write info file '$filename'";
    autoflush $SIM 1;                # Preserves data even if interrupted.

    if ($MCSTAS::mcstas_config{'PLOTTER'} =~ /McStas|PGPLOT/i) {
      do_instr_header("", $SIM);
      print $SIM "\nbegin simulation\n";
      $loc_prefix = "";
    } elsif ($MCSTAS::mcstas_config{'PLOTTER'} =~ /Matlab/i) {
      do_instr_init($SIM, $filename);
      do_instr_header("% ", $SIM);
      print $SIM "\nsim.class='simulation';\n";
      print $SIM "sim.name='$filename';\n";
      $loc_prefix = "sim.";
    } else {
      do_instr_init($SIM, $filename);
      do_instr_header("// ", $SIM);
      print $SIM "\nsim = struct(); sim.class='simulation';\n";
      print $SIM "sim.name='$filename';\n";
      $loc_prefix = "sim.";
    }
    do_sim_header($loc_prefix, $SIM);
    if ($MCSTAS::mcstas_config{'PLOTTER'} =~ /McCode|PGPLOT/i) {
      print $SIM "end simulation\n\nbegin data\n";
    } elsif ($MCSTAS::mcstas_config{'PLOTTER'} =~ /Matlab/i) {
      print $SIM "\nmccode.sim = sim; clear sim;\n";
      print $SIM "mccode.File= '$filename';\n";
      print $SIM "mccode.instrument.Source= '$sim_def';\n";
      print $SIM "mccode.instrument.parent= 'mcstas';\n";
      print $SIM "mccode.instrument.class = 'instrument';\n";
      print $SIM "mccode.instrument.name  = '$instr_name';\n";
      print $SIM "mccode.instrument.Parameters  = '...';\n";
      print $SIM "\ndata.class = 'superdata';\n";
      print $SIM "data.name = 'Scan of $namvar';\n";
      print $SIM "data.scannedvar = '$namvar';\n";
      print $SIM "data.numpoints  = $numpoints;\n";
      print $SIM "data.minvar  = $minvar;\n";
      print $SIM "data.maxvar  = $maxvar;\n";
      $loc_prefix = "data.";
    } else {
      print $SIM "\nmccode = struct(); mcstas.sim = 0; mcstas.sim = sim; clear sim;\n";
      print $SIM "mccode.File= '$filename';\n";
      print $SIM "instrument = struct();\n";
      print $SIM "instrument.Source= '$sim_def';\n";
      print $SIM "instrument.parent= 'mcstas';\n";
      print $SIM "instrument.class = 'instrument';\n";
      print $SIM "instrument.name  = '$instr_name';\n";
      print $SIM "instrument.Parameters  = '...';\n";
      print $SIM "mccode.instrument = 0; mccode.instrument = instrument; clear instrument;\n";
      print $SIM "\ndata = struct(); data.class = 'superdata';\n";
      print $SIM "data.name = 'Scan of $namvar';\n";
      print $SIM "data.scannedvar = '$namvar';\n";
      print $SIM "data.numpoints  = $numpoints;\n";
      print $SIM "data.minvar  = $minvar;\n";
      print $SIM "data.maxvar  = $maxvar;\n";
      $loc_prefix = "data.";
    }
    do_data_header($loc_prefix, $SIM, $info, $youts, $variables, $datfile, $datablock);
    if ($MCSTAS::mcstas_config{'PLOTTER'} =~ /McStas|PGPLOT/i) {
      print $SIM "end data\n";
    } elsif ($MCSTAS::mcstas_config{'PLOTTER'} =~ /Matlab/i) {
      print $SIM "\nmcstas.sim.data = data; clear data;\n";
      do_sim_tail($SIM);
    } else {
      print $SIM "\nmcstas.sim.data = 0; mcstas.sim.data = data; clear data;\n";
      do_sim_tail($SIM);
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
      if ($ncount) {
        if(mkdir($data_dir, 0777)) {
            $prefix = "$data_dir/";
        } else {
            die "Error: unable to create directory '$data_dir'.\n(Maybe the directory already exists?)";
            print $MCSTAS::mcstas_config{'PLOTTER'}; # unreachable code, avoids warning for mcstas_config
        }
      } else {
        print STDERR "# mkdir $data_dir\n";
      }
    }
    # Use user-specified output file name, with a default of "mccode.dat".
    my $datfile = ($data_file || "mccode.dat");
    # Add a default '.dat' extension if no other extension given.
    $datfile .= ".dat" unless $datfile =~ m'\.[^/]*$'; # Quote hack ';
    my $simfile = $datfile;
    $simfile =~ s/\.dat$//;         # Strip any trailing ".dat" extension ...
    $simfile .= $format_ext;        # ... and add ".sim|m|sci" extension.
    my $DAT;
    # Only initialize / use $DAT datafile if format is PGPLOT
    if ($MCSTAS::mcstas_config{'PLOTTER'} =~ /PGPLOT|McCode/i) {
      $DAT = new FileHandle;
      if ($ncount) {
        open($DAT, ">${prefix}$datfile");
        autoflush $DAT 1;                # Preserves data even if interrupted.
      }
    } else {
      $datfile = $simfile;             # Any reference should be to the simfile
    }
    our $OPT;
    our @opt_out;

    my $firsttime = 1;
    my $variables = "";
    my @youts = ();
    our $point;
    my @lab_datablock = ();          # Storing of scan data in variable
                                     # for saving datablock in matlab
    my $datablock = "";              # 'sim' file.
    my $Monitors_nb=0;
    my $found_invalid_scans=0;
    # Initialize the @lab_datablock according to 'format'
    for($point = 0; $point < $numpoints; $point++) {
        my @Monitors = ();
        my @Monitors_E = ();
        my @Intensities = ();
        my @Errors = ();
        my @Rays = ();
        if (($point >= 0) && ($point <= $numpoints)) {
            my $out = "";
            my $j;
            for($j = 0; $j < @{$info->{VARS}}; $j++) {
                my $i = $info->{VARS}[$j]; # Index of variable to be scanned
                if ($numpoints > 1) {
                  $vals{$params[$i]} =
                      ($info->{MAX}[$j] - $info->{MIN}[$j])/($numpoints - 1)*$point +
                      $info->{MIN}[$j];
                } else {
                  $vals{$params[$i]} =
                      ($info->{MAX}[$j] + $info->{MIN}[$j])/2;
                }
                $out .= "$vals{$params[$i]} ";
                $variables .= "$params[$i] " if $firsttime && !$found_invalid_scans
            } # end for
            if (@{$info->{VARS}} == 0)
            {
              $out .= "$point ";
              $variables .= "Point " if $firsttime && !$found_invalid_scans;
            }
            # Decide how to handle output files.
            my $output_opt= $data_dir ? "$data_dir/$point" : undef;
            my $got_error = 0;
            our $pid;
            if ($Config{'osname'} eq 'MSWin32') {
                # Win32 needs all possible parameters here, since we can not open(SIM,"-|");
                my @cmdlist = ("$MCSTAS::mcstas_config{'RUNCMD'}",
                              $out_file, "--ncount=$ncount", @options, map("$_=$vals{$_}", @params),
                              $force_compile && ($multi >= 1 || $slave ne 0) ? "--force-compile" : "",
                							$output_opt ? "--dir=$output_opt" : "--no-output-files",
                							"--format=$MCSTAS::mcstas_config{'PLOTTER'}");
                $pid = open(SIM, "@cmdlist |");
            } else {
                $pid = open(SIM, "-|");
            } # end if Config
            die "mcrun: Failed to spawn simulation command" unless defined($pid);
            if($pid) {              # Parent
                # install SIG handler for scans
                sub sighandler {
                  my $signame = shift;
                  kill $signame, $pid; # kill scan (child)
                  if ($signame eq "INT" || $signame eq "TERM") {
                    print STDERR "mcrun: Recieved signal $signame during scan ($point of $numpoints). Finishing.\n";
                    $point = $numpoints;
                  }
                } # end sighandler
                if ($optim_flag == 0) {
                  $SIG{'INT'}  = \&sighandler;
                  $SIG{'TERM'} = \&sighandler;
                  if ($Config{'osname'} ne 'MSWin32') {
                    $SIG{'USR1'} = \&sighandler;
                    $SIG{'USR2'} = \&sighandler;
                    $SIG{'HUP'}  = \&sighandler;
                  }
                }
                my $Counter;
		my $Counter2=0;
                # analyze scan step output
                while(<SIM>) {
                    chomp;
                    if(/Detector: ([^ =]+_[A-Z]) *= *([^ =]+) ([^ =]+_ERR) *= *([^ =]+) ([^ =]+_N) *= *([^ =]+) *(?:"[^"]+" *)?$/) { # Quote hack -> ")
                        my $sim_I = $2;
                        my $sim_err = $4;
                        my $sim_N = $6;
                        my $index = -1;
                        for ($Counter = 0; $Counter < @Monitors; $Counter++) {
                            if ($1 eq $Monitors[$Counter]) {
                              $index = $Counter;
                            }
                        }
                        if ($index == -1) {
                            # Didn't record this monitor before
                            push @Monitors, $1;
                            push @Monitors_E, $3;
                            push @Intensities, $sim_I;
                            push @Errors, $sim_err;
                            push @Rays, $sim_N;
                        } else {
			    # Heard about this monitor before, likely a secound output
			    $Counter2++;
                            push @Monitors, ${1}."_".${Counter2};
                            push @Monitors_E, $3;
                            push @Intensities, $sim_I;
                            push @Errors, $sim_err;
                            push @Rays, $sim_N;
			  }
                    } elsif(m'^Error:') { # quote hack '
                        $got_error = 1;
                    } # end while SIM
                    print "$_\n";
                } # end while
                # Output final monitor data:
                if (!$Monitors_nb) { $Monitors_nb = @Monitors; }  # store number of monitors (valid scan)
                if ($Monitors_nb > 0 && $Monitors_nb != @Monitors) {
                  for ($Counter = 0; $Counter < $Monitors_nb; $Counter++) {
                    $out .= " 0 0"; # add empty line to scan data when error prevents reading Monitors
                  }
                } else {
                  for ($Counter = 0; $Counter < @Monitors; $Counter++) {  # skipped if no Monitor read
                      if ($firsttime){
                        $variables .= " $Monitors[$Counter] $Monitors_E[$Counter]";
                        push @youts, "($Monitors[$Counter],$Monitors_E[$Counter])";
                      }
                      $out .= " $Intensities[$Counter] $Errors[$Counter]";
                  }
                }
                # remove SIG handler
                if ($optim_flag == 0) {
                  $SIG{'INT'}  = 'DEFAULT';
                  $SIG{'TERM'} = 'DEFAULT';
                  if ($Config{'osname'} ne 'MSWin32') {
                    $SIG{'USR1'} = 'DEFAULT';
                    $SIG{'USR2'} = 'DEFAULT';
                    $SIG{'HUP'}  = 'DEFAULT';
                  }
                }
            } else {                # Child
                open(STDERR, ">&STDOUT") || die "mcrun: Can't dup stdout";
                exec_sim($output_opt);
            } # end if pid

            my $ret = close(SIM);
            if ($ncount && ($got_error || (! $ret && ($? != 0 || $!)))) {
              $found_invalid_scans++;
              print "mcrun: Exit #$found_invalid_scans due to error returned by simulation program ($out_file=$?)\n" ;
              # continue scan as further steps may be OK, and final scan dimension is right
            }
            # output section, only when we know how many monitors there are
            if ($Monitors_nb) {
              if ($firsttime eq 1) {  # output header for first valid scan step
                if ($ncount && $MCSTAS::mcstas_config{'PLOTTER'} =~ /PGPLOT|McStas/i) {
                    output_dat_header($DAT, "# ", $info, \@youts, $variables, $datfile);
                }
                $firsttime = 0;
              } else {
                push @lab_datablock, "\n";
              }
              # add scan data line
              if ($ncount && $MCSTAS::mcstas_config{'PLOTTER'} =~ /PGPLOT|McStas/i) {
                      print $DAT "$out\n";
              }
              push @lab_datablock, "$out";
            }
        } # end if point
    } # end for point
    if ($ncount) {
     if ($MCSTAS::mcstas_config{'PLOTTER'} =~ /PGPLOT|McStas/i) {
        close($DAT);
      }
      $datablock = join(" ", @lab_datablock);
      output_sim_file("${prefix}$simfile", $info, \@youts, $variables, $datfile, $datablock);

      print "Output file: '${prefix}$datfile'\nOutput parameters: $variables\n";

    } else {
      die "Ending simulation scan (ncount=0)\n";
    }
    if ($found_invalid_scans) {
      die "mcrun: Error: Simulation $out_file $data_dir returned $found_invalid_scans invalid scan steps,
       which intensity was set to zero in data file $datfile.\n"; }

    return ($datablock,$variables, @youts);
}

sub my_system {
    use IPC::Open3;
    # Very simple error checking system call.
    my ($cmd, $err) = @_;
    my $output;
    my $WRITE = new FileHandle;
    my $READ = new FileHandle;
    my $ERR = new FileHandle;
    IPC::Open3::open3($WRITE,$READ,$ERR,"$cmd ") || die "$cmd: $err (could not start)\n";
    while (<$READ>) {
        $output = $_;
        print STDERR "$output\n";
    }
    if (<$ERR> && $err ne "") {
        my @emsg = <$ERR>; # The error comes from here
        print STDERR "$cmd: $err: @emsg\n";
    }
    if (!<$ERR>) { return 0; } else { return "<$ERR>"; }
}

                    ##########################
                    # Start of main program. #
                    ##########################

parse_args();                         # Parse command line arguments

# assign output format for scans. Default is PGPLOT

$format_ext        = ".sim";
$format_assign     =":";
$format_start_value="";
$format_end_value  ="";
$format_prefix     ="# ";
$format_limprefix  ="x";
if ($MCSTAS::mcstas_config{'PLOTTER'} =~ /Matlab/i) {
  $format_ext        = ".m";
  $format_assign     ="=";
  $format_start_value="'";
  $format_end_value  ="';";
  $format_prefix     ="% ";
  $format_limprefix  ="xy";
} elsif ($numpoints > 1 && $MCSTAS::mcstas_config{'PLOTTER'} !~ /PGPLOT|McStas/i) {
  print STDERR "mcrun: Warning: format $MCSTAS::mcstas_config{'PLOTTER'} does not support parameter scans\n";
}

if ($exec_test && $ncount) {
  my $status;
  $status = do_test(sub { print "$_[0]\n"; }, $force_compile, $MCSTAS::mcstas_config{'PLOTTER'}, $exec_test, $cflags, $mpi, $ncount, $sim_def, $seed);
  if (defined $status) {
    print STDERR "mcrun: $status\n";
    exit(1);
  } else {
    exit(0);
  }
}

our $scan_info = check_input_params(); # Get variables to scan, if any
# force compile only on localhost
($out_file, undef) = get_out_file($sim_def, $force_compile && !$multi && $slave eq 0, $mpi, $cflags, @ccopts);
exit(1) unless $out_file;
if ($Config{'osname'} eq 'darwin') {
  my_system("install_name_tool -add_rpath $MCSTAS::sys_dir/miniconda3/lib $out_file");
}

$instr_info = get_sim_info("$out_file","--format=$MCSTAS::mcstas_config{'PLOTTER'}");

if($numpoints == 1 && $optim_flag == 0) {
  # single simulation
  print "Running simulation '$out_file' ...\n";
  exec_sim($data_dir) || die "mcrun: Failed to run simulation";
} else {
	if ($optim_flag && $MCSTAS::mcstas_config{'AMOEBA'}) {
		# main optimization routine. Use optim_names, scan_info, numpoints, data_dir and vals
		require "mcoptimlib.pl";
		$max_iteration = 4*@{$scan_info->{VARS}};
		if ($numpoints > 1) { $max_iteration = $numpoints; }
		my $j;
		my $i;

		# get guessed parameter set (MAX-MIN) and scale. Print optimization config
		die "Specify parameter range=MIN,MAX or MIN,GUESS,MAX for optimization\n" unless @{$scan_info->{VARS}};

		print "Starting optimization of $sim_def paramaters:\n";
		for($j = 0; $j < @{$scan_info->{VARS}}; $j++) {
		  $i = $scan_info->{VARS}[$j]; # Index of variable to be scanned
		  $guess[$j] =  $scan_info->{GUESS}[$j];
		  $scale[$j] = ($scan_info->{MAX}[$j] - $scan_info->{MIN}[$j])/2;
		  print "$params[$i]=$guess[$j] ";
		}
		print "\nto maximize ($max_iteration iterations, within $optim_prec accuracy):\n  ";
		if ($optim_flag > 1) {
		  print "All monitors";
		} else {
		  for($i = 0; $i < @optim_names; $i++) {
		    print "$optim_names[$i] ";
		  }
		}
		print "\n";

		# set numpoints to 1 for do_scan
		$numpoints = 1;

		# undefine $data_dir not to have dir overwrite error in do_scan
		$data_dir_saved = $data_dir ? $data_dir : undef;
		undef($data_dir);

		$SIG{'INT'}  = \&sighandler_optim;
		$SIG{'TERM'} = \&sighandler_optim;
		if ($Config{'osname'} ne 'MSWin32') {
		  $SIG{'USR1'} = \&sighandler_optim;
		  $SIG{'USR2'} = \&sighandler_optim;
		  $SIG{'HUP'}  = \&sighandler_optim;
		}

		# call Amoeba to minimize criteria
		my ($p,$minimum) =
		  MinimiseND(\@guess,\@scale,\&minimize_function,$optim_prec,$max_iteration);

		# constrain within limits
		for($j = 0; $j < @{$scan_info->{VARS}}; $j++) {
		  if    ($optim_best[$j] > $scan_info->{MAX}[$j]) { $optim_best[$j] = $scan_info->{MAX}[$j]; }
		  elsif ($optim_best[$j] < $scan_info->{MIN}[$j]) { $optim_best[$j] = $scan_info->{MIN}[$j]; }
		}

		# display result
		if ($optim_iterations >= $max_iteration) {
		  print "Optimization failed (no convergence after $optim_iterations iterations).\nIncrease number of iterations (-N 100) or looser required accuracy (--optim-prec=1e-2).\nLast estimates:\n";
		}

		save_optimization(1);

	} else {
		if ($optim_flag && not $MCSTAS::mcstas_config{'AMOEBA'}) {
		  print STDERR "Optimization not available (install perl Math::Amoeba first)\n";
		}
		do_scan($scan_info); # single iteration
	}
}
exit(0);
