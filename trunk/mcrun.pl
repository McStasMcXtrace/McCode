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
    $MCSTAS::perl_dir = "$MCSTAS::sys_dir/tools/perl"
}
use lib $MCSTAS::perl_dir;

use FileHandle;
use strict;

require "mcrunlib.pl";
require "mcstas_config.perl";

# Turn off buffering on stdout. This improves the use with the Unix
# "tee" program to create log files of scans.
autoflush STDOUT 1;

# Various parameters determined by the command line.
my ($sim_def, $force_compile, $data_dir, $data_file);
my $ncount = 1e6;                # Number of neutron histories in one simulation
my $numpoints = 1;                # Number of points in scan (if any)
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
    if($text =~ /^([a-zæøåA-ZÆØÅ0-9_]+)\=(.*)$/) {
        set_inputpar($1, $2);
    } else {
        die "Invalid input parameter specification '$text'";
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
        if(/^--force-compile$/ || /^-c$/) {
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
            push @options, "--$1=$ARGV[++$i]";
            $plotter = $ARGV[$i];
        } elsif(/^--(format)\=(.*)$/ || /^--(plotter)\=(.*)$/) {
            push @options, "--$1=$2";
            $plotter = $2;
        } elsif(/^--(data-only|help|info|trace|no-output-files|gravitation)$/) {
            push @options, "--$1";
        } elsif(/^-([ahitg])$/) {
            push @options, "-$1";
        }
        # Non-option arguments.
        elsif(/^-/) {                # Unrecognised option
            push @ccopts, "$_";
        } elsif(/^([a-zæøåA-ZÆØÅ0-9_]+)\=(.*)$/) {
            set_inputpar($1, $2);
        } else {                        # Name of simulation definition
            if($sim_def) {
                die "Only a single instrument definition may be given";
            } else {
                $sim_def = $_;
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
   --format=FORMAT            Output data files using format FORMAT.
                              (format list obtained from <instr>.out -h)
This program both runs mcstas with Instr and the C compiler to build an
independent simulation program. The following environment variables may be 
specified for building the instrument:
  MCSTAS        Location of the McStas and component library 
                (e.g. /usr/local/lib/mcstas).
  MCSTAS_CC     Name of the C compiler (e.g. cc or gcc)
  MCSTAS_CFLAGS Options for compilation (e.g. '-O')
  MCSTAS_FORMAT Default FORMAT to use for data files
SEE ALSO: mcstas, mcdoc, mcplot, mcrun, mcgui, mcresplot, mcstas2vitess
DOC:      Please visit http://neutron.risoe.dk/mcstas/
** No instrument definition name given\n" unless $sim_def;
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
                die "Cannot scan variable $v using only one data point.
Please use -N to specify the number of points.";
            }
            $j++;
        } elsif($vals{$v} =~ /^(.+)$/) {
            # Constant variable (no action).
        } else {
            die "Invalid parameter specification '$vals{$v}' for parameter $v";
        }
        $i++;
    }
    return { VARS => \@scanned, MIN => \@minval, MAX => \@maxval };
}

sub exec_sim {
    my (@options) = @_;
    my @cmdlist = ($out_file, @options, map("$_=$vals{$_}", @params));
    # ToDo: This is broken in that it does not show the proper quoting
    # that would be necessary if the user actually wantet to run the
    # command manually. (Note that the exec() call is correct since it
    # does not need any quoting).
    print join(" ", @cmdlist), "\n";
    exec @cmdlist;
}

# Handle the case of a single run of the simulation (no -N option).
sub do_single {
    push @options, "--dir=$data_dir" if $data_dir;
    push @options, "--file=$data_file" if $data_file;

    print "Running simulation '$out_file' ...\n";
    exec_sim(@options) || die "Failed to run simulation";
}


# Handle output of header information for .dat files and .sim information files.
sub do_instr_header {
    my ($prefix, $OUT) = @_;
    print $OUT join("", map("$prefix$_", @{$instr_info->{'RAW'}}));
}

sub do_sim_header {
    my ($prefix, $OUT) = @_;
    my $date = localtime(time());
    my $format_member=".";
    if ($plotter =~ /McStas|PGPLOT/i) { $format_member=": "; }
    print $OUT "${prefix}Date$format_assign $format_start_value$date$format_end_value\n";
    print $OUT "${prefix}Ncount$format_assign $format_start_value$ncount$format_end_value\n";
    print $OUT "${prefix}Numpoints$format_assign $format_start_value$numpoints$format_end_value\n";
    my $paramtext = join("\n", map("${prefix}Param$format_member$_ = $format_start_value$vals{$_}$format_end_value", @params));
    print $OUT $paramtext, "\n";
}

sub do_data_header {
    my ($pr, $OUT, $info, $youts, $variables, $datfile) = @_;
    my $scannedvars;
    my $xvars;
    my $yvars = join " ", @$youts;
    my $xlabel;
    my $min;
    my $max;
    if (@{$info->{VARS}} == 0) { $xlabel = "Point number"; $scannedvars="Point"; $xvars="Point"; $min=1; $max=$numpoints; }
    else { 
      $xlabel = $params[$info->{VARS}[0]]; 
      $scannedvars = join ", ", map($params[$_], @{$info->{VARS}}); 
      $xvars = join " ", map($params[$_], @{$info->{VARS}}); 
      $min = $info->{MIN}[0]; $max = $info->{MAX}[0];
    }
    print $OUT <<END
${pr}type$format_assign ${format_start_value}multiarray_1d($numpoints)$format_end_value
${pr}title$format_assign ${format_start_value}'Scan of $scannedvars'$format_end_value
${pr}xvars$format_assign ${format_start_value}$xvars$format_end_value
${pr}yvars$format_assign ${format_start_value}$yvars$format_end_value
${pr}xlabel$format_assign '$xlabel'
${pr}ylabel$format_assign 'Intensity'
${pr}xlimits$format_assign ${format_start_value}$min $max$format_end_value
${pr}filename$format_assign ${format_start_value}$datfile$format_end_value
${pr}variables$format_assign ${format_start_value}$variables$format_end_value
END
}

# Write <NAME>.sim information file, to be read by mcplot.
sub output_sim_file {
    my ($filename, $info, $youts, $variables, $datfile) = @_;
    my $SIM = new FileHandle;
    my $loc_prefix = "";
    open($SIM, ">$filename") ||
        die "Failed to write info file '$filename'";
    # print $SIM "begin instrument\n";
    do_instr_header($loc_prefix, $SIM);
    # print $SIM "end instrument\n\n";
    if (plotter ~= /McStas|PGPLOT/i) {
      print $SIM "\nbegin simulation\n";
      $loc_prefix = "";
    } elsif (plotter ~= /Matlab/i) {
      print $SIM "\nsim.class='simulation';\n";
      $loc_prefix = "sim.";
    } else {
      print $SIM "\nsim = struct(); sim.class='simulation';\n";
      $loc_prefix = "sim.";
    }
    do_sim_header($loc_prefix, $SIM);
    if (plotter ~= /McStas|PGPLOT/i) {
      print $SIM "end simulation\n\nbegin data\n";
    } elsif (plotter ~= /Matlab/i) {
      print $SIM "\nmcstas.sim = sim; clear sim;\n";
      print $SIM "\ndata.class = 'data;\n";
      $loc_prefix = "data.";
    } else {
      print $SIM "\nmcstas.sim = 0; mcstas.sim = sim; clear sim;\n";
      print $SIM "\ndata = struct(); data.class = 'data;\n";
      $loc_prefix = "data.";
    }
    do_data_header($loc_prefix, $SIM, $info, $youts, $variables, $datfile);
    if (plotter ~= /McStas|PGPLOT/i) {
      print $SIM "end data\n";
    } elsif (plotter ~= /Matlab/i) {
      print $SIM "\nmcstas.sim.data = data; clear data;\n";
    } else {
      print $SIM "\nmcstas.sim.data = 0; mcstas.sim.data = data; clear data;\n";
    }
    close($SIM);
}

# Output header information for mcrun .dat scan file.
sub output_dat_header {
    my ($OUT, $info, $youts, $variables, $datfile) = @_;
    print $OUT "# Instrument-source: '$instr_info->{'Instrument-source'}'\n";
    do_sim_header("# ", $OUT);
    do_data_header("# ", $OUT, $info, $youts, $variables, $datfile);
}

# Do a scan: Run a series of simulations, varying one or more input
# parameters.
sub do_scan {
    my ($info) = @_;
    # Create the output directory if requested.
    my $prefix = "";
    if($data_dir) {
        if(mkdir($data_dir, 0777)) {
            $prefix = "$data_dir/";
        } else {
            die "Error: unable to create directory '$data_dir'.\n(Maybe the directory already exists?)";
        }
    }
    # Use user-specified output file name, with a default of "mcstas.dat".
    my $datfile = ($data_file || "mcstas.dat");
    # Add a default '.dat' extension if no other extension given.
    $datfile .= ".dat" unless $datfile =~ m'\.[^/]*$'; # Quote hack ';
    my $simfile = $datfile;
    $simfile =~ s/\.dat$//;        # Strip any trailing ".dat" extension ...
    $simfile .= $format_ext;        # ... and add ".sim|m|sci" extension.
    my $DAT = new FileHandle;
    open($DAT, ">${prefix}$datfile");
    autoflush $DAT 1;                # Preserves data even if interrupted.
    my $firsttime = 1;
    my $variables = "";
    my @youts = ();
    my $point;
    for($point = 0; $point < $numpoints; $point++) {
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
        my $pid = open(SIM, "-|");
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
        die "Exit due to error returned by simulation program"
            if $got_error || (! $ret && ($? != 0 || $!));
        output_dat_header($DAT, $info, \@youts, $variables, $datfile)
            if $firsttime;
        print $DAT "$out\n";
        $firsttime = 0;
    }
    close($DAT);
    output_sim_file("${prefix}$simfile", $info, \@youts, $variables, $datfile);
    print "Output file: '${prefix}$datfile'\nOutput parameters: $variables\n";
}


                    ##########################
                    # Start of main program. #
                    ##########################

# set default plotter
$plotter = defined($ENV{'MCSTAS_FORMAT'}) ?
                $ENV{'MCSTAS_FORMAT'} : "$MCSTAS::mcstas_config{'PLOTTER'}";

parse_args();                         # Parse command line arguments
# Check value of $plotter variable
if ($plotter =~ /PGPLOT|McStas|0/i) {
  $plotter="McStas"; $format_ext = ".sim";
  $format_assign     =":";    
  $format_start_value=""
  $format_end_value  =""; 
  $format_prefix     ="# ";    
} elsif ($plotter =~ /Matlab|1|2/i) {
  $plotter="Matlab"; $format_ext = ".m";
  $format_assign     ="=";    
  $format_start_value="'"
  $format_end_value  ="'"; 
  $format_prefix     ="";  
} elsif ($plotter =~ /Scilab|3|4/i) {
  $plotter="Scilab"; $format_ext = ".sci";
  $format_assign     ="=";    
  $format_start_value="'"
  $format_end_value  ="'"; 
  $format_prefix     =""; 
}

my $scan_info = check_input_params(); # Get variables to scan, if any
$out_file = get_out_file($sim_def, $force_compile, @ccopts);
exit(1) unless $out_file;
exit(1) if $ncount==0;

# Make sure that the current directory appears first in the path;
# contrary to normal use, this is what the user expects here.
$ENV{PATH} = $ENV{PATH} ? ".:$ENV{PATH}" : ".";

$instr_info = get_sim_info("$out_file --format=$plotter");
if($numpoints == 1) {
    do_single();
} else {
    do_scan($scan_info);
}
