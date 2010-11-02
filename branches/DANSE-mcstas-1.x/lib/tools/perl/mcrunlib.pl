# Library of McStas runtime perl functions
#
#   This file is part of the McStas neutron ray-trace simulation package
#   Copyright (C) 1997-2008, All rights reserved
#   Risoe National Laborartory, Roskilde, Denmark
#   Institut Laue Langevin, Grenoble, France
#
#   This program is free software; you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation; version 2 of the License.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
#
#   You should have received a copy of the GNU General Public License
#   along with this program; if not, write to the Free Software
#   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
#
use Config;
use File::Path;
use File::Basename;
use File::Copy;
use File::stat;
use Cwd;
require "mcstas_config.perl";

# Overload with user's personal config
if ($ENV{"HOME"} && -e $ENV{"HOME"}."/.mcstas/mcstas_config.perl") {
  require $ENV{"HOME"}."/.mcstas/mcstas_config.perl";
}

require "mcfrontlib.pl";

# get MCSTAS::mcstas_config{'PLOTTER'}
my $plotter=$MCSTAS::mcstas_config{'PLOTTER'};

# Strip any single quotes around argument.
sub strip_quote {
    my ($str) = @_;
    $str = $1 if($str =~ /^'(.*)'$/); # Remove quotes if present.
    return $str;
}

# Get a yes/no argument.
sub get_yes_no {
    my ($str) = @_;
    return ($str =~ /yes/i) ? 1 : 0;
}

# Read output from "sim --info" or "begin instrument" section in mcstas.sim
# from file handle.
# Reads lines from handle until the "end instrument" line is encountered,
# skips that line and returns the information read in a hash reference.
# Also terminates upon end-of-file.
sub read_instrument_info {
    my ($h) = @_;
    my $inf = {};
    $inf->{'RAW'} = [];                # List of lines from output of sim.out --info
    while(<$h>) {
        push @{$inf->{'RAW'}}, $_;
        if(/^\s*Name:\s*([a-zA-Z_0-9]+)\s*$/i) {
            $inf->{'Name'} = $1;
        } elsif(/^\s*Parameters:\s*([a-zA-Z_0-9 \t()]*?)\s*$/i) {
            my $full = $1;
            my $parms = [ ];
            my $parmtypes = { };
            my $p;
            for $p (split ' ', $full) {
                if($p =~ /^([a-zA-Z_0-9+]+)\(([a-z]+)\)$/) {
                    push @$parms, $1;
                    $parmtypes->{$1} = $2;
                } elsif($p =~ /^([a-zA-Z_0-9+]+)$/) {
                    # Backward compatibility: no type specifier.
                    push @$parms, $1;
                    $parmtypes->{$1} = 'double'; # Default is double
                } else {
                    die "mcrun: Invalid parameter specification:\n'$p'";
                }
            }
            $inf->{'Parameters'} = $parms;
            $inf->{'Parameter-types'} = $parmtypes;
        } elsif(/^\s*Instrument-source:\s*(.*?)\s*$/i) {
            $inf->{'Instrument-source'} = strip_quote($1);
        } elsif(/^\s*Source:\s*(.*?)\s*$/i) {
            $inf->{'Instrument-source'} = strip_quote($1);
        } elsif(/^\s*Trace-enabled:\s*(no|yes)\s*$/i) {
            $inf->{'Trace-enabled'} = get_yes_no($1);
        } elsif(/^\s*Trace_enabled:\s*(no|yes)\s*$/i) {
            $inf->{'Trace-enabled'} = get_yes_no($1);
        } elsif(/^\s*Default-main:\s*(no|yes)\s*$/i) {
            $inf->{'Default-main'} = get_yes_no($1);
        } elsif(/^\s*Default_main:\s*(no|yes)\s*$/i) {
            $inf->{'Default-main'} = get_yes_no($1);
        } elsif(/^\s*Embedded-runtime:\s*(no|yes)\s*$/i) {
            $inf->{'Embedded-runtime'} = get_yes_no($1);
        } elsif(/^\s*Embedded_runtime:\s*(no|yes)\s*$/i) {
            $inf->{'Embedded-runtime'} = get_yes_no($1);
        } elsif(/^\s*end\s+instrument\s*$/i) {
            last;
        } else {
            # print "\# $_";
        }
    }
    return $inf;
}

sub get_sim_info {
    my ($simprog) = @_;
    # Needs quoting if this is Win32...
    my $cmdstring="$simprog -i";
    if ($Config{'osname'} eq 'MSWin32') {
      $cmdstring="\"$cmdstring\" ";
    }
    use FileHandle;
    my $h = new FileHandle;
    open $h, "$cmdstring |" or die "mcrun: Could not run simulation.";
    my $inf = read_instrument_info($h);
    my $sinf= read_simulation_info($h);
    close $h;
    $inf->{'Params'} = $sinf->{'Params'};
    return $inf;
}


# Supporting function for get_out_file() below, suitable for use in a
# call-back style GUI application.
#
# Returns two results. The first is a data structure to pass to
# subsequence get_out_file_next() calls; if undefined, the second
# results is an error message.
#
sub get_out_file_init {
    my ($inname, $force, $mpi, $threads, @ccopts) = @_;
    return (undef, "mcrun: No simulation filename given") unless $inname;
    # Add a default extension of ".instr" if given name does not exist
    # as file.
    my $sim_def = $inname;
    $sim_def .= ".instr" if(! (-e $sim_def) && (-e "$sim_def.instr"));
    return(undef, "mcrun: Simulation '$sim_def' not found") unless -e $sim_def;
    my $file_type = MCSTAS;
    my $base_name = $sim_def;
    # Different executable suffixes on Win32 vs. unix
    # PW 20030314
    my $ext;
    $ext=$MCSTAS::mcstas_config{'EXE'};

    if($sim_def =~ /(.*)\.instr$/) {
        $base_name = $1;
    } elsif($sim_def =~ /(.*)\.c$/) {
        $base_name = $1;
        $file_type = C;
    } elsif($sim_def =~ /(.*)\.$ext$/) {
        $base_name = $1;
        $file_type = OUT;
    }
    my $dir;
    if($base_name =~ m'^(.*)/[^/]*$') { # quote hack -> ') {
        $dir = $1;
    }
    my $c_name = "$base_name.c";
    my $out_name = "$base_name.$ext";
    $sim_def = "$base_name.instr" unless $file_type eq MCSTAS;
    my $v = { };
    $v->{'force'} = $force;
    $v->{'mpi'} = $mpi;
    $v->{'threads'} = $threads;
    $v->{'ccopts'} = join(" ",@ccopts);
    $v->{'file_type'} = $file_type;
    $v->{'dir'} = $dir;
    $v->{'sim_def'} = $sim_def;
    $v->{'c_name'} = $c_name;
    $v->{'out_name'} = $out_name;
    $v->{'sim_age'} = -e $sim_def ? -M $sim_def : undef;
    $v->{'c_age'} = -e $c_name ? -M $c_name : undef;
    $v->{'out_age'} = -e $out_name ? -M $out_name : undef;
    $v->{'stage'} = PRE_MCSTAS;
    $v->{'cc_cmd'} = "";
    return ($v, "");
}

# Supporting function for get_out_file() below, suitable for use in a
# call-back style GUI application.
#
# Takes two args. The first is the data structure returned by
# get_out_file_init(). The second is a function to call to print
# messages.
#
# Returns two results. The first is a status value, describing the
# meaning of the second result value:
#    status    value
#    CONTINUE  undefined   Ok, call again for next stage
#    RUN_CMD   command     Ok, run the command and call again for next stage
#    ERROR     message     An error occured, stop
#    FINISHED  out_name    Compilation has finished, here is .exe name
#
# In RUN_CMD, the "command" is a ref to a list for execvl(). The other
# values are strings. This function should be called repeatedly until
# either ERROR or FINISHED is returned.
#
sub get_out_file_next {
  my ($v, $printer, $mcrunflag) = @_;
  # The mcrunflag allows mcgui to request a compilation
  # using mcrun -c (Win32 specific).
  my ($cmd, $exit_val);
  my $force = $v->{'force'};
  my $file_type = $v->{'file_type'};
  my $sim_def = $v->{'sim_def'};
  my $c_name = $v->{'c_name'};
  my $out_name = $v->{'out_name'};
  my $sim_age = $v->{'sim_age'};
  my $c_age = $v->{'c_age'};
  my $out_age = $v->{'out_age'};
  my $stage = $v->{'stage'};
  my $mpi   = $v->{'mpi'};
  my $threads   = $v->{'threads'};
  my $ccopts = $v->{'ccopts'};
  my $cccmd  = $v->{'cc_cmd'};
  if($stage eq PRE_MCSTAS) {
    # Translate simulation definition into C if newer than existing C
    # version.
    if($file_type eq C && (defined($sim_age) && $sim_age < $c_age)) {
        &$printer("Warning: simulation definition '$sim_def'" .
                  " is newer than '$c_name'");
    }
    if($file_type eq OUT && (defined($sim_age) && $sim_age < $out_age)) {
        &$printer("Warning: simulation definition '$sim_def'" .
                  " is newer than '$out_name'");
    }
    if($file_type eq OUT && (defined($c_age) && $c_age < $out_age)) {
        &$printer("Warning: C source '$c_name'" .
                  " is newer than '$out_name'");
    }
    if($file_type eq MCSTAS &&
       ($force || !defined($c_age) || $c_age > $sim_age)) {
        &$printer("Translating instrument definition '$sim_def'" .
                  " into C ...");
      # On Win32, quote the filenames if containing spaces...
      my $dir=$v->{'dir'};
      if (($Config{'osname'} eq 'MSWin32') && (($c_name =~ /\ /) || ($dir =~ /\ /))) {
        $c_name="\"$c_name\"";
        $sim_def="\"$sim_def\"";
        if (defined($dir)) { $dir="\"$dir\""; }
      }
      my @inc = $v->{'dir'} ? ("-I", $dir) : ();
      if (defined($mcrunflag) && $mcrunflag eq 1) {
         my $cmd = ["mcrun$MCSTAS::mcstas_config{'SUFFIX'} -c -n0 ", $mpi == 1 ? "--mpi" : (), $sim_def];
      &$printer(join(" ", @$cmd));
	$v->{'stage'} = POST_MCSTAS;
      return (RUN_CMD, $cmd);
      } else {
      my $cmd = ["mcstas", @inc, "-t", "-o", $c_name, $sim_def];
      &$printer(join(" ", @$cmd));
      $v->{'stage'} = POST_MCSTAS;
      return (RUN_CMD, $cmd);
      }
    } else {
      $v->{'stage'} = PRE_CC;
      return (CONTINUE, undef);
    }
  } elsif($stage eq POST_MCSTAS) {
    $v->{'c_age'} = -M $c_name;
    $v->{'out_age'} = undef; # Force recompilation.
    $v->{'stage'} = PRE_CC;
    return (CONTINUE, undef);
  } elsif($stage eq PRE_CC) {
    unless(-e $c_name) {
        return (ERROR, "Could not translate simulation '$sim_def' into C");
    }
    # Compile C source if newer than existing out file.
    # ToDo: splitting CFLAGS should handle shell quoting as well ...
    my $cc     = $MCSTAS::mcstas_config{CC};
    my $cflags = $MCSTAS::mcstas_config{CFLAGS};
    my $libs = "-lm ";
    if ($v->{'threads'} && $MCSTAS::mcstas_config{THREADS} ne "") {
      $libs .= $MCSTAS::mcstas_config{THREADS};
    }
    if ($v->{'mpi'} && $MCSTAS::mcstas_config{MPICC} ne "no") {
      $libs .= " -DUSE_MPI ";
      $cc      = $MCSTAS::mcstas_config{'MPICC'};
    }
    if ($MCSTAS::mcstas_config{'PLOTTER'} =~ /NeXus|HDF/i && $MCSTAS::mcstas_config{'NEXUS'} ne "") {
      $libs .= $MCSTAS::mcstas_config{'NEXUS'};
    }
    # Needs quoting on MSWin32 if containing spaces...
    if (($Config{'osname'} eq 'MSWin32') && ($out_name =~ /\ /)) {
      $out_name="\"$out_name\"";
      $c_name="\"$c_name\"";
    }
    if ($ccopts) { $libs .= $ccopts; }
    my $cmd = [$cc, split(' ', $cflags), "-o",
               $out_name, $c_name, split(' ', $libs)];
    $v->{'cc_cmd'} = join(" ", @$cmd);           
    if(($file_type eq MCSTAS || $file_type eq C) &&
       ($force || !defined($out_age) || $out_age > $c_age)) {
      &$printer("Compiling C source '$c_name' ...");
      &$printer(join(" ", @$cmd));
      $v->{'stage'} = POST_CC;
      return (RUN_CMD, $cmd);
    } else {
      $v->{'stage'} = FINISHED;
      return (FINISHED, $out_name);
    }
  } elsif($stage eq POST_CC) {
    unless(-e $out_name) {
        return (ERROR, "Could not compile C source file '$c_name'");
    }
    $v->{'stage'} = FINISHED;
    return (FINISHED, $out_name);
  } else {
    die "mcrun: Internal: get_out_file_next: $stage";
  }
}

#
# Get the name of the executable file for the simulation, translating
# and compiling the instrument definition if necessary.
#
# The optional $force option, if true, forces unconditional recompilation.
#
sub get_out_file {
    my ($inname, $force, $mpi, $threads, @ccopts) = @_;
    my ($v, $msg, $status, $value);
    ($v, $msg) = get_out_file_init($inname, $force, $mpi, $threads, @ccopts);
    unless($v) {
        print STDERR "$msg\n";
        return undef;
    }
    for(;;) {
        ($status, $value) = get_out_file_next($v, sub { print "$_[0]\n"; });
        if($status eq FINISHED) {
            return ($value,$v);
        } elsif($status eq RUN_CMD) {
            my $exit_val = system(@$value);
            if($exit_val) {
                print STDERR "** Error exit **\n";
                return (undef,$v);
            }
            next;
        } elsif($status eq ERROR) {
            print STDERR "$value\n";
            return (undef,$v);
        } elsif(!($status eq CONTINUE)) {
            die "mcrun: Internal: get_out_file";
        }
    }
}

# McStas selftest procedure: copy LIB/examples and execute
sub do_test {
  my ($printer,$force, $plotter, $exec_test, $mpi, $ncount) = @_;
  my $j;
  my $pwd=getcwd;

  &$printer( "# McStas self-test (mcrun --test='$exec_test')");
  if ($mpi) {
      &$printer("# MPI enabled, spawning $mpi computenodes");
  }
  &$printer(`mcstas --version`);
  &$printer("# Installing 'selftest' directory in $pwd");
  if (-d "selftest") # directory already exists
  { if ($force) { eval { rmtree('selftest',0,1); } }
    else {
      return "mcrun: Directory 'selftest' already exists.\n       Use '-c' or '--force' option to overwrite.\n";
    }
  }
  # create selftest direcory
  eval { mkpath('selftest') };
  if ($@) {
    return "mcrun: Couldn't create 'selftest': $@\n";
  }
  # copy all instruments
  &$printer("# Copying instruments from $MCSTAS::sys_dir/examples/");
  if (opendir(DIR,"$MCSTAS::sys_dir/examples/")) {
    my @instruments = readdir(DIR);
    closedir(DIR);
    next unless @instruments;
    my @paths = map("$MCSTAS::sys_dir/examples/$_", grep(/\.(instr)$/, @instruments));
    for ($j=0 ; $j<@paths; $j++) {
      my ($base, $dirname, $suffix) = fileparse($paths[$j],".instr");
      if (! copy("$paths[$j]","./selftest/$base$suffix")) {
        return "Could not copy $paths[$j] to 'selftest' directory: $!\n";
      }
    }
  } else { return "mcrun: no test instruments found. Aborting.\n"; }
  # go into the selftest directory
  chdir("selftest") or return "mcrun: Can get into selftest: $!\n";
  # Initialize test
  my $n_single=int($ncount/10);
  my $n_scan=int($ncount/100);
  my $now = localtime;
  my $start_sec = time();
  &$printer("# Counts: single=$n_single, scans=$n_scan");
  &$printer("# Output format: $plotter");
  &$printer("# Start Date: $now");
  my @test_names;
  my @test_commands;
  my @test_monitor_names;
  my @test_monitor_values;
  my $suffix='';
  $suffix=$MCSTAS::mcstas_config{'SUFFIX'};
  $prefix=$MCSTAS::mcstas_config{'PREFIX'};
  $ENV{'MCSTAS_FORMAT'} = $plotter;
  if ($mpi) {
      $mpi="--mpi=$mpi";
  }

  # compatible test definition
  if ($exec_test =~ /compatible/i) {
  @test_names   = ("ISIS prisma2: in focusing mode",
      "ISIS prisma2: in non-focusing mode",
      "vanadium_example: vanadium scattering anisotropy",
      "Brookhaven H8: Thermal TAS with vanadium sample",
      "Risoe TAS1: monochromator rocking curve (no collimator)",
      "Risoe TAS1: monochromator rocking curve (with 30 minutes collimator)",
      "Risoe TAS1: collimator C1 tilt",
      "Risoe TAS1: monochromator rotation (PHM aka A1) with C1 tilted by OMC1=-2 deg",
      "Risoe TAS1: monochromator rotation (PHM aka A1) with C1 tilted by OMC1=-5 deg",
      "Risoe TAS1: monochromator rotation (PHM aka A1) with C1 tilted by OMC1=-6 deg",
      "Risoe TAS1: monochromator rotation (PHM aka A1) with C1 tilted by OMC1=-10 deg",
      "Risoe TAS1: sample take-off angle (TT aka A4) scan with 3 collimators. Slit sample",
      "Risoe TAS1: analyzer arm alignment (TTA aka A6). No detector collimation C3. Vanadium sample",
      "Risoe TAS1: analyzer arm alignment (TTA aka A6). 30 minutes detector collimation C3. Vanadium sample",
      "Risoe TAS1: sample two-theta (TT aka A4). positive side . Powder sample",
      "Risoe TAS1: sample two-theta (TT aka A4). negative side . Powder sample",
      "Risoe TAS1: Triple axis mode. Analyzer rocking curve (OMA aka A5). Vanadium sample",
      "Risoe TAS1: Triple axis mode. Sample take-off (TT aka A4). Powder sample",);
  @test_commands= ("mcrun$suffix $mpi  --dir=prisma2a prisma2.instr --ncount=$n_single TT=-30 PHA=22 PHA1=-3 PHA2=-2 PHA3=-1 PHA4=0 PHA5=1 PHA6=2 PHA7=3 TTA=44",
      "mcrun$suffix $mpi  --dir=prisma2b prisma2.instr --ncount=$n_single TT=-30 PHA=22 PHA1=3 PHA2=2 PHA3=1 PHA4=0 PHA5=-1 PHA6=-2 PHA7=-3 TTA=44",
      "mcrun$suffix $mpi  --dir=V_test vanadium_example.instr --ncount=$n_single ROT=0",
      "mcrun$suffix $mpi  -n $n_single --dir=h8_test  h8_test.instr Lambda=2.359",
      "mcrun$suffix $mpi  --numpoints=41 -n $n_scan --dir=linup_1_45 linup-1.instr PHM=-39,-35 TTM=-74 C1=0",
      "mcrun$suffix $mpi  --numpoints=41 -n $n_scan --dir=linup_2_45 linup-1.instr PHM=-39,-35 TTM=-74 C1=30",
      "mcrun$suffix $mpi  --numpoints=41 -n $n_scan --dir=linup_3_45 linup-2.instr PHM=-37.077 TTM=-74 C1=30 OMC1=-50,50",
      "mcrun$suffix $mpi  --numpoints=41 -n $n_scan --dir=linup_4_45 linup-2.instr PHM=-39,-35 TTM=-74 C1=30 OMC1=-1.81715",
      "mcrun$suffix $mpi  --numpoints=31 -n $n_scan --dir=linup_5_m5 linup-2.instr PHM=-38.5,-35.5 TTM=-74 C1=30 OMC1=-5",
      "mcrun$suffix $mpi  --numpoints=31 -n $n_scan --dir=linup_5_m6 linup-2.instr PHM=-38.5,-35.5 TTM=-74 C1=30 OMC1=-6",
      "mcrun$suffix $mpi  --numpoints=31 -n $n_scan --dir=linup_5_m10 linup-2.instr PHM=-38.5,-35.5 TTM=-74 C1=30 OMC1=-10",
      "mcrun$suffix $mpi  --numpoints=41 -n $n_scan --dir=linup_6_0 linup-3.instr PHM=-37.077 TTM=-74 TT=-1.5,1.5 C1=30 OMC1=-5.5 C2=0 C3=0",
      "mcrun$suffix $mpi  --numpoints=41 -n $n_scan --dir=linup_7 linup-4.instr PHM=-37.077 TTM=-74 TT=33.52 TTA=-3,3 C1=30 OMC1=-5.5 C2=28 C3=0",
      "mcrun$suffix $mpi  --numpoints=41 -n $n_scan --dir=linup_8 linup-4.instr PHM=-37.077 TTM=-74 TT=33.52 TTA=-3,3 C1=30 OMC1=-5.5 C2=28 C3=67",
      "mcrun$suffix $mpi  --numpoints=41 -n $n_scan --dir=linup_9 linup-5.instr PHM=-37.077 TTM=-74 TT=32,35 TTA=0 C1=30 OMC1=-5.5 C2=28 C3=67",
      "mcrun$suffix $mpi  --numpoints=41 -n $n_scan --dir=linup_10 linup-5.instr PHM=-37.077 TTM=-74 TT=-32,-35 TTA=0 C1=30 OMC1=-5.5 C2=28 C3=67",
      "mcrun$suffix $mpi  --numpoints=21 -n $n_scan --dir=linup_11 linup-6.instr PHM=-37.077 TTM=-74 TT=33.57 OMA=-16.44,-18.44 TTA=-34.883 C1=30 OMC1=-5.5 C2=28 C3=67",
      "mcrun$suffix $mpi  --numpoints=21 -n $n_scan --dir=linup_13 linup-7.instr PHM=-37.077 TTM=-74 TT=32.5,34.5 OMA=-17.45 TTA=-34.9 C1=30 OMC1=-5.5 C2=28 C3=67");
  @test_monitor_names =("mon9_I","mon9_I","PSD_4pi_I","D7_SC3_In_I","","","","","","","","","","","","","","");
  @test_monitor_values=(6.4e-13,4.5e-13,2.1e-06,3.11e-11,0,0,0,0,0,0,0,0,0,0,0,0,0,0);
  } # end of compatible test definition
  # now execute each simulation and look for errors
  my $error_flag    = 0;
  my $accuracy_flag = 0;
  my $plot_flag     = 0;
  my $total_diff    = 0;
  for ($j=0 ; $j<@test_commands ; $j++) {
    my $this_cmd =$test_commands[$j];
    my $this_name=$test_names[$j];
    &$printer("Executing: $this_cmd");
    my $res = qx/$this_cmd/;
    my $child_error_text = $!;
    my $child_error_code = $?;
    if ($child_error_code) {
      &$printer("[FAILED] $this_name: ($child_error_code): $child_error_text");
      $error_flag = 1;
      last;
    } else {
      my $diff = 0;
      my $sim_I= 0;
      my $line;
      #Analyse test output if reference value is available
      if ($test_monitor_values[$j] ne 0) { # there is a reference value...
        # split the output in lines
        for $line (split "\n", $res) {
        # search reference monitor in these lines
          if($line =~ m/Detector: ([^ =]+_I) *= *([^ =]+) ([^ =]+_ERR) *= *([^ =]+) ([^ =]+_N) *= *([^ =]+) *(?:"[^"]+" *)?$/) {
            my $sim_I_name = $1;
            if ($test_monitor_names[$j] eq $sim_I_name) {
              $sim_I = $2;
              $diff = abs($sim_I/$test_monitor_values[$j] -1);
              $total_diff = $total_diff+$diff;
            }
          }
        } # end for
        if ($diff) {
          if ($diff > 0.2) {
            $accuracy_flag = 1;
            $diff = $diff*100;
            &$printer("[FAILED] $this_name ($test_monitor_names[$j] = $sim_I, should be $test_monitor_values[$j])");
          } else { &$printer("[OK] $this_name ($test_monitor_names[$j] accuracy within $diff %)"); }
        }
      } # end if ($test_monitor_values[$j] ne 0)
      if ($diff eq 0) { &$printer("[OK] $this_name (accuracy not checked)"); }
    } # end else
  } # end for
  my $elapsed_sec = time() - $start_sec;

  # now test graphics...
  if ($exec_test =~ /graphics/i) {
    @test_names   = ("Plot of Scan of parameters with Risoe TAS1 monochromator rocking curve (no collimator)",
      "Plot of Single simulation with Brookhaven H8 Termal TAS with vanadium sample");
    @test_commands= ("mcplot$suffix -psc linup_1_45",
      "mcplot$suffix -psc h8_test");
    @test_monitor_names =("linup_1_45","h8_test");
    for ($j=0 ; $j<@test_commands ; $j++) {
      my $this_cmd =$test_commands[$j];
      my $this_name=$test_names[$j];
      &$printer("Executing: $this_cmd");
      my $res = qx/$this_cmd/;
      my $child_error_text = $!;
      my $child_error_code = $?;
      if ($child_error_code) {
        &$printer("[Warning] $this_name: ($child_error_code): $child_error_text");
      }
      my $this_flag = 1;
      if (opendir(DIR, "$test_monitor_names[$j]")) {
        my @files = readdir(DIR);
        closedir(DIR);
        my $k;
        my $filename;
        my @paths = map("$test_monitor_names[$j]/$_", grep(/\.(gif|png|ps|eps|jpg)$/i, @files));
        for ($k=0 ; $k<@paths; $k++) {
          $filename = $paths[$k];
          $this_flag = 1;
          if (-f "$filename") {
            my $sb = stat($filename);
            if ($sb->size) {
              &$printer("[OK] $this_name ($filename)");
              $this_flag = 0;
            } else {
              &$printer("[Warning] $this_name ($filename) exists but is empty");
              $this_flag = 0;
            }
          } # end if (-f "$filename")
        } # end for
      } # end opendir
      if ($this_flag) { &$printer("[FAILED] $this_name"); $plot_flag=1; }
    } # end for
  } # end of graphics test

  $now = localtime();
  if ($error_flag) {
    &$printer("# Installation check: FAILED. McStas has not been properly installed.");
    &$printer("# >> Check that you have a C compiler, perl, and perl-Tk installed.");
  } else {
    &$printer("# Installation check: OK.     Computing time: $elapsed_sec [sec].");
    if ($accuracy_flag) {
      &$printer("# Accuracy     check: FAILED. Results are not reliable.");
      &$printer("# >> This McStas installation does NOT produce accurate results.");
    } else {
      &$printer("# Accuracy     check: OK.");
    }
    if ($exec_test =~ /graphics/i) {
      if ($plot_flag) {
        &$printer("# Plotter      check: FAILED.");
        &$printer("# >> The $plotter plotter may NOT be working properly.");
        &$printer("# >> Check that you have Scilab/Matlab/PGPLOT installed");
        &$printer("# >>    and that your Display is available.");
      } else {
        &$printer("# Plotter      check: OK.     Using Plotter $plotter.");
      }
    } else {
      &$printer("# Plotter NOT  checked ($plotter).");
    }
  }

  &$printer("# End Date: $now");
  chdir($pwd) or return "mcrun: Can not come back to $pwd: $!\n";; # come back to initial directory
  return undef;
}

# return the component name given the file name for the definition.
sub compname {
    my ($path) = @_;
    my $name = $path;
    my $i;
    if($i = rindex($name, "/")) {
        $name = substr($name, $i + 1);
    }
    if($name =~ /^(.+)\.(comp|cmp|com)$/) {
        $name = $1;
    }
    return $name;
}

# Parse comment header in McDoc format in component definition.
# Return a data structure containing the gathered information.
sub parse_header {
    my ($f) = @_;
    my $d;
    my ($i,$where, $thisparm);

    $where = "";
    $d->{'identification'} = { 'author' => "(Unknown)",
                               'origin' => "(Unknown)",
                               'date' => "(Unknown)",
                               'version' => "(Unknown)",
                               'history' => [ ],
                               'short'  => ""
                           };
    $d->{'description'} = undef;
    $d->{'parhelp'} = { };
    $d->{'links'} = [ ];
    $d->{'site'}="";
    while(<$f>) {
        if(/\%INSTRUMENT_SITE:(.*)$/i) {
            $d->{'site'}=$1;
        } elsif(/\%I[a-z]*/i && not /\%include/i) {
            $where = "identification";
        } elsif(/\%D[a-z]*/i) {
            $where = "description";
        } elsif(/\%P[a-z]*/i) {
            $where = "parameters";
            undef $thisparm;
        } elsif(/\%L[a-z]*/i) {
            $where = "links";
            push @{$d->{'links'}}, "";
        } elsif(/\%E[a-z]*/i) {
            last;
        } else {
            s/^[ ]?\*[ ]?//;
            if($where eq "identification") {
                if(/(Written by|Author):(.*)$/i) {
                    $d->{'identification'}{'author'} = $2;
                }elsif(/Origin:(.*)$/i) {
                    $d->{'identification'}{'origin'} = $1;
                }elsif(/Date:(.*)$/i) {
                    $d->{'identification'}{'date'} = $1;
                }elsif(/Release:(.*)$/i) {
                    $d->{'identification'}{'release'} = $1;
                }elsif(/Version:(.*)$/i) {
                    my $verstring = $1;
                    # Special case for RCS style $[R]evision: 1.2 $ tags.
                    # Note the need for [R] to avoid RCS keyword expansion
                    # in the mcdoc source code!
                    if($verstring =~ /^(.*)\$[R]evision: (.*)\$(.*)$/) {
                        $d->{'identification'}{'version'} = "$1$2$3";
                    } else {
                        $d->{'identification'}{'version'} = $verstring;
                    }
                }elsif(/Modified by:(.*)$/i) {
                    push @{$d->{'identification'}{'history'}}, $1;
                } else {
                    $d->{'identification'}{'short'} .= $_
                        unless /^\s*$/;
                }
            } elsif($where eq "description") {
                $d->{'description'} .= $_;
            } elsif($where eq "parameters") {
                if(/^[ \t]*([a-zA-Z0-9_]+)\s*:(.*)/) {
                    $thisparm = \$d->{'parhelp'}{$1}{'full'};
                    $$thisparm = "$2\n";
                } elsif(/^[ \t]*$/) { # Empty line
                    undef $thisparm;
                } elsif($thisparm && /^(  | *\t)[ \t]*(.*)/) {
                    # Continuation line needs at least two additional
                    # indentations
                    $$thisparm .= "$2\n";
                } elsif(/^[ \t]*([a-zA-Z0-9_]+)\s*(.*)/) {
                    $thisparm = \$d->{'parhelp'}{$1}{'full'};
                    $$thisparm = "$2\n";
                } else {
                    # Skip it
                }
            } elsif($where eq "links") {
                $d->{'links'}[-1] .= $_;
            } else {
                # Skip.
            }
        }
    }
    # Now search for unit specifications in the parameter information.
    # This is a bit tricky due to various formats used in the old
    # components. The preferred format is a specification of the unit
    # in square brackets "[..]", either first or last in the short
    # description. Specification using parenthesis "(..)" is also
    # supported for backwards compatibility only, but only one set of
    # nested parenthesis is supported.
    for $i (keys %{$d->{'parhelp'}}) {
        my $s = $d->{'parhelp'}{$i}{'full'};
        my ($unit, $text);
        if($s =~ /^\s*\(([^()\n]*(\([^()\n]*\))?[^()\n]*)\)\s*((.|\n)*)\s*$/){
            $unit = $1;
            $text = $3;
        } elsif($s =~ /^\s*((.|\n)*)\s*\(([^()\n]*(\([^()\n]*\))?[^()\n]*)\)\s*$/){
            $unit = $3;
            $text = $1;
        } elsif($s =~ /^\s*\[([^][\n]*)\]\s*((.|\n)*)\s*$/){
            $unit = $1;
            $text = $2;
        } elsif($s =~ /^\s*((.|\n)*)\s*\[([^][\n]*)\]\s*$/){
            $unit = $3;
            $text = $1;
        } else {
            # No unit. Just strip leading and trailing white space.
            $unit = "-";
            if($s =~ /^\s*((.|\n)*\S)\s*$/) {
                $text = $1;
            } else {
                $s =~ /^\s*$/ || die "mcrun: Internal: parse_header match 1";
                $text = "$s";
            }
        }
        $d->{'parhelp'}{$i}{'unit'} = $unit;
        $d->{'parhelp'}{$i}{'text'} = $text;
    }
    return $d;
}

# This sub gets component information by parsing the McStas
# metalanguage. For now this is a regexp hack, later the real mcstas
# parser will be used.
sub get_comp_info {
    my ($name, $d) = @_;
    my $file = new FileHandle;
    my ($cname, $decl, $init, $trace, $finally, $disp, $typ);
    my (@dpar, @spar, @ipar, @opar);
    open($file, $name)  || die "mcrun: Could not open file $name\n";
    local $/ = undef;                # Read the whole file in one go.
    my $s = <$file>;
    close($file);
    $typ = "Component";
    @opar = (); @dpar = (); @spar = ();
    if ($s =~ m!DEFINE\s+INSTRUMENT\s+([a-zA-Z0-9_]+)\s*\(([-+.a-zA-Z0-9_ \t\n\r=,/*{}\"]+)\)!i) {
        $cname = $1;
        $typ   = "Instrument";
        foreach (split(",", $2)) {
            if(/^\s*([a-zA-Z0-9_ \s\*]+)\s*\=\s*(.*)\s*$/)  {  # [type] name=value
                my $p = $1;
                my @p_splitted = split(" ", $p);
                my $length = scalar @p_splitted;
                my $p_last_word = $p_splitted[$length-1];
                push @spar, $p_last_word;
                $d->{'parhelp'}{$p_last_word}{'default'} = $2;
            } elsif(/^\s*([a-zA-Z0-9_ \s\*]+)\s*$/) {                # [type] name
                my $p = $1;
                my @p_splitted = split(" ", $p);
                my $length = scalar @p_splitted;
                my $p_last_word = $p_splitted[$length-1];
                push @spar, $p_last_word;
            } else {
                print STDERR "Warning: Possible syntax error in specification of PARAMETER in instrument $cname: $1 .\n";
            }
        }
        if ($s =~ /DEFINE\s+COMPONENT\s+([a-zA-Z0-9_]+)/i)
        { push @opar, "$1"; $d->{'parhelp'}{$1}{'default'} = "This instrument contains embedded components"; }
    } elsif ($s =~ /DEFINE\s+COMPONENT\s+([a-zA-Z0-9_]+)/i) {
        $cname = $1;
        if($s =~ m!DEFINITION\s+PARAMETERS\s*\(([-+.a-zA-Z0-9_ \t\n\r=,/*{}\"]+)\)!i && $typ ne "Instrument") {
            foreach (split(",", $1)) {
                if(/^\s*([a-zA-Z0-9_ \s\*]+)\s*\=\s*(.*)\s*$/) { # [type] name=define
                    my $p = $1;
                    my @p_splitted = split(" ", $p);
                    my $length = scalar @p_splitted;
                    my $p_last_word = $p_splitted[$length-1];
                    my $p_first_word= $p_splitted[0];
                    push @dpar, $p_last_word;
                    $d->{'parhelp'}{$p_last_word}{'default'} = $2;
                    if ($length > 1) {
                      $d->{'parhelp'}{$p_last_word}{'type'} = $p_first_word;
                    }
                } elsif(/^\s*([a-zA-Z0-9_ \s\*]+)\s*$/) {                # [type] name
                    my $p = $1;
                    my @p_splitted = split(" ", $p);
                    my $length = scalar @p_splitted;
                    my $p_last_word = $p_splitted[$length-1];
                    my $p_first_word= $p_splitted[0];
                    push @dpar, $p_last_word;
                    if ($length > 1) {
                      $d->{'parhelp'}{$p_last_word}{'type'} = $p_first_word;
                    }
                } else {
                    print STDERR "Warning: Possible syntax error in specification of DEFINITION PARAMETER in component $cname: $1 .\n";
                }
            }
        }
        if($s =~ m!SETTING\s+PARAMETERS\s*\(([-+.a-zA-Z0-9_ \t\n\r=,/*]+)\)!i && $typ ne "Instrument") {
            foreach (split(",", $1)) {
                if(/^\s*([a-zA-Z0-9_ \s\*]+)\s*\=\s*([-+.e0-9]+)\s*$/) { # [type] name=numerical value
                    my $p = $1;
                    my @p_splitted = split(" ", $p);
                    my $length = scalar @p_splitted;
                    my $p_last_word = $p_splitted[$length-1];
                    push @spar, $p_last_word;
                    my $p_first_word = $p_splitted[0];
                    $d->{'parhelp'}{$p_last_word}{'default'} = $2;
                    if ($length > 1) {
                      $d->{'parhelp'}{$p_last_word}{'type'} = $p_first_word;
                    }
                } elsif(/^\s*([a-zA-Z0-9_ \s\*]+)\s*\=\s*(.*)\s*$/) { # [type] name=other value
                    my $p = $1;
                    my @p_splitted = split(" ", $p);
                    my $length = scalar @p_splitted;
                    my $p_last_word = $p_splitted[$length-1];
                    push @spar, $p_last_word;
                    my $p_first_word = $p_splitted[0];
                    if ($length > 1 && $p_first_word !~ m/char/ && $p_first_word !~ m/string/) {
                      print STDERR "
Warning: SETTING parameter $1 with default value $2\n
         is not of type char/string. Ignoring default value.\n";
                    } else {
                      $d->{'parhelp'}{$p_last_word}{'default'} = $2;
                    }
                    if ($length > 1) {
                      $d->{'parhelp'}{$p_last_word}{'type'} = $p_first_word;
                    }
                } elsif(/^\s*([a-zA-Z0-9_]+)\s*$/) {                    # [type] name
                    my $p = $1;
                    my @p_splitted = split(" ", $p);
                    my $length = scalar @p_splitted;
                    my $p_last_word = $p_splitted[$length-1];
                    my $p_first_word = $p_splitted[0];
                    push @spar, $p_last_word;
                    if ($length > 1) {
                      $d->{'parhelp'}{$p_last_word}{'type'} = $p_first_word;
                    }
                } else {
                    print STDERR "Warning: Possible syntax error in specification of SETTING PARAMETER in component $cname: $1 .\n";
                }
            }
        }
        if($s =~ /OUTPUT\s+PARAMETERS\s*\(([a-zA-Z0-9_, \t\r\n]+)\)/i && $typ ne "Instrument") {
            @opar = split (/\s*,\s*/, $1);
        }
    } else {
        $cname = "<Unknown>";
    }

    @ipar = (@dpar, @spar);

    # DECLARE, INITIALIZE, ... blocks will have to wait for the real parser.
    $d->{'name'} = $cname;
    $d->{'type'} = $typ;
    if ($typ eq "Component") { $d->{'ext'} = "comp"; }
    else { $d->{'ext'} = "instr"; }
    $d->{'inputpar'} = \@ipar;
    $d->{'definitionpar'} = \@dpar;
    $d->{'settingpar'} = \@spar;
    $d->{'outputpar'} = \@opar;
}


# Return component information given filename.
sub component_information {
    my ($comp) = @_;
    my $file = new FileHandle;
    open($file, $comp)  || return undef;
    my $data = parse_header($file);
    close($file);
    return undef unless defined($data);
    get_comp_info($comp, $data);
    return $data;
}

# Open .instr file for component information
# Needed for implementation of 'inspect' feature
# in mcgui.pl
# PW 20030314
sub instrument_information {
    my ($instr) = @_;
    my $file = new FileHandle;
    open($file, $instr) || return undef;
    my @data = parse_instrument($file);
    close($file);
    return @data;
}

# Parse .instr file
sub parse_instrument {
    my ($f) = @_;
    my @d;
    my ($i,$where, $thisparm);
    while(<$f>) {
        if(/^\s*COMPONENT \s*([a-zA-Z0-9_]+)\s=*/) {
      push @d, $1;
        } else  {
  }
    }
    return @d;

}

1;



