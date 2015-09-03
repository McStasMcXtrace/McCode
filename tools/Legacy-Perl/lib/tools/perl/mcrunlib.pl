# Library of McStas/McXtrace runtime perl functions
#
#   This file is part of the McStas/McXtrace neutron/xray ray-trace simulation package
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
require "mccode_config.perl";

# Overload with user's personal config
if ($ENV{"HOME"} && -e $ENV{"HOME"}."/.".$MCSTAS::mcstas_config{'MCCODE'}."/mccode_config.perl") {
  print "$0: reading local $MCSTAS::mcstas_config{'MCCODE'} configuration from " . $ENV{"HOME"}."/.".$MCSTAS::mcstas_config{'MCCODE'}."/mccode_config.perl\n";
  require $ENV{"HOME"}."/.".$MCSTAS::mcstas_config{'MCCODE'}."/mccode_config.perl";
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
                    die "$MCSTAS::mcstas_config{'RUNCMD'}: Invalid parameter specification:\n'$p'";
                }
            }
            $inf->{'Parameters'} = $parms;
            $inf->{'Parameter-types'} = $parmtypes;
        } elsif(/^\s*Instrument-source:\s*(.*?)\s*$/i) {
            $inf->{'Instrument-source'} = strip_quote($1);
        } elsif(/^\s*Instrument_source:\s*(.*?)\s*$/i) {
            $inf->{'Instrument-source'} = strip_quote($1);
        } elsif(/^\s*Source:\s*(.*?)\s*$/i) {
            $inf->{'Instrument-source'} = strip_quote($1);
        } elsif(/^\s*Instrument:\s*(.*?)\s*$/i) {
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
    open $h, "$cmdstring |" or die "$MCSTAS::mcstas_config{'RUNCMD'}: Could not run simulation.";
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
    my ($inname, $force, $mpi, $cflags, @ccopts) = @_;
    return (undef, "$MCSTAS::mcstas_config{'RUNCMD'}: No simulation filename given") unless $inname;
    # Add a default extension of ".instr" if given name does not exist
    # as file.
    my $sim_def = $inname;
    $sim_def .= ".instr" if(! (-e $sim_def) && (-e "$sim_def.instr"));
    return(undef, "$MCSTAS::mcstas_config{'RUNCMD'}: Simulation '$sim_def' not found") unless -e $sim_def;
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
    $v->{'cflags'} = $cflags;
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
  my ($v, $printer, $mpi, $mcrunflag) = @_;
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
  $mpi   = $v->{'mpi'};
  my $cflags   = $v->{'cflags'};
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
	 my $mpistr;
	 if ($mpi) {$mpistr="--mpi=1";}
	 else {$mpistr="";}
         my $cmd = ["$MCSTAS::mcstas_config{'RUNCMD'}$MCSTAS::mcstas_config{'SUFFIX'} -c $mpistr -n0 ", $sim_def];
      &$printer(join(" ", @$cmd));
	$v->{'stage'} = POST_MCSTAS;
      return (RUN_CMD, $cmd);
      } else {
      my $cmd = [$MCSTAS::mcstas_config{'MCCODE'}, @inc, "-t", "-o", $c_name, $sim_def];
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
    my $mcstas_cflags = "";
    if ($cflags) { $mcstas_cflags = $MCSTAS::mcstas_config{CFLAGS}; }
    # Check for existing c-dependencies in the generated C-file
    open(my $fh, "<", $c_name);
    while (<$fh>) {
      if (/CFLAGS=(.*)/) {
	$mcstas_cflags .= " ".$1;
      }
      # Replace any @MCCODE_LIB@ by the McStas system path
      $mcstas_cflags =~ s/\@MCCODE_LIB\@/${MCSTAS::sys_dir}/g;
    }
    my $libs = "-lm ";
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
    my $cmd = [$cc, split(' ', $mcstas_cflags), "-o",
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
    die "$MCSTAS::mcstas_config{'RUNCMD'}: Internal: get_out_file_next: $stage";
  }
}

#
# Get the name of the executable file for the simulation, translating
# and compiling the instrument definition if necessary.
#
# The optional $force option, if true, forces unconditional recompilation.
#
sub get_out_file {
    my ($inname, $force, $mpi, $cflags, @ccopts) = @_;
    my ($v, $msg, $status, $value);
    ($v, $msg) = get_out_file_init($inname, $force, $mpi, $cflags, @ccopts);
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
            die "$MCSTAS::mcstas_config{'RUNCMD'}: Internal: get_out_file";
        }
    }
}

# McStas/McXtrace selftest procedure: copy LIB/examples and execute
sub do_test {
  my ($printer,$force, $plotter, $exec_test, $mpi, $ncount, $sim_def) = @_;
  my $pwd=getcwd;

  &$printer( "# $MCSTAS::mcstas_config{'MCCODE'} self-test ($MCSTAS::mcstas_config{'RUNCMD'} --test)");
  if ($mpi) {
      &$printer("# MPI enabled, spawning $mpi compute nodes");
  }
  &$printer(`$MCSTAS::mcstas_config{'MCCODE'} --version`);
  # create selftest direcory
  require File::Temp; # for tempdir
  $tmpdir = File::Temp::tempdir( 'selftest_XXXX' ) || return "$MCSTAS::mcstas_config{'RUNCMD'}: Couldn't create 'selftest': $@\n";
  &$printer("# Installing '$tmpdir' directory in $pwd");
  # copy all instruments
  my @paths=();
  if ($sim_def && $sim_def !~ m'\.[^/]*$') { $sim_def .= ".instr"; }
  if ($sim_def && -e $sim_def) {    # local instrument to test
    &$printer("# Using instrument $sim_def");
    push @paths, "$sim_def";
    copy("$sim_def","$tmpdir/$sim_def");
  } else {
    &$printer("# Copying instruments from $MCSTAS::sys_dir/examples/");
    if (opendir(DIR,"$MCSTAS::sys_dir/examples/")) {
      my @instruments = readdir(DIR);
      closedir(DIR);
      next unless @instruments;
      my @paths_loc = ();
      @paths_loc = map("$MCSTAS::sys_dir/examples/$_", grep(/\.(instr)$/, @instruments));
      for ($j=0 ; $j<@paths_loc; $j++) {
        my ($base, $dirname, $ext) = fileparse($paths_loc[$j],".instr");
        next if ($sim_def && $sim_def !~ $base);
        if (! copy("$paths_loc[$j]","$tmpdir/$base$ext")) {
          return "Could not copy $paths_loc[$j] to '$tmpdir' directory: $!\n";
        } else {
          push @paths, $paths_loc[$j];
        }
      }
    }
  }
  if (!@paths) { return "$MCSTAS::mcstas_config{'RUNCMD'}: no test instruments found. Aborting.\n"; }
  # go into the selftest directory
  chdir($tmpdir) or return "$MCSTAS::mcstas_config{'RUNCMD'}: Can not go into $tmpdir: $!\n";
  
  # Initialize test
  my $now = localtime;
  my $start_sec = time();
  my $n_single;
  if (!$ncount) {$n_single=1000000;}
  else {$n_single=int($ncount);}
  &$printer("# Counts:        $n_single");
  &$printer("# Output format: $plotter");
  &$printer("# Start Date:    $now");
  my $suffix=$MCSTAS::mcstas_config{'SUFFIX'};
  my $prefix=$MCSTAS::mcstas_config{'PREFIX'};
  $ENV{'MCSTAS_FORMAT'} = $plotter;
  if ($mpi) { $mpi=" --mpi=$mpi"; }

  # now execute each simulation and look for errors
  my $error_flag    = 0;
  my $accuracy_flag = 0;
  my $j;
  my $index=0;
  my $test_abstract="Test Abstract for $tmpdir\n";
  for ($j=0 ; $j<@paths ; $j++) {  # loop on instruments
    my $data=component_information($paths[$j]);     # read instrument header and extract info
    my @val_par=@{$data->{'validation_par'}};
    my @val_det=@{$data->{'validation_det'}};
    my @val_val=@{$data->{'validation_val'}};
    my ($base, $dirname, $ext) = fileparse($paths[$j],".instr");
    my $k;
    if (!@val_par) { 
    	&$printer("Instrument without test: $base"); 
    	my $this_cmd = "$MCSTAS::mcstas_config{'RUNCMD'} -c -n0 --no-cflags $base";
    	&$printer("Executing: $this_cmd");
    	my $res = qx/$this_cmd/;
    	if ($child_error_code) {
    	  &$printer("[FAILED] $base: ($child_error_code): $child_error_text");
        $test_abstract .= "[FAILED] $base". "_$index (compilation/execution)\n";
        $error_flag++; 
      } else {
    		$test_abstract .= "[notest] $base (no test procedure)\n";
    	}
    }
    for ($k=0; $k<@val_par; $k++) { # loop on tests
      if ($k == 0) { &$printer("INSTRUMENT $base:\n  $data->{'identification'}{'short'}"); }
      my $this_cmd =$val_par[$k];
      $index++;
      # check command
      if ($this_cmd !~ m/$base/) { $this_cmd = "$base $this_cmd"; } # only parameters ?
      if ($this_cmd !~ m/$MCSTAS::mcstas_config{'RUNCMD'}/ && $this_cmd !~ m/$MCSTAS::mcstas_config{'PLOTCMD'}/ && $this_cmd !~ m/$MCSTAS::mcstas_config{'TRACECMD'}/) 
                                 { $this_cmd = "$MCSTAS::mcstas_config{'RUNCMD'} $this_cmd"; } # omitted $MCSTAS::mcstas_config{'RUNCMD'} ?
      if ($this_cmd !~ m/mpi/ && $mpi) { $this_cmd .= $mpi; }              # add mpi
      if ($this_cmd !~ m/-n/ && $this_cmd !~ m/--ncount/) { $this_cmd.= " -n $n_single"; }
      if ($this_cmd !~ m/--format/) { $this_cmd.= " --format=$plotter"; }
      if ($this_cmd !~ m/-d/ && $this_cmd !~ m/--dir/) { $this_cmd.= " -d $base" . "_$index"; }
      
      if ($this_cmd =~ m/$MCSTAS::mcstas_config{'RUNCMD'}/) { $this_cmd .= " --no-cflags"; }
      &$printer("Executing: $this_cmd");
      my $res = qx/$this_cmd/;
      my $child_error_text = $!;
      my $child_error_code = $?;
      if ($child_error_code) {
        &$printer("[FAILED] $base: ($child_error_code): $child_error_text");
        $test_abstract .= "[FAILED] $base". "_$index (compilation/execution)\n";
        $error_flag++; 
        last; # go to next instrument (exit for $k)
      } else {
        #Analyse test output if reference value is available
        if ($val_val[$k] ne 0) { # there is a reference value...
          # split the output in lines
          my $line;
          my $sim_I= 0;
          my $sim_E= 0;
          for $line (split "\n", $res) {
            # search reference monitor in these lines
            if($line =~ m/Detector: ([^ =]+_I) *= *([^ =]+) ([^ =]+_ERR) *= *([^ =]+) ([^ =]+_N) *= *([^ =]+) *(?:"[^"]+" *)?$/) {
              my $sim_I_name = $1;
              if ($val_det[$k] eq $sim_I_name) {
                $sim_I = $2;
                $sim_E = abs($4);
              }
            }
          } # end for $line
          if ($sim_E) { # found monitor for this test, either below 1 % or within Error bar
            my $diff = int(abs($sim_I/$val_val[$k]-1)*100+0.99);
            if (abs($sim_I/$sim_E) < 2) { # error is higher than half signal: stats too low
              &$printer("[OK] $base: $val_det[$k] = $sim_I +/- $sim_E (statistics too low for testing, increase ncount)"); 
              $test_abstract .= "[OK]     $base". "_$index (statistics too low for testing, increase ncount)\n";
            } elsif (abs($val_val[$k]-$sim_I) < abs($val_val[$k]*0.05) || abs($val_val[$k]-$sim_I) < 3*$sim_E)  { 
              &$printer("[OK] $base: $val_det[$k] = $sim_I +/- $sim_E, equals $val_val[$k] within $diff \%"); 
              $test_abstract .= "[OK]     $base". "_$index (accuracy, $diff \%)\n";
            } elsif (abs($val_val[$k]-$sim_I) < abs($val_val[$k]*0.2))  { 
              &$printer("[OK] $base: $val_det[$k] = $sim_I +/- $sim_E, equals $val_val[$k] within $diff \%"); 
              $test_abstract .= "[OK]     $base". "_$index (accuracy, fair $diff \%)\n";
            } else {
              $accuracy_flag++;
              &$printer("[FAILED] $base: $val_det[$k] = $sim_I +/- $sim_E, should be $val_val[$k] ");
              $test_abstract .= "[FAILED] $base". "_$index (accuracy off by $diff \%)\n";
            }
          } else {
            &$printer("[???] $base: $val_det[$k] = $sim_I (may have failed, reference not found)"); 
            $test_abstract .= "[??????] $base". "_$index (reference not found)\n";
          }
        }  else {   # no reference value
          &$printer("[OK] $base: $val_det[$k] = $sim_I (accuracy not checked)"); 
          $test_abstract .= "[OK]     $base". "_$index (accuracy not checked)\n";
        }
      } # end else $child_error_code (execution)
    } # end for $k (examples in instrument)
  } # end for $j (instruments)
  my $elapsed_sec = time() - $start_sec;
  $test_abstract .= "\n";
  $now = localtime();
  &$printer($test_abstract);
  if ($error_flag) {
    &$printer("# Execution check:    FAILED. $error_flag instrument(s) did not compile/execute.");
    &$printer("# >> Check instruments and $MCSTAS::mcstas_config{'MCCODE'} installation.");
  } else {
    &$printer("# Execution check:    OK.     Computing time: $elapsed_sec [sec] for $index tests.");
    if ($accuracy_flag > 2) {
      &$printer("# Accuracy check:     FAILED. $accuracy_flag test(s) with inaccurate results.");
    } elsif ($accuracy_flag==0) {
      &$printer("# Accuracy check:     OK.");
    } else {
    	&$printer("# Accuracy check:     FAIR. $accuracy_flag test(s) with inaccurate results.");
    }
  }

  &$printer("# End Date: $now");
  chdir($pwd) or return "$MCSTAS::mcstas_config{'RUNCMD'}: Can not come back to $pwd: $!\n";; # come back to initial directory
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
    my @val_det=();
    my @val_val=();
    my @val_par=();
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
        } elsif(/\%EXAMPLE: (.*) Detector: *([^ =]+_I|[^ =]+) *= *([^ =]+)/i) {
                  push @val_det, "$2";
                  push @val_val,  $3;
                  push @val_par, "$1";       
        } elsif(/\%EXAMPLE: (.*)/i) {
                  push @val_det, "";
                  push @val_val, 0;
                  push @val_par, "$1";    
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
    $d->{'validation_det'} = \@val_det;
    $d->{'validation_val'} = \@val_val;
    $d->{'validation_par'} = \@val_par;
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
                $s =~ /^\s*$/ || die "$MCSTAS::mcstas_config{'RUNCMD'}: Internal: parse_header match 1";
                $text = "$s";
            }
        }
        $d->{'parhelp'}{$i}{'unit'} = $unit;
        $d->{'parhelp'}{$i}{'text'} = $text;
    }
    return $d;
}

# This sub gets component information by parsing the McStas/McXtrace
# metalanguage. For now this is a regexp hack, later the real mcstas/mcxtrace
# parser will be used.
sub get_comp_info {
    my ($name, $d) = @_;
    my $file = new FileHandle;
    my ($cname, $decl, $init, $trace, $finally, $disp, $typ);
    my (@dpar, @spar, @ipar, @opar);
    open($file, $name)  || die "$MCSTAS::mcstas_config{'RUNCMD'}: Could not open file $name\n";
    local $/ = undef;                # Read the whole file in one go.
    my $s = <$file>;
    close($file);
    $typ = "Component";
    @opar = (); @dpar = (); @spar = ();
    if ($s =~ m!DEFINE\s+INSTRUMENT\s+([a-zA-Z0-9_]*)\s*\(([-+.a-zA-Z0-9_ \t\n\r=,/*{}\"]*)\)!i) {
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
        if($s =~ m!SETTING\s+PARAMETERS\s*\(([-+.a-zA-Z0-9_ \t\n\r=,/*\"]+)\)!i && $typ ne "Instrument") {
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
		    my $val = $2;
                    my @p_splitted = split(" ", $p);
                    my $length = scalar @p_splitted;
                    my $p_last_word = $p_splitted[$length-1];
                    push @spar, $p_last_word;
                    my $p_first_word = $p_splitted[0];
                    if ($length > 1 && $p_first_word !~ m/char/ && $p_first_word !~ m/string/) {
                      print STDERR "
Warning: SETTING parameter $1 with default value $val\n
         is not of type char/string. Ignoring default value.\n";
                    } else {
                      $d->{'parhelp'}{$p_last_word}{'default'} = $val;
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



