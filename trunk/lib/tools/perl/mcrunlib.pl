require "mcstas_config.perl";

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
    $inf->{'RAW'} = [];		# List of lines from output of sim.out --info
    while(<$h>) {
	push @{$inf->{'RAW'}}, $_;
	if(/^\s*Name:\s*([a-zA-ZæøåÆØÅ_0-9]+)\s*$/i) {
	    $inf->{'Name'} = $1;
	} elsif(/^\s*Parameters:\s*([a-zA-ZæøåÆØÅ_0-9 \t()]*?)\s*$/i) {
	    my $full = $1;
	    my $parms = [ ];
	    my $parmtypes = { };
	    my $p;
	    for $p (split ' ', $full) {
		if($p =~ /^([a-zA-ZæøåÆØÅ_0-9+]+)\(([a-z]+)\)$/) {
		    push @$parms, $1;
		    $parmtypes->{$1} = $2;
		} elsif($p =~ /^([a-zA-ZæøåÆØÅ_0-9+]+)$/) {
		    # Backward compatibility: no type specifier.
		    push @$parms, $1;
		    $parmtypes->{$1} = 'double'; # Default is double
		} else {
		    die "Invalid parameter specification:\n'$p'";
		}
	    }
	    $inf->{'Parameters'} = $parms;
	    $inf->{'Parameter-types'} = $parmtypes;
	} elsif(/^\s*Instrument-source:\s*(.*?)\s*$/i) {
	    $inf->{'Instrument-source'} = strip_quote($1);
	} elsif(/^\s*Trace-enabled:\s*(no|yes)\s*$/i) {
	    $inf->{'Trace-enabled'} = get_yes_no($1);
	} elsif(/^\s*Default-main:\s*(no|yes)\s*$/i) {
	    $inf->{'Default-main'} = get_yes_no($1);
	} elsif(/^\s*Embedded-runtime:\s*(no|yes)\s*$/i) {
	    $inf->{'Embedded-runtime'} = get_yes_no($1);
	} elsif(/^\s*end\s+instrument\s*$/i) {
	    last;
	} else {
	    die "Invalid line in siminfo file:\n'$_'";
	}
    }
    return $inf;
}

sub get_sim_info {
    my ($simprog) = @_;
    use FileHandle;
    my $h = new FileHandle;
    open $h, "$simprog --info |" or die "Could not run simulation.";
    my $inf = read_instrument_info($h);
    close $h;
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
    my ($inname, $force) = @_;
    return (undef, "No simulation filename given") unless $inname;
    # Add a default extension of ".instr" if given name does not exist
    # as file.
    my $sim_def = $inname;
    $sim_def .= ".instr" if(! (-e $sim_def) && (-e "$sim_def.instr"));
    return(undef, "Simulation '$sim_def' not found") unless -e $sim_def;
    my $file_type = MCSTAS;
    my $base_name = $sim_def;
    if($sim_def =~ /(.*)\.instr$/) {
	$base_name = $1;
    } elsif($sim_def =~ /(.*)\.c$/) {
	$base_name = $1;
	$file_type = C;
    } elsif($sim_def =~ /(.*)\.out$/) {
	$base_name = $1;
	$file_type = OUT;
    }
    my $dir;
    if($base_name =~ m'^(.*)/[^/]*$') { # quote hack -> ') {
	$dir = $1;
    }
    my $c_name = "$base_name.c";
    my $out_name = "$base_name.out";
    $sim_def = "$base_name.instr" unless $file_type eq MCSTAS;
    my $v = { };
    $v->{'force'} = $force;
    $v->{'file_type'} = $file_type;
    $v->{'dir'} = $dir;
    $v->{'sim_def'} = $sim_def;
    $v->{'c_name'} = $c_name;
    $v->{'out_name'} = $out_name;
    $v->{'sim_age'} = -e $sim_def ? -M $sim_def : undef;
    $v->{'c_age'} = -e $c_name ? -M $c_name : undef;
    $v->{'out_age'} = -e $out_name ? -M $out_name : undef;
    $v->{'stage'} = PRE_MCSTAS;
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
    my ($v, $printer) = @_;
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
	    my @inc = $v->{'dir'} ? ("-I", $v->{'dir'}) : ();
	    my $cmd = ["mcstas", @inc, "-t", "-o", $c_name, $sim_def];
	    &$printer(join(" ", @$cmd));
	    $v->{'stage'} = POST_MCSTAS;
	    return (RUN_CMD, $cmd);
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
	if(($file_type eq MCSTAS || $file_type eq C) &&
	   ($force || !defined($out_age) || $out_age > $c_age)) {
	    &$printer("Compiling C source '$c_name' ...");
	    # ToDo: splitting CFLAGS should handle shell quoting as well ...
	    my $cc = $ENV{'MCSTAS_CC'} || $MCSTAS::mcstas_config{CC};
	    my $cflags =
		$ENV{'MCSTAS_CFLAGS'} || $MCSTAS::mcstas_config{CFLAGS};
	    my $cmd = [$cc, split(' ', $cflags), "-o",
		       $out_name, $c_name, "-lm"];
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
	die "Internal: get_out_file_next: $stage";
    }
}

#
# Get the name of the executable file for the simulation, translating
# and compiling the instrument definition if necessary.
#
# The optional $force option, if true, forces unconditional recompilation.
#
sub get_out_file {
    my ($inname, $force) = @_;
    my ($v, $msg, $status, $value);
    ($v, $msg) = get_out_file_init($inname, $force);
    unless($v) {
	print STDERR "$msg\n";
	return undef;
    }
    for(;;) {
	($status, $value) = get_out_file_next($v, sub { print "$_[0]\n"; });
	if($status eq FINISHED) {
	    return $value;
	} elsif($status eq RUN_CMD) {
	    my $exit_val = system(@$value);
	    if($exit_val) {
		print STDERR "** Error exit **\n";
		return undef;
	    }
	    next;
	} elsif($status eq ERROR) {
	    print STDERR "$value\n";
	    return undef;
	} elsif(!($status eq CONTINUE)) {
	    die "Internal: get_out_file";
	}
    }
}

1;
