require "mcstas_config.perl";

#
# Get the name of the executable file for the simulation, translating
# and compiling the instrument definition if necessary.
#
sub get_out_file {
    my ($inname) = @_;
    unless($inname) {
	print STDERR "No simulation filename given\n";
	return undef;
    }
    # Add a default extension of ".instr" if given name does not exist
    # as file.
    my $sim_def = $inname;
    if(! (-e $sim_def) && (-e "$sim_def.instr")) {
	$sim_def .= ".instr";
    }
    unless(-e $sim_def) {
	print STDERR "Simulation '$sim_def' not found\n";
	return undef;
    }
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
    my $c_name = "$base_name.c";
    my $out_name = "$base_name.out";
    $sim_def = "$base_name.instr" unless $file_type eq MCSTAS;

    # Translate simulation definition into C if newer than existing C
    # version.
    $sim_age = -e $sim_def ? -M $sim_def : undef;
    $c_age = -e $c_name ? -M $c_name : undef;
    $out_age = -e $out_name ? -M $out_name : undef;
    if($file_type eq C && (defined($sim_age) && $sim_age < $c_age)) {
	print STDERR "Warning: simulation definition '",
	$sim_def, "' is newer than '", $c_name, "'\n";
    }
    if($file_type eq OUT && (defined($sim_age) && $sim_age < $out_age)) {
	print STDERR "Warning: simulation definition '",
	$sim_def, "' is newer than '", $out_name, "'\n";
    }
    if($file_type eq OUT && (defined($c_age) && $c_age < $out_age)) {
	print STDERR "Warning: C source '",
	$c_name, "' is newer than '", $out_name, "'\n";
    }
    if($file_type eq MCSTAS && (!defined($c_age) || $c_age > $sim_age)) {
	print "Translating instrument definition '",
	$sim_def, "' into C ...\n";
	$cmd = "mcstas -t -o \"$c_name\" \"$sim_def\"";
	print "$cmd\n";
	$exit_val = (system($cmd)) >> 8;
	if($exit_val) {
	    print STDERR "** Error exit **\n";
	    return undef;
	}
	$c_age = -M $c_name;
	$file_type = C;
	$out_age = undef;		# Force recompilation.
    }
    unless(-e $c_name) {
	print STDERR "Could not translate simulation '",
	$sim_def, "' into C\n";
	return undef;
    }

    # Compile C source if newer than existing out file.
    if($file_type eq C && (!defined($out_age) || $out_age > $c_age)) {
	print "Compiling C source '$c_name' ...\n";
	$cmd = "$mcstas_config{CC} $mcstas_config{CFLAGS} " .
	    "-o \"$out_name\" \"$c_name\" -lm";
	print "$cmd\n";
	$exit_val = (system($cmd)) >> 8;
	if($exit_val) {
	    print STDERR "** Error exit **\n";
	    return undef;
	}
    }
    unless(-e $out_name) {
	print STDERR "Could not compile C source file '",
	$c_name, "'\n";
	return undef;
    }
    return $out_name;
}

1;
