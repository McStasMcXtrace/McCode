# Library of common routines for McStas frontends.

sub strip_quote {
    my ($str) = @_;
    $str = $1 if($str =~ /^'(.*)'$/); # Remove quotes if present.
    return $str;
}

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
    while(<$h>) {
	if(/^\s*Name:\s*([a-zA-ZæøåÆØÅ_0-9]+)\s*$/i) {
	    $inf->{'Name'} = $1;
	} elsif(/^\s*Parameters:\s*([a-zA-ZæøåÆØÅ_0-9 \t]*?)\s*$/i) {
	    $inf->{'Parameters'} = [split ' ',$1];
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

sub read_simulation_info {
    my ($handle) = @_;
    my $inf = { Params => {} };
    while(<$handle>) {
	if(/^\s*Date:\s*(.*?)\s*$/i) {
	    $inf->{'Date'} = $1;
	} elsif(/^\s*Ncount:\s*([-+0-9.eE]+)\s*$/i) {
	    $inf->{'Ncount'} = $1;
	} elsif(/^\s*Seed:\s*([-+0-9.eE]+)\s*$/i) {
	    $inf->{'Seed'} = $1;
	} elsif(/^\s*Trace:\s*(no|yes)\s*$/i) {
	    $inf->{'Trace'} = get_yes_no($1);
	} elsif(/^\s*Param:\s*([a-zA-ZæøåÆØÅ_0-9]+)\s*=\s*([-+0-9.eE]+)\s*$/i){
	    $inf->{'Params'}{$1} = $2;
	} elsif(/^\s*end\s+simulation\s*$/i) {
	    last;
	} else {
	    die "Invalid line in siminfo file:\n'$_'";
	}
    }
    return $inf;
}

sub read_data_info {
    my ($handle, $basedir) = @_;
    my ($type,$fname);
    my ($compname,$title,$xlabel,$ylabel) = ("","","","");
    my ($xmin,$xmax,$ymin,$ymax) = (0,1,0,1);
    while(<$handle>) {
	if(/^\s*type:\s*(.*?)\s*$/i) {
	    $type = $1;
	} elsif(/^\s*component:\s*([a-zA-ZæøåÆØÅ_0-9]+)\s*$/i) {
	    $compname = $1;
	} elsif(/^\s*title:\s*(.*?)\s*$/i) {
	    $title = strip_quote($1);
	} elsif(/^\s*filename:\s*(.*?)\s*$/i) {
	    $fname = strip_quote($1);
	} elsif(/^\s*xlabel:\s*(.*?)\s*$/i) {
	    $xlabel = strip_quote($1);
	} elsif(/^\s*ylabel:\s*(.*?)\s*$/i) {
	    $ylabel = strip_quote($1);
	} elsif(/^\s*xylimits:\s*
		([-+0-9.eE]+)\s+
		([-+0-9.eE]+)\s+
		([-+0-9.eE]+)\s+
		([-+0-9.eE]+)\s*$/ix) {
	    ($xmin,$xmax,$ymin,$ymax) = ($1,$2,$3,$4);
	} elsif(/^\s*xlimits:\s*
		([-+0-9.eE]+)\s+
		([-+0-9.eE]+)\s*$/ix) {
	    ($xmin,$xmax) = ($1,$2);
	} elsif(/^\s*end\s+data\s*$/i) {
	    last;
	} else {
	    die "Invalid line in siminfo file:\n'$_'";
	}
    }
    die "Missing type for component $compname"
	unless $type;
    die "Missing filename for component $compname"
	unless $fname;
    return { Type => $type,
	     Component => $compname,
	     Title => $title,
	     Filename => $basedir ? "$basedir/$fname" : $fname,
	     Xlabel => $xlabel,
	     Ylabel => $ylabel,
	     Limits => [$xmin,$xmax,$ymin,$ymax]
	 };
}

sub read_sim_info {
    my ($handle, $basedir) = @_;
    my @datalist = ();
    my $instrument_info;
    my $simulation_info;
    while(<$handle>) {
	if(/^\s*begin\s+data\s*$/i) {
	    push @datalist, read_data_info($handle, $basedir);
	} elsif(/^\s*begin\s+instrument\s*$/i) {
	    $instrument_info = read_instrument_info($handle);
	} elsif(/^\s*begin\s+simulation\s*$/i) {
	    $simulation_info = read_simulation_info($handle);
	} elsif(/^\s*$/) {
	    next;
	} else {
	    die "Invalid line in siminfo file:\n'$_'";
	}
    }
    return ($instrument_info, $simulation_info, \@datalist);
}

sub read_sim_file {
    my ($file) = @_;
    my $basedir;
    $basedir = $1 if $file =~ m|^(.*)/[^/]*$|;
    my $handle = new FileHandle;
    open $handle, $file or die "Could not open file '$file'";
    read_sim_info($handle, $basedir);
}

1;
