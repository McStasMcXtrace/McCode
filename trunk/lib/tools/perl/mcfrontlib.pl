# Library of common routines for McStas frontends.

use PDL;

require FileHandle;

sub strip_quote {
    my ($str) = @_;
    $str = $1 if($str =~ /^'(.*)'$/); # Remove quotes if present.
    return $str;
}

sub get_yes_no {
    my ($str) = @_;
    return ($str =~ /yes/i) ? 1 : 0;
}

# Read 2D numeric data, skipping comment lines.
sub read_data_file_2D {
    my ($file) = @_;
    my $h = new FileHandle;
    if(open($h, $file)) {
	my @list = ();
	while(<$h>) {
	    next if /^\s*#/;
	    push(@list, new PDL (split " "));
	}
	close $h;
	return cat @list;
    } else {
	print STDOUT "Warning: failed to read data file \"$file\"\n";
	return undef;
    }
}

# Read 2D embedded numeric data.
sub read_array2D {
    my ($h,$m,$n) = @_;
    my @list = ();
    while(<$h>) {
	if(/^[-+0-9eE. \t]+$/) {
	    push(@list, new PDL (split " "));
	} else {
	    last if /^\s*end\s+array2D\s*$/i;
	    die "Bad embedded numeric data in array2D in file.";
	}
    }
    return cat @list;
}


# Get numerical data for 2D detector, reading it from file if necessary
# the first time.
sub get_detector_data_2D {
    my ($info) = @_;
    $info->{'Numeric Data'} = read_data_file_2D($info->{'Filename'})
        unless defined($info->{'Numeric Data'});
    return $info->{'Numeric Data'};
}


# Get numerical data for 1D detector, reading it from file if necessary
# the first time.
sub get_detector_data_1D {
    my ($info) = @_;
    if($info->{'Numeric Data'}) {
	return $info->{'Numeric Data'};
    } else {
	my ($file) = @_;
	my $a = read_data_file_2D($info->{'Filename'});
	return undef unless defined($a);
	my ($m,$n) = $a->dims;
	my $vars = $info->{'Variables'};
	my $i;
	my $r = {};
	for $i (0..$m-1) {
	    my $key = $vars->[$i] ? $vars->[$i] : "column $i";
	    $r->{$key} = $a->slice("($i),");
	}
	$info->{'Numeric Data'} = $r;
	return $r;
    }
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

# Unquote a C-style quoted string. Limited to the four quote
# combinations '\n', '\r', '\"', and '\\'.
# The basic technique is to do a simple substitution, but it is
# complicated by the possibility of having multiple backslashes in a
# row (ie '\\\n'). To solve this problem, we first change all '\\'
# sequences to '\!'.
sub str_unquote {
    my ($val) = @_;
    # First replace any initial '\\' with '\!'.
    $val =~ s/^\\\\/\\!/;
    # Now replace any other '\\' with '\!'.
    while($val =~ s/([^\\])\\\\/$1\\!/g) {}
    # Finally replace all quote-combinations with their intended character.
    $val =~ s/\\n/\n/g;
    $val =~ s/\\r/\r/g;
    $val =~ s/\\"/"/g;
    $val =~ s/\\!/\\/g;
    return $val;
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
	} elsif(/^\s*Param:\s*([a-zA-ZæøåÆØÅ_0-9]+)\s*=\s*"(.*)"\s*$/i){
	    my ($param, $val) = ($1, $2);
	    $inf->{'Params'}{$param} = str_unquote($val);
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
    my ($type, $fname, $data, $xvar, $yvar, $yerr);
    my @vars = qw/X N I p2/;
    my @vals = ();
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
	} elsif(/^\s*variables:\s*([a-zA-ZæøåÆØÅ_0-9 \t]*?)\s*$/i) {
	    @vars = split(" ", $1);
	} elsif(/^\s*values:\s*([-+0-9.eE \t]*?)\s*$/i) {
	    @vals = split(" ", $1);
	} elsif(/^\s*xvar:\s*([a-zA-ZæøåÆØÅ_0-9]+?)\s*$/i) {
	    $xvar = $1;
	} elsif(/^\s*yvar:\s*([a-zA-ZæøåÆØÅ_0-9]+?)\s*$/i) {
	    $yvar = $1;
	    $yerr = undef;
	} elsif(/^\s*yvar:\s*
		\(\s*([a-zA-ZæøåÆØÅ_0-9]+)\s*,
		  \s*([a-zA-ZæøåÆØÅ_0-9]+)\s*
		\)\s*$/ix) {
	    $yvar = $1;
	    $yerr = $2;
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
	} elsif(/^\s*begin array2D \(([0-9]+),([0-9]+)\)\s*/i) {
	    $data = read_array2D($handle,$1,$2);
	} elsif(/^\s*end\s+data\s*$/i) {
	    last;
	} else {
	    die "Invalid line in siminfo file:\n'$_'";
	}
    }
    die "Missing type for component $compname"
	unless $type;
    # Convert 2D array to 1D array hash for 1D detector type.
    if($data && $type =~ /^\s*array_1d\s*\(\s*([0-9]+)\s*\)\s*$/i) {
	my $r = {};
	my ($m,$n) = $data->dims;
	my $i;
	for $i (0..$m-1) {
	    my $key = $vars[$i] ? $vars[$i] : "column $i";
	    $r->{$key} = $data->slice("($i),");
	}
	$data = $r;
    }
    # Select some reasonable defaults for axis variables if not present.
    if($type !~ /^\s*array_0d\s*$/i) {
	$xvar = $vars[0] ? $vars[0] : "column 0" unless $xvar;
	$yvar = $vars[1] ? $vars[1] : "column 1" unless $yvar;
	die "Missing filename for component $compname"
	    unless $fname || $data;
	$fname = "$basedir/$fname" if($fname && $basedir);
    }
    return { Type => $type,
	     Component => $compname,
	     Title => $title,
	     Variables => \@vars,
	     Values => \@vals,
	     Xvar => [$xvar],
	     Yvar => [$yvar],
	     Yerr => [$yerr],
	     Filename => $fname,
	     "Numeric Data" => $data,
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
	    my $info = read_data_info($handle, $basedir);
	    push @datalist, $info
		unless $info->{Type} =~ /^\s*array_0d\s*$/i;
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
    $basedir = $1 if $file && $file =~ m|^(.*)/[^/]*$|;
    my $handle = new FileHandle;
    open $handle, $file or die "Could not open file '$file'";
    read_sim_info($handle, $basedir);
}

1;
