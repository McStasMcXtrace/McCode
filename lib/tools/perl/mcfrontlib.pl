# Library of common routines for McStas frontends.

use PDL;

use FileHandle;

require "mcrunlib.pl";

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
	} elsif(/^\s*Numpoints:\s*([-+0-9.eE]+)\s*$/i) {
	    $inf->{'Numpoints'} = $1;
	} elsif(/^\s*Seed:\s*([-+0-9.eE]+)\s*$/i) {
	    $inf->{'Seed'} = $1;
	} elsif(/^\s*Trace:\s*(no|yes)\s*$/i) {
	    $inf->{'Trace'} = get_yes_no($1);
	} elsif(/^\s*Param:\s*([a-zA-ZÊ¯Â∆ÿ≈_0-9]+)\s*=\s*([-+0-9.eE]+)\s*$/i){
	    $inf->{'Params'}{$1} = $2;
	} elsif(/^\s*Param:\s*([a-zA-ZÊ¯Â∆ÿ≈_0-9]+)\s*=\s*"(.*)"\s*$/i){
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
    my ($type, $fname, $data, $xvar, $yvar, $yerr, @xvars, @yvars, @yerrs);
    my @vars = qw/X N I p2/;
    my @vals = ();
    my ($compname,$title,$xlabel,$ylabel) = ("","","","");
    my ($xmin,$xmax,$ymin,$ymax) = (0,1,0,1);
    while(<$handle>) {
	if(/^\s*type:\s*(.*?)\s*$/i) {
	    $type = $1;
	} elsif(/^\s*component:\s*([a-zA-ZÊ¯Â∆ÿ≈_0-9]+)\s*$/i) {
	    $compname = $1;
	} elsif(/^\s*title:\s*(.*?)\s*$/i) {
	    $title = strip_quote($1);
	} elsif(/^\s*filename:\s*(.*?)\s*$/i) {
	    $fname = strip_quote($1);
	} elsif(/^\s*variables:\s*([a-zA-ZÊ¯Â∆ÿ≈_0-9 \t]*?)\s*$/i) {
	    @vars = split(" ", $1);
	} elsif(/^\s*values:\s*([-+0-9.eE \t]*?)\s*$/i) {
	    @vals = split(" ", $1);
	} elsif(/^\s*xvar:\s*([a-zA-ZÊ¯Â∆ÿ≈_0-9]+?)\s*$/i) {
	    $xvar = $1;
	} elsif(/^\s*yvar:\s*([a-zA-ZÊ¯Â∆ÿ≈_0-9]+?)\s*$/i) {
	    $yvar = $1;
	    $yerr = undef;
	} elsif(/^\s*yvar:\s*
		\(\s*([a-zA-ZÊ¯Â∆ÿ≈_0-9]+)\s*,
		  \s*([a-zA-ZÊ¯Â∆ÿ≈_0-9]+)\s*
		\)\s*$/ix) {
	    $yvar = $1;
	    $yerr = $2;
	} elsif(/^\s*xvars:\s*
		([a-zA-ZÊ¯Â∆ÿ≈_0-9]+
		 (\s+[a-zA-ZÊ¯Â∆ÿ≈_0-9]+)*
		)\s*$/ix) {
	    @xvars = split(" ", $1);
	} elsif(/^\s*yvars:
		((
		 \s*\([a-zA-ZÊ¯Â∆ÿ≈_0-9]+,[a-zA-ZÊ¯Â∆ÿ≈_0-9]+\)
		)+)\s*$/ix) {
	    @yvars = ();
	    @yerrs = ();
	    for (split(" ", $1)) {
		if(/\(([a-zA-ZÊ¯Â∆ÿ≈_0-9]+),([a-zA-ZÊ¯Â∆ÿ≈_0-9]+)\)/) {
		    push @yvars, $1;
		    push @yerrs, $2;
		} else {
		    die "Internal: mcfrontlib/yvars";
		}
	    }
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
    # Use first of multiple X variables as single X variable.
    $xvar = $xvars[0] if @xvars && !$xvar;
    # Use first of multiple Y variables as single Y variable.
    $yvar = $yvars[0] if @yvars && !$yvar;
    $yerr = $yerrs[0] if @yerrs && !$yvar;
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
    # Convert type multiarray_1d to multiple array_1d (for mcrun scan output).
    if($type =~ /^multiarray_1d\((.*)\)$/i) {
	my $size = $1;
	my $res = [];
	my $i;
	for($i = 0; $i < @yvars; $i++) {
	    push @$res, { Type => "array_1d($size)",
			  Component => $compname,
			  Title => $title,
			  Variables => \@vars,
			  Values => \@vals,
			  Xvar => [$xvar],
			  Yvar => [$yvars[$i]],
			  Yerr => [$yerrs[$i]],
			  Filename => $fname,
			  "Numeric Data" => $data,
			  Xlabel => $xlabel,
			  Ylabel => "$ylabel $yvars[$i]",
			  Limits => [$xmin,$xmax,$ymin,$ymax]
			  };
	}
	return @$res;
    } else {
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
}

sub read_sim_info {
    my ($handle, $basedir) = @_;
    my @datalist = ();
    my $instrument_info;
    my $simulation_info;
    while(<$handle>) {
	if(/^\s*begin\s+data\s*$/i) {
	    my @info = read_data_info($handle, $basedir);
	    push @datalist, grep($_->{Type} !~ /^\s*array_0d\s*$/, @info);
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
    while(<$f>) {
	if(/\%I[a-z]*/i) {
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
		if(/^[ \t]*([a-zA-Z0-9Ê¯Â∆ÿ≈_]+)\s*:(.*)/) {
		    $thisparm = \$d->{'parhelp'}{$1}{'full'};
		    $$thisparm = "$2\n";
		} elsif(/^[ \t]*$/) { # Empty line
		    undef $thisparm;
		} elsif($thisparm && /^(  | *\t)[ \t]*(.*)/) {
		    # Continuation line needs at least two additional
		    # indentations
		    $$thisparm .= "$2\n";
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
	    $unit = "";
	    if($s =~ /^\s*((.|\n)*\S)\s*$/) {
		$text = $1;
	    } else {
		$s =~ /^\s*$/ || die "Internal: match 1";
		$text = "";
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
    my ($cname, $decl, $init, $trace, $finally, $disp);
    my (@dpar, @spar, @ipar, @opar);
    open($file, $name)  || die "Could not open file $name\n";
    local $/ = undef;		# Read the whole file in one go.
    my $s = <$file>;
    close($file);
    if($s =~ /DEFINE\s+COMPONENT\s+([a-zA-ZÊ¯Â∆ÿ≈0-9_]+)/i) {
	$cname = $1;
    } else {
	$cname = "<Unknown>";
    }
    @dpar = ();
    if($s =~ m!DEFINITION\s+PARAMETERS\s*\(([-+.a-zA-ZÊ¯Â∆ÿ≈0-9_ \t\n\r=,/*]+)\)!i) {
	foreach (split(",", $1)) {
	    if(/^\s*([a-zA-ZÊ¯Â∆ÿ≈0-9_]+)\s*\=\s*([-+.e0-9]+)\s*$/) {
		push @dpar, $1;
		$d->{'parhelp'}{$1}{'default'} = $2;
	    } elsif(/^\s*([a-zA-ZÊ¯Â∆ÿ≈0-9_]+)\s*$/) {
		push @dpar, $1;
	    } else {
		print STDERR "Warning: Unrecognized DEFINITION PARAMETER in component $cname.\n";
	    }
	}
    }
    @spar = ();
    if($s =~ m!SETTING\s+PARAMETERS\s*\(([-+.a-zA-ZÊ¯Â∆ÿ≈0-9_ \t\n\r=,/*]+)\)!i) {
	foreach (split(",", $1)) {
	    if(/^\s*([a-zA-ZÊ¯Â∆ÿ≈0-9_]+)\s*\=\s*([-+.e0-9]+)\s*$/) {
		push @spar, $1;
		$d->{'parhelp'}{$1}{'default'} = $2;
	    } elsif(/^\s*([a-zA-ZÊ¯Â∆ÿ≈0-9_]+)\s*$/) {
		push @spar, $1;
	    } else {
		print STDERR "Warning: Unrecognized SETTING PARAMETER in component $cname.\n";
	    }
	}
    }
    @ipar = (@dpar, @spar);
    if($s =~ /OUTPUT\s+PARAMETERS\s*\(([a-zA-ZÊ¯Â∆ÿ≈0-9_, \t\r\n]+)\)/i) {
	@opar = split (/\s*,\s*/, $1);
    } else {
	@opar = ();
    }
    # DECLARE, INITIALIZE, ... blocks will have to wait for the real parser.
    $d->{'name'} = $cname;
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

1;
