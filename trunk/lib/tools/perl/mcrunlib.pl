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
    $inf->{'RAW'} = [];                # List of lines from output of sim.out --info
    while(<$h>) {
        push @{$inf->{'RAW'}}, $_;
        if(/^\s*Name:\s*([a-zA-ZÊ¯Â∆ÿ≈_0-9]+)\s*$/i) {
            $inf->{'Name'} = $1;
        } elsif(/^\s*Parameters:\s*([a-zA-ZÊ¯Â∆ÿ≈_0-9 \t()]*?)\s*$/i) {
            my $full = $1;
            my $parms = [ ];
            my $parmtypes = { };
            my $p;
            for $p (split ' ', $full) {
                if($p =~ /^([a-zA-ZÊ¯Â∆ÿ≈_0-9+]+)\(([a-z]+)\)$/) {
                    push @$parms, $1;
                    $parmtypes->{$1} = $2;
                } elsif($p =~ /^([a-zA-ZÊ¯Â∆ÿ≈_0-9+]+)$/) {
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
            my $cc = defined($ENV{'MCSTAS_CC'}) ?
                $ENV{'MCSTAS_CC'} : $MCSTAS::mcstas_config{CC};
            my $cflags = defined($ENV{'MCSTAS_CFLAGS'}) ?
                $ENV{'MCSTAS_CFLAGS'} : $MCSTAS::mcstas_config{CFLAGS};
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
    local $/ = undef;                # Read the whole file in one go.
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
            if(/^\s*([a-zA-ZÊ¯Â∆ÿ≈0-9_\s\*]+)\s*\=\s*([-+.e0-9]+)\s*$/) {
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
