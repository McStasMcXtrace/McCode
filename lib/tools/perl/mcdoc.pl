#! /usr/bin/perl -w

use FileHandle;

sub parse_header {
    my ($f) = @_;
    my $d;
    my ($i,$where, $thisparm);

    $where = "";
    $d->{'identification'} = { 'author' => "(Unknown)",
			       'origin' => "(Unknown)",
			       'date' => "(Unknown)",
			       'version' => "(Unknown)",
			       'short'  => ""
			   };
    $d->{'description'} = undef;
    $d->{'parhelp'} = { };
    while(<$f>) {
	if(/\%I[a-z]*/i) {
	    $where = "identification";
	} elsif(/\%D[a-z]*/i) {
	    $where = "description";
	} elsif(/\%P[a-z]*/i) {
	    $where = "parameters";
	    undef $thisparm;
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
		    $d->{'identification'}{'version'} = $1;
		} else {
		    $d->{'identification'}{'short'} .= $_
			unless /^\s*$/;
		}
	    } elsif($where eq "description") {
		$d->{'description'} .= $_;
	    } elsif($where eq "parameters") {
		if(/^[ \t]*([a-zA-Z0-9Ê¯Â∆ÿ≈_]+):(.*)/) {
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
    if($s =~ /DEFINITION\sPARAMETERS\s+\(([a-zA-ZÊ¯Â∆ÿ≈0-9_, \t\r\n]+)\)/i) {
	@dpar = split (/\s*,\s*/, $1);
    } else {
	@dpar = ();
    }
    if($s =~ /SETTING\sPARAMETERS\s+\(([a-zA-ZÊ¯Â∆ÿ≈0-9_, \t\r\n]+)\)/i) {
	@spar = split (/\s*,\s*/, $1);
    } else {
	@spar = ();
    }
    @ipar = (@dpar, @spar);
    if($s =~ /OUTPUT\sPARAMETERS\s+\(([a-zA-ZÊ¯Â∆ÿ≈0-9_, \t\r\n]+)\)/i) {
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


sub show_header {
    my ($d) = @_;
    my ($i);
    print "######## Component: $d->{'name'} #####################\n";
    print "[Author]: $d->{'identification'}{'author'}\n";
    print "[Origin]: $d->{'identification'}{'origin'}\n";
    print "[Date]: $d->{'identification'}{'date'}\n";
    print "[Version]: $d->{'identification'}{'version'}\n";
    print "\n";
    print $d->{'identification'}{'short'};
    print "######## Input parameters: ##############################\n";
    for $i (@{$d->{'inputpar'}}) {
	print "<$i>: ";
	if($d->{'parhelp'}{$i}) {
	    print "[$d->{'parhelp'}{$i}{'unit'}] "
		if $d->{'parhelp'}{$i}{'unit'};
	    print "$d->{'parhelp'}{$i}{'text'}\n";
	} else {
	    print("<Undocumented>\n");
	}
    }
    print "######## Output parameters: #############################\n";
    for $i (@{$d->{'outputpar'}}) {
	print "<$i>: ";
	if($d->{'parhelp'}{$i}) {
	    print "[$d->{'parhelp'}{$i}{'unit'}] "
		if $d->{'parhelp'}{$i}{'unit'};
	    print "$d->{'parhelp'}{$i}{'text'}\n";
	} else {
	    print("<Undocumented>\n");
	}
    }
    if($d->{'description'}) {
	print "######## Description: ###################################\n";
	print $d->{'description'};
    }
    print "#########################################################\n";
}

#
# Output the start of the main component index HTML table
#
sub html_main_start {
    my ($f) = @_;
    print $f <<END;
<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 3.2//EN\">
<HTML>
<HEAD>
   <META NAME="GENERATOR" CONTENT="McDoc">
   <TITLE>McStas : Library components</TITLE>
</HEAD>
<BODY>

<CENTER><H1>Components for <i>McStas</i></H1></CENTER>

<TABLE BORDER COLS=5 WIDTH="100%" NOSAVE>
<TR>
<TD><B>Name</B></TD>
<TD WIDTH="10%"><B>Origin</B></TD>
<TD WIDTH="10%"><B>Author(s)</B></TD>
<TD WIDTH="10%"><B>Help</B></TD>
<TD><B>Description</B></TD>
</TR>
END
}

#
# Output the HTML table row describing component with information in
# $d to file handle $f.
#
sub html_table_entry {
    my ($d, $f, $bn) = @_;
    print $f "<TR>\n";
    print $f "<TD> ";
    print $f "<B>" if %{$d->{'parhelp'}};
    print $f "<A HREF=\"$bn.comp\">$d->{'name'}</A>";
    print $f "</B>" if %{$d->{'parhelp'}};
    print $f "</TD>\n";

    print $f "<TD>$d->{'identification'}{'origin'}</TD>\n";

    print $f "<TD>$d->{'identification'}{'author'}</TD>\n";

    print $f "<TD>";
    print $f "<A HREF=\"$bn.html\">More...</A>"
	if %{$d->{'parhelp'}} || $d->{'description'};
    print $f "</TD>\n";

    print $f "<TD>$d->{'identification'}{'short'}</TD>\n";
    print $f "</TR>\n\n";
}

#
# Output the end of the main component index HTML table
#
sub html_main_end {
    my ($f) = @_;
    my $date = `date +'%b %e %Y'`;
    print $f <<END;
</TABLE>

<P>This Component list was updated on $date.
<HR WIDTH="100%">
<CENTER>
  [ <A HREF="http://www.ill.fr/tas/mcstas/mcstas_ill.html"><I>McStas</I> at ILL</A>
  | <A href="http://neutron.risoe.dk/mcstas/"><I>McStas</I> at Ris&oslash;</A> ]
</CENTER>

<P><BR>
<ADDRESS>
Generated by McDoc, 
Maintained by Emmanuel Farhi &lt;<a href="mailto:farhi\@ill.fr">farhi\@ill.fr</a>>
and Kristian Nielsen &lt;<a href="mailto:kristian.nielsen\@risoe.dk">kristian.nielsen\@risoe.dk</a>>.
Contact us for any comments.
</ADDRESS>
</BODY></HTML>
END
}

#
# Output the HTML table for either input or output parameters.
#
sub gen_param_table {
    my ($f, $ps, $qs) = @_;
    my $i;
    # Avoid outputting empty table.
    unless(@$ps) {
	print $f "None.\n";
	return;
    }
    print $f "<TABLE BORDER=1>\n";
    print $f "<TR><TH>Name</TH>  <TH>Unit</TH>  <TH>Description</TH></TR>\n";
    for $i (@$ps) {
	print $f "<TR> <TD>$i</TD>\n";
	if($qs->{$i}) {
	    print $f "     <TD>$qs->{$i}{'unit'}</TD>\n";
	    print $f "     <TD>$qs->{$i}{'text'}</TD> </TR>\n";
	} else {
	    print $f "     <TD></TD> <TD></TD> </TR>\n";
	}
    }
    print $f "</TABLE>\n\n";
}

#
# Generate description web page from component with information in $d.
#
sub gen_html_description {
    my ($d, $bn) = @_;
    my $f = new FileHandle;
    open($f, ">$bn.html") || die "Cannot open $d->{'name'}.html";
    print $f "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 3.2//EN\">\n";
    print $f "<HTML><HEAD>\n";
    print $f "<TITLE>$d->{'name'}</TITLE>\n";
    print $f "<LINK REV=\"made\" HREF=\"mailto:kristian.nielsen\@risoe.dk\">\n";
    print $f "</HEAD>\n\n";
    print $f "<H1>The <CODE>$d->{'name'}</CODE> component</H1>\n\n";
    print $f "<BODY>\n\n";
    print $f "$d->{'identification'}{'short'}\n\n";
    print $f "<H2>Identification</H2>\n";
    print $f "\n<UL>\n";
    print $f "<LI> <B>Author:</B>$d->{'identification'}{'author'}</B>\n";
    print $f "<LI> <B>Origin:</B>$d->{'identification'}{'origin'}</B>\n";
    print $f "<LI> <B>Date:</B>$d->{'identification'}{'date'}</B>\n";
    print $f "<LI> <B>Version:</B>$d->{'identification'}{'version'}</B>\n";
    print $f "</UL>\n";
    print $f "\n<H2>Input parameters</H2>\n";
    gen_param_table($f, $d->{'inputpar'}, $d->{'parhelp'}); 
    print $f "\n<H2>Output parameters</H2>\n";
    gen_param_table($f, $d->{'outputpar'}, $d->{'parhelp'}); 
    if($d->{'description'}) {
	print $f "<H2>Description</H2>\n";
	print $f "\n<PRE>\n$d->{'description'}\n</PRE>\n";
    }
    print $f "\n<H2>Links</H2>\n\n<UL>\n";
    print $f "  <LI> <A HREF=\"$d->{'name'}.comp\">Source code</A> ";
    print $f "for <CODE>$d->{'name'}.comp</CODE>\n";
    # Additional links from component comment header go here.
    print $f "</UL>\n";
    print $f "\n<HR><ADDRESS>\n";
    print $f "Generated automatically by McDoc, Kristian Nielsen\n";
    print $f "&lt;<A HREF=\"mailto:kristian.nielsen\@risoe.dk\">";
    print $f   "kristian.nielsen\@risoe.dk</A>&gt; /\n";
    print $f `date +'%b %e %Y'`;
    print $f "</ADDRESS>\n";
    print $f "</BODY></HTML>\n";
    close $f;
}

#
# Add component with info in $d to web page handle $f, and generate
# stand-alone documentation page. $bn is the base name (file name
# without trailing .comp).
#
sub add_comp_html {
    my ($d, $f, $bn) = @_;
    html_table_entry($d, $f, $bn);
    gen_html_description($d, $bn);
}


my $comp;

my $indexfile = new FileHandle;
open($indexfile, ">index.html") ||
    die "Could not open index.html for writing.\n";
html_main_start($indexfile);
for $comp (@ARGV) {
    if($comp =~ /^(.*)\.(com|comp|cmp)$/) {
	my $basename = $1;
	my $file = new FileHandle;
	open($file, $comp)  || die "Could not open file '$comp'\n";
	my $data = parse_header($file);
	close($file);
	die "Parse of file '$comp' failed" unless $data;
	get_comp_info($comp, $data);
#	show_header($data);
	add_comp_html($data, $indexfile, $basename);
    } else {
	print STDERR "Warning: skipping bad name '$comp'\n";
    }
}
html_main_end($indexfile);
close($indexfile);
