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


sub show_header {
    my ($d) = @_;
    my ($i);
    print "######## Component: $d->{'name'} #####################\n";
    print "[Author]: $d->{'identification'}{'author'}\n";
    print "[Origin]: $d->{'identification'}{'origin'}\n";
    print "[Date]: $d->{'identification'}{'date'}\n";
    print "[Version]: $d->{'identification'}{'version'}\n";
    for $i (@{$d->{'identification'}{'history'}}) {
	print "[Modified by]: $i\n";
    }
    print "\n";
    print $d->{'identification'}{'short'};
    print "######## Input parameters: ##############################\n";
    for $i (@{$d->{'inputpar'}}) {
	if(defined($d->{'parhelp'}{$i}{'default'})) {
	    print "<$i=$d->{'parhelp'}{$i}{'default'}>: ";
	} else {
	    print "<$i>: ";
	}
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
    my ($f, $toolbar) = @_;
    print $f <<END;
<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 3.2//EN\">
<HTML>
<HEAD>
   <META NAME="GENERATOR" CONTENT="McDoc">
   <TITLE>McStas : Library components</TITLE>
</HEAD>
<BODY>

$toolbar
<CENTER><H1>Components for <i>McStas</i></H1></CENTER>

<P> Names in <B>Boldface</B> denote components that are properly
documented with comments in the source code.</P>
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
    print $f "<A HREF=\"$bn.html\">More...</A>";
    print $f "</TD>\n";

    print $f "<TD>$d->{'identification'}{'short'}</TD>\n";
    print $f "</TR>\n\n";
}

#
# Output the end of the main component index HTML table
#
sub html_main_end {
    my ($f, $toolbar) = @_;
    my $date = `date +'%b %e %Y'`;
    print $f <<END;
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
    print $f "<TR><TH>Name</TH>  <TH>Unit</TH>  <TH>Description</TH> <TH>Default</TH></TR>\n";
    for $i (@$ps) {
	my $default = $qs->{$i}{'default'};
	print $f "<TR> <TD>";
	print $f "<B>" unless defined($default);
	print $f "$i";
	print $f "</B>" unless defined($default);
	print $f "</TD>\n";
	if($qs->{$i}{'unit'} && $qs->{$i}{'text'}) {
	    print $f "     <TD>$qs->{$i}{'unit'}</TD>\n";
	    print $f "     <TD>$qs->{$i}{'text'}</TD>\n";
	} else {
	    print $f "     <TD></TD> <TD></TD>\n";
	}
	print $f "<TD ALIGN=RIGHT>", defined($default) ?
	    $default : "&nbsp;", "</TD> </TR>\n";
    }
    print $f "</TABLE>\n\n";
}

#
# Generate description web page from component with information in $d.
#
sub gen_html_description {
    my ($d, $bn) = @_;
    my $f = new FileHandle;
    my $toolbar = <<'TB_END';
<P ALIGN=CENTER>
 [ <A href="#id">Identification</A>
 | <A href="#desc">Description</A>
 | <A href="#ipar">Input parameters</A>
 | <A href="#opar">Output parameters</A>
 | <A href="#links">Links</A> ]
</P>
TB_END
    open($f, ">$bn.html") || die "Cannot open $d->{'name'}.html";
    print $f "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 3.2//EN\">\n";
    print $f "<HTML><HEAD>\n";
    print $f "<TITLE>$d->{'name'}</TITLE>\n";
    print $f "<LINK REV=\"made\" HREF=\"mailto:kristian.nielsen\@risoe.dk\">\n";
    print $f "</HEAD>\n\n";
    print $f "<BODY>\n\n$toolbar\n";
    print $f "<H1>The <CODE>$d->{'name'}</CODE> component</H1>\n\n";
    print $f "$d->{'identification'}{'short'}\n\n";
    print $f "<H2><A NAME=id></A>Identification</H2>\n";
    print $f "\n<UL>\n";
    print $f "  <LI> <B>Author:</B>$d->{'identification'}{'author'}</B>\n";
    print $f "  <LI> <B>Origin:</B>$d->{'identification'}{'origin'}</B>\n";
    print $f "  <LI> <B>Date:</B>$d->{'identification'}{'date'}</B>\n";
    print $f "  <LI> <B>Version:</B>$d->{'identification'}{'version'}</B>\n";
    if(@{$d->{'identification'}{'history'}}) {
	my $entry;
	print $f "  <LI> <B>Modification history:</B> <UL>\n";
	for $entry (@{$d->{'identification'}{'history'}}) {
	    print $f "    <LI> $entry\n";
	}
	print $f "  </UL>\n";
    }
    print $f "</UL>\n";
    if($d->{'description'}) {
	print $f "<H2><A NAME=desc></A>Description</H2>\n";
	print $f "\n<PRE>\n$d->{'description'}</PRE>\n";
    }
    print $f "\n<H2><A NAME=ipar></A>Input parameters</H2>\n";
    if(@{$d->{'inputpar'}}) {
	print $f "Parameters in <B>boldface</B> are required;\n";
	print $f "the others are optional.\n";
    }
    gen_param_table($f, $d->{'inputpar'}, $d->{'parhelp'}); 
    print $f "\n<H2><A NAME=opar></A>Output parameters</H2>\n";
    gen_param_table($f, $d->{'outputpar'}, $d->{'parhelp'}); 
    print $f "\n<H2><A NAME=links></A>Links</H2>\n\n<UL>\n";
    print $f "  <LI> <A HREF=\"$d->{'name'}.comp\">Source code</A> ";
    print $f "for <CODE>$d->{'name'}.comp</CODE>.\n";
    # Additional links from component comment header go here.
    my $link;
    for $link (@{$d->{'links'}}) {
	print $f "  <LI> $link";
    }
    print $f "</UL>\n";
    print $f "<HR>\n$toolbar\n<ADDRESS>\n";
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

#
# Add a whole section of components, given the section directory name.
#
sub add_comp_section_html {
    my ($sec, $header, $indexfile) = @_;
    if(opendir(DIR, $sec)) {
	my @comps = readdir(DIR);
	closedir DIR;
	return unless @comps;
	print $indexfile <<END;

<P><A NAME="$sec"></A>
$header
<TABLE BORDER COLS=5 WIDTH="100%" NOSAVE>
<TR>
<TD><B><I>Name</I></B></TD>
<TD WIDTH="10%"><B><I>Origin</I></B></TD>
<TD WIDTH="10%"><B><I>Author(s)</I></B></TD>
<TD WIDTH="10%"><B><I>Help</I></B></TD>
<TD><B><I>Description</I></B></TD>
</TR>
END
	my ($comp, $name);
	for $name (sort(@comps)) {
	    my $comp = "$sec/$name";
	    next unless $comp =~ /^(.*)\.(com|comp|cmp)$/;
	    my $basename = $1;
	    my $file = new FileHandle;
	    open($file, $comp)  || die "Could not open file '$comp'\n";
	    my $data = parse_header($file);
	    close($file);
	    die "Parse of file '$comp' failed" unless $data;
	    get_comp_info($comp, $data);
print "$comp\n";
#show_header($data);
	    add_comp_html($data, $indexfile, $basename);
	}
	print $indexfile <<END;
</TABLE>

END
    }
}

my $comp;

my $indexfile = new FileHandle;
open($indexfile, ">index.html") ||
    die "Could not open index.html for writing.\n";
my @sections = ("sources", "optics", "samples", "monitors", "misc");
my %section_headers =
    ("sources" => '<B><FONT COLOR="#FF0000">Sources</FONT></B>',
     "optics" => '<B><FONT COLOR="#FF0000">Optics</FONT></B>',
     "samples" => '<B><FONT COLOR="#FF0000">Samples</FONT></B>',
     "monitors" => '<B><FONT COLOR="#FF0000">Detectors</FONT> and monitors</B>',
     "misc" => '<B><FONT COLOR="#FF0000">Misc</FONT></B>');
my @tblist = map "<A href=\"#$_\">$_</A>", @sections;
my $toolbar = "<P ALIGN=CENTER>\n [ " . join("\n | ", @tblist) . " ]\n</P>\n";
html_main_start($indexfile, $toolbar);
my $sec;
for $sec (@sections) {
    add_comp_section_html($sec, $section_headers{$sec}, $indexfile);
}
html_main_end($indexfile, $toolbar);
close($indexfile);
