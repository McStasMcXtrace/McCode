#! /usr/bin/perl

# Install a handler for the interrupt signal (CTRL-C) that simply exits the
# program cleanly (this removes tmpfiles correctly).
$SIG{INT} = sub { exit };

# Determine the path to the McStas system directory. This must be done
# in the BEGIN block so that it can be used in a "use lib" statement
# afterwards.

use Config;
use Cwd;
BEGIN {
    ENV_HEADER
}

use lib $MCSTAS::perl_dir;
require "mccode_config.perl";

# Overload with user's personal config
if ($ENV{"HOME"} && -e $ENV{"HOME"}."/.".$MCSTAS::mcstas_config{'MCCODE'}."/".$MCSTAS::mcstas_config{'VERSION'}."/mccode_config.perl") {
  print "$0: reading local $MCSTAS::mcstas_config{'MCCODE'} configuration from " . $ENV{"HOME"}."/.".$MCSTAS::mcstas_config{'MCCODE'}."/".$MCSTAS::mcstas_config{'VERSION'}."/mccode_config.perl\n";
  require $ENV{"HOME"}."/.".$MCSTAS::mcstas_config{'MCCODE'}."/".$MCSTAS::mcstas_config{'VERSION'}."/mccode_config.perl";
}

use FileHandle;
use File::Basename;
require "mcrunlib.pl";

my $is_single_file= 0;  # true when doc requested for a single component
my $is_user_lib   = 0;  # true when doc requested for a directory
my $lib_dir       = $MCSTAS::sys_dir;
our $out_file      = ""; # default name for output of catalog will be index
my $use_local     = 0;  # true when also looking into current path
my $single_comp_name = 0;  # component single name
my $browser       = $MCSTAS::mcstas_config{'BROWSER'};
my $is_forced     = 0; # true when force re-writing of existing HTML
my $is_exact      = 0; # true when used for generating TeX docs from comp headers, i.e. we KNOW there is a comp like this
my @valid_names; # Full list of possible comp matches
my $HTTP_SYSDIR = $MCSTAS::sys_dir; 
if (!($Config{'osname'} eq 'MSWin32')) {
# Many Unix browsers need a file:// to properly access local files
    $HTTP_SYSDIR="file://".$HTTP_SYSDIR;
}
$HTTP_SYSDIR =~ s!\\!/!g;

sub show_header { # output in text mode
    my ($d) = @_;
    my ($i);
    print "######## $d->{'type'}: $d->{'name'} #####################\n";
    if ($d->{'type'} eq "Instrument") {
      print "[Site]:   $d->{'site'}\n";
    }
    print "[Author]: $d->{'identification'}{'author'}\n";
    print "[Origin]: $d->{'identification'}{'origin'}\n";
    print "[Date]:   $d->{'identification'}{'date'}\n";
    print "[Version]:$d->{'identification'}{'version'}\n";
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
            print "$d->{'parhelp'}{$i}{'text'}"
                if $d->{'parhelp'}{$i}{'text'};  # text finishes by \n
            print("\n");
        } else {
            print("<Undocumented>\n");
        }
    }
    if (@{$d->{'outputpar'}}) {
      print "\n######## Output parameters: #############################\n";
      for $i (@{$d->{'outputpar'}}) {
          print "<$i>: ";
          if($d->{'parhelp'}{$i}) {
              print "[$d->{'parhelp'}{$i}{'unit'}] "
                  if $d->{'parhelp'}{$i}{'unit'};
              print "$d->{'parhelp'}{$i}{'text'}"
                  if $d->{'parhelp'}{$i}{'text'};  # text finishes by \n
              print("\n");
          } else {
              print("<Undocumented>\n");
          }
      }
    }
    if($d->{'description'}) {
        print "\n######## Description: ###################################\n";
        print $d->{'description'};
    }
    print "\n#########################################################\n";
}

#
# Output the start of the main component index HTML table
# parameters: ($filehandle, $toolbar);
sub html_main_start {
    my ($f, $toolbar) = @_;
    print $f <<END;
<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 3.2//EN\">
<HTML>
<HEAD>
   <META NAME="GENERATOR" CONTENT="McDoc">
   <TITLE>$MCSTAS::mcstas_config{'PKGNAME'} : Components/Instruments Library </TITLE>
</HEAD>
<BODY>

$toolbar
<CENTER><H1>Components and Instruments from the Library for <i>$MCSTAS::mcstas_config{'PKGNAME'}</i></H1></CENTER>

<P> Names in <B>Boldface</B> denote components that are properly
documented with comments in the source code.</P>
END
}

#
# Output the HTML table row describing component with information in
# $d to file handle $f.
# parameters: ($data, $filehandle, $basename)
sub html_table_entry {
    my ($d, $f, $bn, $vn) = @_;
    print $f "<TR>\n";
    print $f "<TD> ";
    print $f "<B>" if %{$d->{'parhelp'}};
    my $link = "file://".$vn;
    $link =~ s!\\!/!g;

    if ($d->{'type'} eq "Instrument") {
      print $f "$d->{'site'} <A HREF=\"$link.html\">$d->{'name'}</A> ($d->{'path'})";
    } else {
      print $f "<A HREF=\"$link.html\">$d->{'name'}</A>";
    }
    print $f "</B>" if %{$d->{'parhelp'}};
    print $f "</TD>\n";

    print $f "<TD>$d->{'identification'}{'origin'}</TD>\n";

    print $f "<TD>$d->{'identification'}{'author'}</TD>\n";

    $link = "file://".$bn;
    $link =~ s!\\!/!g;

    print $f "<TD>";
    print $f "<A HREF=\"$link.$d->{'ext'}\">$d->{'ext'}</A>";
    print $f "</TD>\n";

    print $f "<TD>$d->{'identification'}{'short'}</TD>\n";
    print $f "</TR>\n\n";
}

#
# Output the end of the main component index HTML table
# parameters: ($filehandle, $toolbar);
sub html_main_end {
    my ($f, $toolbar) = @_;
    my $date = gmtime;

    print $f <<END;
<P>This Component list was updated on $date.
<HR WIDTH="100%">
<CENTER>
END
    print $f "[ <A href=\"http://www.$MCSTAS::mcstas_config{'MCCODE'}.org/\"><I>$MCSTAS::mcstas_config{'PKGNAME'} web site</I></A> ]";
    print $f <<END;
</CENTER>

<P><BR>
<ADDRESS>
Generated by $MCSTAS::mcstas_config{'DOCCMD'},
Maintained by Emmanuel Farhi &lt;<a href="mailto:farhi\@ill.fr">farhi\@ill.fr</a>>,
Peter Willendrup &lt;<a href="mailto:peter.willendrup\@risoe.dk">peter.willendrup\@risoe.dk</a>>,
and Erik B Knudsen &lt;<a href="mailto:erkn\@fysik.dtu.dk">erkn\@fysik.dtu.dk</a>>.
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
# parameters: ($data, $basename, $name);
sub gen_html_description {
    my ($d, $bn, $n) = @_;
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
    my $is_opened = 0;
    my $valid_name= "";
    $n=~ s|.comp\Z||; # remove trailing extension
    $n=~ s|.cmp\Z||; # remove trailing extension
    $n=~ s|.com\Z||; # remove trailing extension
    $n=~ s|.instr\Z||; # remove trailing extension
    $valid_name = $bn;
    if (open($f, ">$bn.html")) { # use component location
      $is_opened = 1;
    }
    if (((not $is_opened) && $is_forced) || (not -f "$valid_name.html")) {
      if (open($f, ">$n.html")) { # create locally
        $is_opened = 1;
        $valid_name = $n;
      }
    }

    if ($is_single_file) {
      $out_file = "$valid_name.html";
      push @valid_names, $out_file;
    }

    if ($is_opened) {

      print $f "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 3.2//EN\">\n";
      print $f "<HTML><HEAD>\n";
      if ($d->{'type'} eq "Instrument") {
        print $f "<TITLE>$MCSTAS::mcstas_config{'PKGNAME'}: $d->{'name'} $d->{'type'} at $d->{'site'}</TITLE>\n";
      } else {
        print $f "<TITLE>$MCSTAS::mcstas_config{'PKGNAME'}: $d->{'name'} $d->{'type'}</TITLE>\n";
      }
      print $f "<LINK REV=\"made\" HREF=\"mailto:peter.willendrup\@risoe.dk\">\n";
      print $f "</HEAD>\n\n";
      print $f "<BODY>\n\n$toolbar\n";
      print $f "<H1>The <CODE>$d->{'name'}</CODE> $d->{'type'}</H1>\n\n";
      print $f "$d->{'identification'}{'short'}\n\n";
      print $f "<H2><A NAME=id></A>Identification</H2>\n";
      print $f "\n<UL>\n";
      if ($d->{'type'} eq "Instrument") {
        print $f "  <LI> <B>Site:   $d->{'site'}</B>\n";
      }
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
          if ($bn =~ m/obsolete/i || $n =~ m/obsolete/i) {
            print $f "WARNING: <B>This is an obsolete $d->{'type'}.";
            print $f "Please avoid usage whenever possible.</B>\n";
          }
          if ($bn =~ m/contrib/i || $n =~ m/contrib/i) {
            print $f "WARNING: <B>This is a contributed $d->{'type'}.</B>\n";
          }
      }
      print $f "\n<H2><A NAME=ipar></A>Input parameters</H2>\n";
      if(@{$d->{'inputpar'}}) {
          print $f "Parameters in <B>boldface</B> are required;\n";
          print $f "the others are optional.\n";
      }
      gen_param_table($f, $d->{'inputpar'}, $d->{'parhelp'});
      if (@{$d->{'outputpar'}}) {
        print $f "\n<H2><A NAME=opar></A>Output parameters</H2>\n";
        gen_param_table($f, $d->{'outputpar'}, $d->{'parhelp'});
      }
      print $f "\n<H2><A NAME=links></A>Links</H2>\n\n<UL>\n";

      print $f "  <LI> <A HREF=\"$d->{'path'}\">Source code</A> ";
      print $f "for <CODE>$d->{'name'}.$d->{'ext'}</CODE>.\n";
      # Additional links from component comment header go here.
      my $link;
      for $link (@{$d->{'links'}}) {
          print $f "  <LI> $link";
      }
      print $f "</UL>\n";
      print $f "<HR>\n$toolbar\n<ADDRESS>\n";
      print $f "Generated automatically by McDoc, Peter Willendrup\n";
      print $f "&lt;<A HREF=\"mailto:peter.willendrup\@risoe.dk\">";
      print $f   "peter.willendrup\@risoe.dk</A>&gt; /\n";
      my $date = gmtime;
      print $f "$date";
      print $f "</ADDRESS>\n";
      print $f "</BODY></HTML>\n";
      close $f;
    } else {
      if (not -f "$valid_name.html") {
        print "$MCSTAS::mcstas_config{'DOCCMD'}: Cannot open $valid_name.html. Use -f option to force.\n";
      }
    }
    return $valid_name;
}

#
# Add component with info in $d to web page handle $f, and generate
# stand-alone documentation page. $bn is the base name (file name
# without trailing .comp).
# parameters: ($data, $filehandle, $basename, $name);
sub add_comp_html {
    my ($d, $f, $bn, $n) = @_;
    my $vn;
    $vn = gen_html_description($d, $bn, $n);
    if ($f) { html_table_entry($d, $f, $bn, $vn); }
}

#
# Add a whole section of components, given the section directory name.
# parameters: ($lib_dir, $section, $section_header, $filehandle);
sub add_comp_section_html {
    my ($lib, $sec, $header, $filehandle) = @_;
    my $sec_orig = $sec;
    if ($sec =~ "local") { $sec = getcwd(); $single_comp_name = basename($single_comp_name); $is_forced=1; }  # local components
    $sec = "$lib/$sec" unless -d $sec;
    if(opendir(DIR, $sec)) {
        my @comps = readdir(DIR);
        closedir DIR;
	if ($is_exact) {
          # test if the given comp/instr name is an actual file name
          if (-f "$sec/$single_comp_name") {
            @comps = ($single_comp_name);
          } else {
	    @comps = ();
	  }
        }
        if ($is_forced) {
          # test if the given comp/instr name is an actual file name
          if (-f "$single_comp_name") {
            push @comps, $single_comp_name;
          }
        }
        return unless @comps;
        if ($filehandle) {
          print $filehandle <<END;

<P><A NAME="$sec_orig"></A>
$header
<TABLE BORDER COLS=5 WIDTH="100%" NOSAVE>
<TR>
<TD><B><I>Name</I></B></TD>
<TD WIDTH="10%"><B><I>Origin</I></B></TD>
<TD WIDTH="10%"><B><I>Author(s)</I></B></TD>
<TD><B><I>Source code</I></B></TD>
<TD><B><I>Description</I></B></TD>
</TR>
END
        } # end if filehandle
        my ($comp, $name);
        my $single_comp_name_base;
        # extract the requested comp/instr name to look for, removing possible path

        $single_comp_name_base= lc basename($single_comp_name);
	
        for $name (sort(@comps)) {
            my $comp = "$sec/$name";
            my $does_match = 0;
            my $name_base;
            my $basename;
            next if (-d "$name"); # skip directories
            # extract the scanned comp/instr name from lib, removing possible path
            $name_base = basename($name);  # with extension
            if ((lc $name_base) =~ (lc $single_comp_name_base)) {
	      # requested doc name does not contain an extension: search all matches
              $does_match = 1;
            }
            # skip non comp/instr
            if ($comp !~ /^(.*)\.(com|comp|cmp|instr)$/)
            {
              if ($comp !~ /^(.*)\.(htm|html)$/ && $does_match) {
                print STDOUT "$0: $comp (not a component/instrument)\n";
              }
              next
            } else { $basename = $1; } # without extension
            if (($is_single_file && $does_match)
              || (not $is_single_file)) {
              $data = component_information($comp);
              if (not defined($data)) {
                print STDERR "$0: Failed to get information for component/instrument '$comp'";
              } else {
                print STDOUT "$0: $comp\n";
                if ($is_single_file) { $data->{'path'} = $comp; }
                else { $data->{'path'} = $name; }
                if ($is_single_file && $browser =~ "text") {
                  show_header($data); # display single comp as text
                  if ($sec =~ m/obsolete/i) {
                    print "WARNING: This is an obsolete $data->{'type'}. \n";
                    print "         Please avoid usage whenever possible.\n";
                  }
                  if ($sec =~ m/contrib/i) {
                    print "WARNING: This is a contributed $data->{'type'}. \n";
                  }
                } else {
                  add_comp_html($data, $filehandle, $basename, $name);
                }
              }

            }
	    # This should in fact never happen
	    last if $does_match == 2;
        } # end for
        if ($filehandle) {
          print $filehandle <<END;
</TABLE>

END
        } # end if filehandle
    } #end if open DIR
}

#
# Add a search of components
sub add_comp_search_html {
    my ($search, $filehandle, @Incomps) = @_;
    my @comps;
    my ($j, $sec, $comp, $inst, $suf);
    for ($j=0; $j<@Incomps; $j++) {
	($comp,$sec,$suf) = fileparse($Incomps[$j],".html");
	print "Testing for $comp\n";
	$inst = "$sec$comp.instr";
	$comp = "$sec$comp.comp";
	if (-f "$comp") {
	    push @comps, $comp;
	} elsif (-f "$inst") {
	    push @comps, $inst;
	}
    }
    return unless @comps;
    if ($filehandle) {
      my $pwd = getcwd();
      print $filehandle <<END;
<html>
<header><title>$MCSTAS::mcstas_config{'DOCCMD'}: Search result for "$search"</title></header>
<body>
<h1>Result of search for "$search" in your $MCSTAS::mcstas_config{'PKGNAME'} library</h1>
<p>(Please note that only current dir $pwd and $lib_dir were searched, discarding 'obsolete' components)
<p><TABLE BORDER COLS=5 WIDTH="100%" NOSAVE>
<TR>
<TD><B><I>Name</I></B></TD>
<TD WIDTH="10%"><B><I>Origin</I></B></TD>
<TD WIDTH="10%"><B><I>Author(s)</I></B></TD>
<TD><B><I>Source code</I></B></TD>
<TD><B><I>Description</I></B></TD>
</TR>
END
    } else {
      print "Could not write $comp to search output file ('$search').\n";
    }

    my $name;
    for $comp (sort(@comps)) {
	# extract the scanned comp/instr name from lib, removing possible path
	if ($comp =~ m/comp/i) {
	    ($name, $sec, $suf) = fileparse($comp, ".comp");  # without extension
	} else {
	    ($name, $sec, $suf) = fileparse($comp, ".instr");  # without extension
	}

	$data = component_information($comp);
	$data->{'path'} = $sec;
	if (not defined($data)) {
	    print STDERR "$0: Failed to get information for component/instrument '$comp'";
	} else {
	    print STDOUT "$0: Search page adding $comp\n";
	}
	if (!($sec =~ m/obsolete/i)) {
	    if ($sec =~ m/contrib/i) {
		print "WARNING: This is a contributed $data->{'type'}. \n";
	    }
	    add_comp_html($data, $filehandle, "$sec/$name", $comp);
	}
    }
    if ($filehandle) {
	print $filehandle <<END;
</TABLE>
END
    }
}

# Start of main ===============================

my $index         = 0;
my $file;
my $show_website  = 0;
my $show_manual   = 0;
my $show_compman  = 0;

for($i = 0; $i < @ARGV; $i++) {
  $_ = $ARGV[$i];
  # Options specific to mcdoc.
  if(/^--show$/i || /^-s$/i || /^--html$/i) {
        $browser = $MCSTAS::mcstas_config{'BROWSER'};
  } elsif(/^--text$/i || /^-t$/i) {
        $browser = "text";
  } elsif(/^--web$/i || /^-w$/i) {
        $show_website = 1;
  } elsif(/^--manual$/i || /^-m$/i) {
        $show_manual = 1;
  } elsif(/^--comp$/i || /^-c$/i) {
        $show_compman = 1;
  } elsif(/^--local$/i) {
        $use_local = 1;
  } elsif(/^--force$/i || /^-f$/i) {
        $is_forced = 1;
  } elsif(/^--exact$/i || /^-e$/i) {
        $is_exact = 1;
  } elsif(/^--out-file\=(.*)$/ || /^-o(.+)$/) {
        $out_file = $1;
  } elsif(/^--help$/i || /^-h$/i || /^-v$/i) {
      my $pkgname=$MCSTAS::mcstas_config{'PKGNAME'};
      print "Usage: $0 [options] <dir|file>\n";
      print "Generate/show component/instrument documentation using $browser\n";
      print "   -f    --force    Force re-writing of existing HTML doc locally\n";
      print "   -e    --exact    Do docs for a comp we KNOW exist, i.e. for TeX manual\n";
      print "   -h    --help     Show this help\n";
      print "   -l    --tools    Display the $pkgname tools list\n";
      print "   -m    --manual   Open the $pkgname User manual\n";
      print "   -c    --comp     Open the $pkgname Component manual\n";
      print "   -t    --text     For single component, display as text\n";
      print "   -w    --web      Open the $pkgname web page http://www." . lc($pkgname) . ".org/\n";
      if($pkgname=~/^mcstas/i){
        print "SEE ALSO: mcstas, mcdoc, mcplot, mcrun, mcgui, mcresplot, mcstas2vitess\n";
        print "DOC:      Please visit http://www.mcstas.org/\n";
      }elsif($pkgname=~/^mcxtrace/i){
        print "SEE ALSO: mcxtrace, mxdoc, mxplot, mcrun, mxgui\n";
        print "DOC:      Please visit http://www.mcxtrace.org/\n";
      }
      exit;
  } elsif(/^--tools$/i || /^-l$/) {
      $_=$MCSTAS::mcstas_config{'MCCODE'};
      if(/^mcstas/){
        print "$MCSTAS::mcstas_config{'PKGNAME'} Tools\n";
        print "   mcstas        Main instrument compiler\n";
        print "   mcrun         Instrument maker and execution utility\n";
        print "   mcgui         Graphical User Interface instrument builder\n";
        print "   mcdoc         Component library documentation generator/viewer\n";
        print "   mcplot        Simulation result viewer\n";
        print "   mcdisplay     Instrument geometry viewer\n";
        print "   mcresplot     Instrument resolution function viewer\n";
        print "   mcstas2vitess McStas to Vitess component translation utility\n";
        print "   mcformat      Conversion tool for text files and MPI/grids\n";
        print "   mcformatgui   GUI for mcformat\n";
        print "   mcdaemon      Instrument results on-line plotting\n";
        print "When used with the -h flag, all tools display a specific help.\n";
        print "SEE ALSO: mcstas, mcdoc, mcplot, mcrun, mcgui, mcresplot, mcstas2vitess\n";
        print "DOC:      Please visit http://www.mcstas.org/\n";
        exit;
      }elsif(/^mcxtrace/){
        print "$MCSTAS::mcstas_config{'PKGNAME'} Tools\n";
        print "   mcxtrace        Main instrument compiler\n";
        print "   mxrun         Instrument maker and execution utility\n";
        print "   mxgui         Graphical User Interface instrument builder\n";
        print "   mxdoc         Component library documentation generator/viewer\n";
        print "   mxplot        Simulation result viewer\n";
        print "   mxdisplay     Instrument geometry viewer\n";
        print "   mxformat      Conversion tool for text files and MPI/grids\n";
        print "   mxdaemon      Instrument results on-line plotting\n";
        print "When used with the -h flag, all tools display a specific help.\n";
        print "SEE ALSO: mcxtrace, mxdoc, mxplot, mxrun, mxgui\n";
        print "DOC:      Please visit http://www.mcxtrace.org/\n";
        exit;
      }
  } else {
      $file = $ARGV[$i];
      $index++;
  }
} # end for

require File::Temp;
if ($out_file eq "" && $browser ne "text") {
  ($fh, $out_file) = File::Temp::tempfile("mccode_doc_tmpXXXXXX", SUFFIX => '.html', UNLINK => 1);
  if (not defined $fh) { $out_file=""; }
}
if ($out_file eq "" && $browser ne "text") { $out_file="index.html"; }

if ($show_website) {
  # open the web site
  my $cmd = "$MCSTAS::mcstas_config{'BROWSER'} http://www.$MCSTAS::mcstas_config{'MCCODE'}.org/ ";
  print "$0: Starting $cmd\n"; system("$cmd\n");
  die "$0: web site done.\n";
}

if ($show_manual) {
  # open the manual using embedded acroread plugin
  $cmd = "$MCSTAS::mcstas_config{'BROWSER'} $HTTP_SYSDIR/doc/manuals/$MCSTAS::mcstas_config{'MCCODE'}-manual.pdf";
  print "$0: Starting $cmd\n"; system("$cmd\n");
  die "$0: User manual done.\n";
}

if ($show_compman) {
  # open the component manual
  $cmd = "$MCSTAS::mcstas_config{'BROWSER'} $HTTP_SYSDIR/doc/manuals/$MCSTAS::mcstas_config{'MCCODE'}-components.pdf";
  print "$0: Starting $cmd\n"; system("$cmd\n");
  die "$0: Component manual done.\n";
}

# if 'file' is given
if ($index > 0) {
  if (-d $file) { $lib_dir = $file; } # get doc of the given dir
  else { $is_single_file=1; $single_comp_name = $file; }  # search locally and in lib
  $use_local=1; # will also search locally
}

my $filehandle = 0;
my @sections;
my %section_headers;

if ((not $is_single_file) && ($browser ne "text") && ($out_file ne "")) {
  # Open the local documentation file
  $filehandle = new FileHandle;
  my $no_lib_write  = 0;

  if (not open($filehandle, ">$lib_dir/$out_file")) { $no_lib_write = 1; }
  if ($no_lib_write) {
    my $no_local_write = 0;
    if (not open($filehandle, ">$out_file")) { $no_local_write = 1; }
    if ($no_local_write) {
      $filehandle = 0; # will not write the catalog
      print STDERR "$0: Could not open $out_file for writing.\n";
    } else {
      print "$0: Opening $out_file\n";
    }
  } else {
    $out_file = "$lib_dir/$out_file";
  }
  if (not $filehandle) {
    if (-f "$lib_dir/$out_file") {
      $out_file = "$lib_dir/$out_file";
    }
    elsif (not -f $out_file) {
      print STDERR "$0: Could not find the $out_file library catalog.\n";
    }
  }
}

if ($use_local) {
  # define local and lib sections
  @sections = ("sources", "optics", "samples", "monitors",
               "misc", "contrib", "union","astrox","obsolete","examples","local","data","share","doc");
  %section_headers =
    ("sources" => '<B><FONT COLOR="#FF0000">Sources</FONT></B>',
     "optics" => '<B><FONT COLOR="#FF0000">Optics</FONT></B>',
     "samples" => '<B><FONT COLOR="#FF0000">Samples</FONT></B>',
     "monitors" => '<B><FONT COLOR="#FF0000">Detectors</FONT> and monitors</B>',
     "contrib" => '<B><FONT COLOR="#FF0000">Contributed</FONT> components</B>',
     "union" => '<B><FONT COLOR="#FF0000">Union</FONT> components</B>',
     "astrox" => '<B><FONT COLOR="#FF0000">AstroX</FONT> components</B>',
     "misc" => '<B><FONT COLOR="#FF0000">Misc</FONT></B>',
     "obsolete" => '<B><FONT COLOR="#FF0000">Obsolete</FONT> (avoid usage whenever possible)</B>',
     "examples" => '<B><FONT COLOR="#FF0000">Instrument Examples</FONT></B>',
     "local" => '<B><FONT COLOR="#FF0000">Local components</FONT></B>',
     "data" => '<B><FONT COLOR="#FF0000">Data files</FONT></B>',
     "share" => '<B><FONT COLOR="#FF0000">Shared libraries</FONT></B>',
     "doc"   => '<B><FONT COLOR="#FF0000">Documentation</FONT></B>');
} else {
  # define lib sections
  @sections = ("sources", "optics", "samples", "monitors", "misc", "contrib", "union", "astrox", "examples");
  %section_headers =
    ("sources" => '<B><FONT COLOR="#FF0000">Sources</FONT></B>',
     "optics" => '<B><FONT COLOR="#FF0000">Optics</FONT></B>',
     "samples" => '<B><FONT COLOR="#FF0000">Samples</FONT></B>',
     "monitors" => '<B><FONT COLOR="#FF0000">Detectors</FONT> and monitors</B>',
     "contrib" => '<B><FONT COLOR="#FF0000">Contributed</FONT> components</B>',
     "union" => '<B><FONT COLOR="#FF0000">Union</FONT> components</B>',
     "astrox" => '<B><FONT COLOR="#FF0000">AstroX</FONT> components</B>',
     "misc" => '<B><FONT COLOR="#FF0000">Misc</FONT></B>',
     "obsolete" => '<B><FONT COLOR="#FF0000">Obsolete</FONT> (avoid usage whenever possible)</B>',
     "examples" => '<B><FONT COLOR="#FF0000">Instrument Examples</FONT></B>',);
}
my @tblist = map "<A href=\"#$_\">$_</A>", @sections;
my $toolbar = "<P ALIGN=CENTER>\n [ " . join("\n | ", @tblist) . " ]\n</P>\n";
$toolbar .= "<P ALIGN=CENTER>\n [ <a href=\"$HTTP_SYSDIR/doc/manuals/$MCSTAS::mcstas_config{'MCCODE'}-manual.pdf\">User Manual</a>
| <a href=\"$HTTP_SYSDIR/doc/manuals/$MCSTAS::mcstas_config{'MCCODE'}-components.pdf\">Component Manual</a>
| <a href=\"$HTTP_SYSDIR/data\">Data files</a> | <a href=\"$HTTP_SYSDIR\">$lib_dir</a> ]\n</P>\n";

if ($filehandle) {
  html_main_start($filehandle, $toolbar);
}
# open each section, look for comps, add entry in index.html,
# and generate comp doc
my $sec;
my $is_forced_orig = $is_forced;
for $sec (@sections) {
    add_comp_section_html($lib_dir, $sec, $section_headers{$sec}, $filehandle);
    $is_forced = $is_forced_orig; # may have been changed globally (sec == local)
}
if ($filehandle) {
  html_main_end($filehandle, $toolbar);
  close($filehandle);
}

if (-f $out_file) {
    if ($browser ne "text") {
      # In case of multiple matches, create table of results:
      if (@valid_names > 1) {
          require File::Temp;
          my $searchfile;
          ($filehandle, $searchfile) = File::Temp::tempfile("mccode_doc_tmpXXXXXX", SUFFIX => '.html', UNLINK => 1);
          open($filehandle, ">$searchfile") || die "Could not write to search output file\n";
          add_comp_search_html($file, $filehandle, @valid_names);
          html_main_end($filehandle, $toolbar);
          close($filehandle);
          $out_file = "$searchfile";
      }

      # open the index.html
      my $cmd = "$MCSTAS::mcstas_config{'BROWSER'} $out_file";
      print "$0: Starting $cmd\n";
      system("$cmd\n");
      # wait for browser tab to open (when browsers open page in current window)
      sleep(10);
    }
}
