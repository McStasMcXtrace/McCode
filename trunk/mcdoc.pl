#! /usr/bin/perl -w

# Determine the path to the McStas system directory. This must be done
# in the BEGIN block so that it can be used in a "use lib" statement
# afterwards.

use Config;
use Cwd;
BEGIN {
  # default configuration (for all high level perl scripts)
  if($ENV{"MCSTAS"}) {
    $MCSTAS::sys_dir = $ENV{"MCSTAS"};
  } else {
    if ($Config{'osname'} eq 'MSWin32') {
      $MCSTAS::sys_dir = "c:\\mcstas\\lib";
    } else {
      $MCSTAS::sys_dir = "/usr/local/lib/mcstas";
    }
  }
  $MCSTAS::perl_dir = "$MCSTAS::sys_dir/tools/perl";
}

use lib $MCSTAS::perl_dir;
require "mcstas_config.perl";

use FileHandle;
use File::Basename;
require "mcrunlib.pl";

my $is_single_file= 0;  # true when doc requested for a single component
my $is_user_lib   = 0;  # true when doc requested for a directory
my $lib_dir       = $MCSTAS::sys_dir; 
my $out_file      = "index.html"; # default name for output of catalog
my $use_local     = 0;  # true when also looking into current path
my $single_comp_name = 0;  # component single name
my $browser       = $MCSTAS::mcstas_config{'BROWSER'};
my $is_forced     = 0; # true when force re-writting of existing HTML

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
   <TITLE>McStas : Components/Instruments Library </TITLE>
</HEAD>
<BODY>

$toolbar
<CENTER><H1>Components and Instruments from the Library for <i>McStas</i></H1></CENTER>

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
    if ($d->{'type'} eq "Instrument") {
      print $f "$d->{'site'} <A HREF=\"$bn.$d->{'ext'}\">$d->{'name'}</A> ($d->{'path'})";
    } else {
      print $f "<A HREF=\"$bn.$d->{'ext'}\">$d->{'name'}</A>";
    }
    print $f "</B>" if %{$d->{'parhelp'}};
    print $f "</TD>\n";

    print $f "<TD>$d->{'identification'}{'origin'}</TD>\n";

    print $f "<TD>$d->{'identification'}{'author'}</TD>\n";

    print $f "<TD>";
    print $f "<A HREF=\"$vn.html\">More...</A>";
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
  [ <A HREF="http://www.ill.fr/tas/mcstas/"><I>McStas</I> at ILL</A>
  | <A href="http://mcstas.risoe.dk/"><I>McStas</I> at Ris&oslash;</A> ]
</CENTER>

<P><BR>
<ADDRESS>
Generated by McDoc, 
Maintained by Emmanuel Farhi &lt;<a href="mailto:farhi\@ill.fr">farhi\@ill.fr</a>>
and Peter Willendrup &lt;<a href="mailto:peter.willendrup\@risoe.dk">peter.willendrup\@risoe.dk</a>>.
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
    }
    
    if ($is_opened) {
      
      print $f "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 3.2//EN\">\n";
      print $f "<HTML><HEAD>\n";
      if ($d->{'type'} eq "Instrument") {
        print $f "<TITLE>McStas: $d->{'name'} $d->{'type'} at $d->{'site'}</TITLE>\n";
      } else {
        print $f "<TITLE>McStas: $d->{'name'} $d->{'type'}</TITLE>\n";
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
        print "mcdoc: Cannot open $valid_name.html. Use -f option to force.\n";
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
<TD WIDTH="10%"><B><I>Help</I></B></TD>
<TD><B><I>Description</I></B></TD>
</TR>
END
        } # end if filehandle
        my ($comp, $name);
        my $single_comp_name_base;
        # extract the requested comp/instr name to look for, removing possible path
        $single_comp_name_base= basename($single_comp_name);
        
        for $name (sort(@comps)) {
            my $comp = "$sec/$name";
            my $does_match = 0;
            my $name_base;
            my $basename;
            next if (-d "$name"); # skip directories
            # extract the scanned comp/instr name from lib, removing possible path
            $name_base = basename($name);  # with extension
            if ($single_comp_name_base =~ /^(.*)\.(com|comp|cmp|instr)$/) {
              # requested doc name includes extension: search exact match
              if($name_base =~ $single_comp_name_base) {
                $does_match = 2; 
              }
            } elsif ($name_base =~ $single_comp_name_base) {
              # requested doc name does not contain an extension: search all matches
              $does_match = 1; 
            }
            # skip non comp/instr
            if ($comp !~ /^(.*)\.(com|comp|cmp|instr)$/)
            { 
              if ($comp !~ /^(.*)\.(htm|html)$/ && $does_match) {
                print STDOUT "mcdoc: $comp (not a component/instrument)\n";
              }
              next 
            } else { $basename = $1; } # without extension 
            if (($is_single_file && $does_match) 
              || (not $is_single_file)) {
              $data = component_information($comp);
              if (not defined($data)) {
                print STDERR "mcdoc: Failed to get information for component/instrument '$comp'";
              } else {
                print STDOUT "mcdoc: $comp\n";
                if ($is_single_file) { $data->{'path'} = $comp; }
                else { $data->{'path'} = $name; }
                if ($is_single_file && $browser =~ "text") {
                  show_header($data); # display single comp as text
                  if ($sec =~ m/obsolete/i) {
                    print "WARNING: This is an obsolete $data->{'type'}. \n";
                    print "         Please avoid usage whenever possible.\n";
                  }
                } else {
                  add_comp_html($data, $filehandle, $basename, $name);
                }
              }
      
            }
            last if $does_match == 2;
        } # end for
        if ($filehandle) {
          print $filehandle <<END;
</TABLE>

END
        } # end if filehandle
    } #end if open DIR
}

# Start of main ===============================

my $index         = 0;
my $file;
my $show_website  = 0;
my $show_manual   = 0;
my $show_compman  = 0;
my $show_tutorial = 0;     

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
  } elsif(/^--tutorial$/i) {
        $show_tutorial = 1; 
  } elsif(/^--local$/i) {
        $use_local = 1;
  } elsif(/^--force$/i || /^-f$/i) {
        $is_forced = 1;
  } elsif(/^--help$/i || /^-h$/i || /^-v$/i) {
      print "Usage: mcdoc [options] <dir|file>\n";
      print "Generate/show component/instrument documentation using $browser\n";
      print "   -f    --force    Force re-writting of existing HTML doc locally\n";
      print "   -h    --help     Show this help\n";
      print "   -l    --tools    Display the McStas tools list\n";
      print "   -m    --manual   Open the McStas User manual\n";
      print "   -c    --comp     Open the McStas Component manual\n";
      print "   -t    --text     For single component, display as text\n";
      print "   -w    --web      Open the McStas web page http://mcstas.risoe.dk/\n";
      print "         --tutorial Open the McStas tutorial from the local McStas library\n";
      print "SEE ALSO: mcstas, mcdoc, mcplot, mcrun, mcgui, mcresplot, mcstas2vitess\n";
      print "DOC:      Please visit http://mcstas.risoe.dk/\n";
      exit;
  } elsif(/^--tools$/i || /^-l$/) {
      print "McStas Tools\n";
      print "   mcstas        Main instrument compiler\n";
      print "   mcrun         Instrument maker and execution utility\n";
      print "   mcgui         Graphical User Interface instrument builder\n";
      print "   mcdoc         Component library documentation generator/viewer\n";
      print "   mcplot        Simulation result viewer\n";
      print "   mcdisplay     Instrument geometry viewer\n";
      print "   mcresplot     Instrument resolution function viewer\n";
      print "   mcstas2vitess McStas to Vitess component translation utility\n";
      print "   mcconvert     Matlab <-> Scilab script conversion tool\n";
      print "When used with the -h flag, all tools display a specific help.\n";
      print "SEE ALSO: mcstas, mcdoc, mcplot, mcrun, mcgui, mcresplot, mcstas2vitess\n";
      print "DOC:      Please visit http://mcstas.risoe.dk/\n";
      exit;
  } else {
      $file = $ARGV[$i];
      $index++;
  }
} # end for

if ($show_website) {
  # open the index.html
  my $cmd = "$MCSTAS::mcstas_config{'BROWSER'} http://mcstas.risoe.dk/ ";
  print "mcdoc: Starting $cmd\n"; system("$cmd\n");
  die "mcdoc: web site done.\n";
}

if ($show_manual) {
  # open the manual using embedded acroread plugin
  $cmd = "$MCSTAS::mcstas_config{'BROWSER'} $MCSTAS::sys_dir/doc/mcstas-manual.pdf";
  print "mcdoc: Starting $cmd\n"; system("$cmd\n");
  die "mcdoc: User manual done.\n";
}

if ($show_compman) {
  # open the component manual
  $cmd = "$MCSTAS::mcstas_config{'BROWSER'} $MCSTAS::sys_dir/doc/mcstas-components.pdf";
  print "mcdoc: Starting $cmd\n"; system("$cmd\n");
  die "mcdoc: Component manual done.\n";
}

if ($show_tutorial) {
  # open the index.html
  $cmd = "$MCSTAS::mcstas_config{'BROWSER'} $MCSTAS::sys_dir/doc/tutorial/html/tutorial.html";
  print "mcdoc: Starting $cmd\n"; system("$cmd\n");
  die "mcdoc: Tutorial done.\n";
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

if (not $is_single_file) {
  # Open the local documentation file
  $filehandle = new FileHandle;
  my $no_lib_write  = 0;
  
  if (not open($filehandle, ">$lib_dir/$out_file")) { $no_lib_write = 1; }
  if ($no_lib_write) { 
    my $no_local_write = 0;
    if (not open($filehandle, ">$out_file")) { $no_local_write = 1; }
    if ($no_local_write) {
      $filehandle = 0; # will not write the catalog
      print STDERR "mcdoc: Could not open $out_file for writing.\n";
    }
  } else { 
    $out_file = "$lib_dir/$out_file";
  }
  if (not $filehandle) {
    if (-f "$lib_dir/$out_file") { 
      $out_file = "$lib_dir/$out_file";  
    }
    elsif (not -f $out_file) {
      print STDERR "mcdoc: Could not find the $out_file library catalog.\n";
    }
  }
}

if ($use_local) {
  # define local and lib sections
  @sections = ("sources", "optics", "samples", "monitors", 
               "misc", "contrib", "obsolete","examples","local","data","share","doc");
  %section_headers =
    ("sources" => '<B><FONT COLOR="#FF0000">Sources</FONT></B>',
     "optics" => '<B><FONT COLOR="#FF0000">Optics</FONT></B>',
     "samples" => '<B><FONT COLOR="#FF0000">Samples</FONT></B>',
     "monitors" => '<B><FONT COLOR="#FF0000">Detectors</FONT> and monitors</B>',
     "contrib" => '<B><FONT COLOR="#FF0000">Contributed</FONT> components</B>',
     "misc" => '<B><FONT COLOR="#FF0000">Misc</FONT></B>',
     "obsolete" => '<B><FONT COLOR="#FF0000">Obsolete</FONT> (avoid usage whenever possible)</B>',
     "examples" => '<B><FONT COLOR="#FF0000">Instrument Examples</FONT></B>',
     "local" => '<B><FONT COLOR="#FF0000">Local components</FONT></B>',
     "data" => '<B><FONT COLOR="#FF0000">Data files</FONT></B>',
     "share" => '<B><FONT COLOR="#FF0000">Shared libraries</FONT></B>',
     "doc"   => '<B><FONT COLOR="#FF0000">Documentation</FONT></B>');
} else {
  # define lib sections
  @sections = ("sources", "optics", "samples", "monitors", "misc", "contrib","examples");
  %section_headers =
    ("sources" => '<B><FONT COLOR="#FF0000">Sources</FONT></B>',
     "optics" => '<B><FONT COLOR="#FF0000">Optics</FONT></B>',
     "samples" => '<B><FONT COLOR="#FF0000">Samples</FONT></B>',
     "monitors" => '<B><FONT COLOR="#FF0000">Detectors</FONT> and monitors</B>',
     "contrib" => '<B><FONT COLOR="#FF0000">Contributed</FONT> components</B>',
     "misc" => '<B><FONT COLOR="#FF0000">Misc</FONT></B>',
     "obsolete" => '<B><FONT COLOR="#FF0000">Obsolete</FONT> (avoid usage whenever possible)</B>',
     "examples" => '<B><FONT COLOR="#FF0000">Instrument Examples</FONT></B>',);
}
my @tblist = map "<A href=\"#$_\">$_</A>", @sections;
my $toolbar = "<P ALIGN=CENTER>\n [ " . join("\n | ", @tblist) . " ]\n</P>\n";

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
    # open the index.html
    my $cmd = "$MCSTAS::mcstas_config{'BROWSER'} $out_file";
    print "mcdoc: Starting $cmd\n"; system("$cmd\n");
  }
}
