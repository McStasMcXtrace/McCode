#! /usr/bin/perl
#
# Converts McStas instruments to Vitess equivalents. Has not been tested
# with current versions of Vitess (20031128)
#
#   This file is part of the McStas neutron ray-trace simulation package
#   Copyright (C) 1997-2004, All rights reserved
#   Risoe National Laborartory, Roskilde, Denmark
#   Institut Laue Langevin, Grenoble, France
#
#   This program is free software; you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation; version 2 of the License.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
#
#   You should have received a copy of the GNU General Public License
#   along with this program; if not, write to the Free Software
#   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

# Determine the path to the McStas system directory. This must be done
# in the BEGIN block so that it can be used in a "use lib" statement
# afterwards.
BEGIN {
    ENV_HEADER
}
use lib $MCSTAS::perl_dir;
use POSIX qw(uname);
use Config;
use FileHandle;

require "mccode_config.perl";

# Overload with user's personal config
if ($ENV{"HOME"} && -e $ENV{"HOME"}."/".$MCSTAS::mcstas_config{'VERSION'}."/.mcstas/mccode_config.perl") {
  print "mcstas2vitess: reading local $MCSTAS::mcstas_config{'MCCODE'} configuration from " . $ENV{"HOME"}."/.".$MCSTAS::mcstas_config{'MCCODE'}."/".$MCSTAS::mcstas_config{'VERSION'}."/mccode_config.perl\n";
  require $ENV{"HOME"}."/.".$MCSTAS::mcstas_config{'MCCODE'}."/".$MCSTAS::mcstas_config{'VERSION'}."/mccode_config.perl";
}

require "mcrunlib.pl";

sub make_instr_file {
    my ($F, $par, $d) = @_;
    my $double_decl = "";
    my $str_decl = "";
    my $double_adr = "";
    my $str_adr = "";
    my $double_letters = "";
    my $str_letters = "";
    my $comp_name = $d->{'name'};
    my $comp_actuals = "";
    my $firstcomp="";
    my $checkinit="";
    my $terminate="";
    my $finally="";
    my $typ;
    for (@$par) {
        my ($p, $let, $typ) = @$_;
        $comp_actuals = $comp_actuals ? "$comp_actuals, $p=$p" : "$p=$p";
        if($typ eq 'double') {
            $double_decl = $double_decl ? "$double_decl, $p" : "double $p";
            $double_adr = $double_adr ? "$double_adr &$p," : "&$p,";
            $double_letters = $double_letters ? "$double_letters '$let'," : "'$let',";
        } elsif($typ eq 'string') {
            $str_decl = $str_decl ? "$str_decl, *$p" : "char *$p";
            $str_adr = $str_adr ? "$str_adr &$p," : "&$p,";
            $str_letters = $str_letters ? "$str_letters '$let'," : "'$let',";
        } else {
            die "Internal: make_instr_file()";
        }
    }
    if ($d->{'isasource'} == 1) {
      $firstcomp="Arm()
    ";
      $checkinit="check_finished = 0;";
      $terminate="our_ncount < Ncount";
      $double_decl= "$double_decl, Ncount";
      $double_adr= "$double_adr &Ncount,";
      $double_letters= "$double_letters 'n',";
      $finally = "";
    } else {
      $firstcomp="Vitess_input(
    filename = vitess_infile, repeat_count = vitess_repcnt,
    bufsize = vitess_bufsize)";
      $checkinit="check_finished = &COMP_GETPAR(vitess_in, finished);";
      $terminate="!*check_finished";
      $finally="  double p_sum=0.0, p2_sum=0.0;
  p_sum  = COMP_GETPAR(vitess_out, p_out);
  p2_sum = COMP_GETPAR(vitess_out, p2_out);
  vitess_write(instrument->counter_N[1]-1, instrument->counter_N[3], p_sum, p2_sum, pos_x, pos_y, pos_z, 0.0, 0.0);"
    }

    print $F <<INSTR_END;
DEFINE INSTRUMENT McStas_$comp_name()
/*
 * This file has been automatically generated using the mcstas2vitess tool.
 * See http://www.mcstas.org
 * Component $comp_name converted into an instrument with Vitess I/O functions
 */
DECLARE
%{
#define printf(...) fprintf(stderr,__VA_ARGS__)
#define fprintf(stdout,...) fprintf(stderr,__VA_ARGS__)
/* Component parameters. */
$double_decl;
$str_decl;
double pos_x, pos_y, pos_z;
double *dptr[] =
  {
    $double_adr
    &pos_x, &pos_y, &pos_z,
    0
  };
char **sptr[] =
  {
    $str_adr
    0
  };
char dchr[] =
  {
    $double_letters
    'x', 'y', 'z', 0
  };
char schr[] =
  {
    $str_letters
    0
  };

/* vitess-lib will be included when embedding Vitess_input component */

/* Pointer to check whether all neutrons have been read. */
int *check_finished;

/* event parameters that exist in VITESS, but not in McStas */
short   vitess_col;
TotalID vitess_ID;

/* Our main() function. */
int main(int argc, char *argv[])
{

  srandom(time(NULL));  /* Random seed */
  vitess_parseopt(argc, argv, dptr, dchr, sptr, schr); /* VITESS-style option parser */
  init();
  double our_ncount=-1;
  do
  {
    _class_particle _particle=mcgenstate();
    raytrace(&_particle);
    our_ncount++;
  } while($terminate);

  finally();
  exit(0);
}
%}
INITIALIZE
%{
  %include "vitess-lib.h"
  /* This double-indirection is necessary here since COMP_GETPAR is not
     available in the DECLARE section. */
  $checkinit
  vitess_col =0;
%}
TRACE

COMPONENT vitess_in = $firstcomp
  AT (0, 0, 0) ABSOLUTE

COMPONENT comp = $comp_name(
    $comp_actuals)
  AT (pos_x, pos_y, pos_z) ABSOLUTE
  ROTATED (0, 0, 0) ABSOLUTE

COMPONENT vitess_out = Vitess_output(
    filename = "stdout", bufsize = vitess_bufsize,
    progress = vitess_tracepoints)
  AT (0, 0, 0) ABSOLUTE

FINALLY
%{
  $finally  
%}

END
INSTR_END
}

sub make_tcl_file {
    my ($F, $par, $d) = @_;

    print $F "global AvailableSET\n";
    print $F "lappend AvailableSET {mcstas_", lc($d->{'name'}), "}\n";

    print $F "### $d->{'name'}\n###\n";
    print $F "gSet mcstas_", lc($d->{'name'}), "ESET {\n";
    my $dsc = $d->{'identification'}{'short'};
    chomp $dsc;
    $dsc =~ s/\n/\\n/g;
    print $F "  {\"$dsc\" header}\n";
    if ($d->{'isasource'} == 1) {
      print $F "  {\"number of tracjectories\" float 1e6 {\"Ncount [1]\" \"Defines number of trajectories to generate\" \"\" n}}\n";
    }
    
    for (@$par) {
        my ($p, $let, $typ) = @$_;
        print $F "  {$p ";
        if($typ eq 'double') {
            print $F "float";
            if($d->{'parhelp'}{$p}{'default'}) {
                print $F " $d->{'parhelp'}{$p}{'default'}";
            } else {
                print $F " \"\"";
            }
        } elsif($typ eq 'string') {
            print $F "string";
            if($d->{'parhelp'}{$p}{'default'}) {
                print $F " $d->{'parhelp'}{$p}{'default'}";
            } else {
                print $F " \"\"";
            }
        } else {
            die "Internal: make_tcl_file()";
        }
        print $F " {\"$p";
        print $F " [$d->{'parhelp'}{$p}{'unit'}]" if $d->{'parhelp'}{$p}{'unit'};
        print $F "\" ";
        print $F "\"";
        if($d->{'parhelp'}{$p}{'text'}) {
            my $txt =  $d->{'parhelp'}{$p}{'text'};
            $txt =~ s/\s+$//;
            $txt =~ s/\n/\\n/g;
	    $txt =~ s/\"//g;
            print $F $txt;
        }
        print $F "\" ";
        print $F "\"\" $let}";
        if($typ eq 'double') {
            print $F " 1" unless $d->{'parhelp'}{$p}{'default'};
        } elsif($typ eq 'string') {
            print $F " \"\" \"\" 1" unless $d->{'parhelp'}{$p}{'default'};
        } else {
            die "Internal: make_tcl_file()";
        }
        print $F "}\n";
    }
    print $F "  {xpos float 0 {\"X position [m]\" \"X position of module\" \"\" x}}\n";
    print $F "  {ypos float 0 {\"Y position [m]\" \"Y position of module\" \"\" y}}\n";
    print $F "  {zpos float 0 {\"Z position [m]\" \"Z position of module\" \"\" z}}\n";
    print $F "}\n";
}

sub print_usage {
    my ($par, $d) = @_;

    print "\n";
    print "Module usage table for Vitess module McStas_", lc($d->{'name'}), ":\n";

    my $dsc = $d->{'identification'}{'short'};
    chomp $dsc;
    $dsc =~ s/\n/\\n/g;
    print "Description: \n\"$dsc\"\n\n";
    print "____________________________________________________________\n";
    print "McStasid, type, default-value, unit, Vitess var, description\n";
    print "____________________________________________________________\n";
    if ($d->{'isasource'} == 1) {
      print "  ncount, float, 1e6, [1], n, Defines number of trajectories to generate\n";
    }
    
    for (@$par) {
        my ($p, $let, $typ) = @$_;
        print "  $p, ";
        if($typ eq 'double') {
            print "float, ";
            if($d->{'parhelp'}{$p}{'default'}) {
                print " $d->{'parhelp'}{$p}{'default'}";
            } else {
                print " \"\"";
            }
        } elsif($typ eq 'string') {
            print "string, ";
            if($d->{'parhelp'}{$p}{'default'}) {
                print " $d->{'parhelp'}{$p}{'default'}";
            } else {
                print " \"\"";
            }
        } else {
            die "Internal: print_usage()";
	}
	print ", [$d->{'parhelp'}{$p}{'unit'}]" if $d->{'parhelp'}{$p}{'unit'};
        print ", ";
	print " $let, ";
	
        if($d->{'parhelp'}{$p}{'text'}) {
            my $txt =  $d->{'parhelp'}{$p}{'text'};
            $txt =~ s/\s+$//;
            $txt =~ s/\n/\\n/g;
	    $txt =~ s/\"//g;
            print $txt;
        }
	print "\n";
    }
    print "  xpos, float, 0, [m], x, X position of module\n";
    print "  ypos, float, 0, [m], y, Y position of module\n";
    print "  zpos, float, 0, [m], z, Z position of module\n";
    print "____________________________________________________________\n";
}


my $issourcemodule = 0;

if(@ARGV == 0 || @ARGV > 2) {
    print STDERR "\n";
    print STDERR "Usage: mcstas2vitess.pl Component.comp [assource]\n";
    print STDERR "\n";
    print STDERR "  1st argument: local component-file \n";
    print STDERR "  2nd argument: [optional] indicate component is a source (i.e. first in Vitess).\n";
    print STDERR "\n";
    print STDERR "       This tool enables to convert a McStas component into\n";
    print STDERR "       an instrument that can be compiled into a Vitess module using mcrun --no-main. \n ";
    print STDERR "\n";
    print STDERR "       If further components are added, these may also provide functionality to the\n";
    print STDERR "       instr/module.\n";
    print STDERR "\n";
    print STDERR "       Supported input arguments are currently of these types only:\n";
    print STDERR "         double, int etc. -> all mapped to float in Vitess (default type)\n";
    print STDERR "         string -> mapped to char* in Vitess\n";
    print STDERR "\n";
    print STDERR "       If the component uses any \"string\" inputs, these MUST be specified in a '.vif file.'\n";
    print STDERR "       Also, \"string\" currently MUST have default values provided in the comp.'\n";
    print STDERR "       Further, the '.vif' file may be used to define the parameter letters used in the \n";
    print STDERR "       resulting Vitess module.\n";
    print STDERR "\n";
    print STDERR "          example content of a .vif file:\n";
    print STDERR "            sector -S string\n";
    print STDERR "            beamline -B double\n";
    print STDERR "\n";
    print STDERR "       - defines that McStas parameter 'sector' is a string that will use Vitess parameter -S\n";
    print STDERR "       - defines that McStas parameter 'beamline' is a double that will use Vitess parameter -B\n";
    print STDERR "\n";
    print STDERR "SEE ALSO: mcstas, mcdoc, mcplot, mcrun, mcgui, mcresplot, mcstas2vitess.pl\n";
    print STDERR "DOC:      Please visit https://www.mcstas.org\n";
    print STDERR "\n";
    exit 1;
} elsif (@ARGV == 1) {
    print "Processing component as NON-source module $ARGV[0] for Vitess\n";
} elsif (@ARGV == 2) {
    print "Processing component as source module $ARGV[0] for Vitess\n";
    $issourcemodule = 1;
}

my $compfile = $ARGV[0];
my $compname = $compfile;
$compname = $1 if $compname =~ /^(.*)\.(comp|cmp|com)$/;

my $data = component_information($compfile);
die "Failed to get information for component '$compfile'"
    unless defined($data);

$data->{'isasource'}=$issourcemodule;

# Read the corresponding .vif file if available.
my %vif = ();
my %vifletters = ();
my $VIF = new FileHandle;
my $vifname = "$compname.vif";
if(open($VIF, $vifname)) {
    while(<$VIF>) {
        if(/^\s*([a-zA-Z0-9_]+)\s+-([a-zA-Z0-9])\s+(string|double)\s*$/) {
            $vif{$1} = [$2, $3];
            $vifletters{$2} = $1;
        } elsif(/^\s*([a-zA-Z0-9_]+)\s+-([a-zA-Z0-9])\s*$/) {
            $vif{$1} = [$2, 'double'];
            $vifletters{$2} = $1;
        } else {
            die "Invalid line:\n$_\nin VITESS information file '$vifname'";
        }
    }
    close $VIF;
} else {
    print "Note: No VITESS information file (.vif) found.\n";
}

# Now decide on how the name and type of each component parameter.
# The following option letters are not available: fFJLZABxyz
my @optletter = ('a', 'b', 'c', 'd', 'e', 'g', 'h', 'i',
                 'j', 'k', 'l', 'm', 'o', 'p', 'q',
                 'r', 's', 't', 'u', 'v', 'w', 'C', 'D',
                 'E', 'G', 'H', 'I', 'K', 'M', 'N', 'O',
                 'P', 'Q', 'R', 'S', 'T', 'Y', 'V', 'W',
                 'X', 'Y');
my @param = ();
my $p;
for $p (@{$data->{'inputpar'}}) {
    # First look for an option letter from the .vif.
    my ($let, $typ) = $vif{$p} ? @{$vif{$p}} : (undef, undef);
    unless($let) {
        # Pick a default option letter not mentioned in the .vif.
        do {
            $let = shift @optletter;
        } while($let && $vifletters{$let});
        die "Too many component parameters!" unless $let;
    }
    if($p =~ /(char\s*\*|string)\s+([a-zA-Z0-9_]+)/i)
      { $typ = 'string'; $p=$2; }
    $typ = 'double' unless $typ;
    push @param, [$p, $let, $typ];
}

print "mcstas2vitess: Converting McStas component ${compname} into Vitess Module 'McStas_${compname}'\n";
# Output the .instr file.
my $INSTR = new FileHandle;
my $instr_name = "McStas_${compname}.instr";
my $c_name = "McStas_${compname}.c";
open($INSTR, ">$instr_name") ||
    die "Could not open output Vitess Module instrument file '$instr_name'.";
make_instr_file($INSTR, \@param, $data);
close($INSTR);
print "Wrote Vitess Module instrument file '$instr_name'.\n";

# Output the .tcl file for the VITESS gui.
my $TCL = new FileHandle;
my $tcl_name = lc("McStas_${compname}.tcl");
open($TCL, ">$tcl_name") ||
    die "Could not open output Vitess Module Tcl file '$tcl_name'.";
make_tcl_file($TCL, \@param, $data);
close($TCL);
print_usage(\@param, $data);
print "\nWrote Vitess Module Tcl GUI file '$tcl_name' for inclusion in your vitess/GUI/usermodule.tcl script.\n\n";
print "mcstas2vitess: Construction of McStas_${compname}.instr completed.\n";
print "\nPlease:\n1) use mcrun -c --no-main McStas_${compname}.instr -n0 to compile\n";
print "2) move resulting executable to mcstas_".lc(${compname})."_\$SYSNAME[.exe]\n";
print "  (\$SYSNAME includes output of `uname` and ` uname -m`, i.e. something like Darwin_arm64)\n";


exit 0;
