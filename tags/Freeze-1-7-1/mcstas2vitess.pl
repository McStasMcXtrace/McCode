#! /usr/bin/perl -w

# Determine the path to the McStas system directory. This must be done
# in the BEGIN block so that it can be used in a "use lib" statement
# afterwards.
BEGIN {
    if($ENV{"MCSTAS"}) {
        $MCSTAS::sys_dir = $ENV{"MCSTAS"};
    } else {
        $MCSTAS::sys_dir = "/usr/local/lib/mcstas";
    }
    $MCSTAS::perl_dir = "$MCSTAS::sys_dir/tools/perl"
}
use lib $MCSTAS::perl_dir;

use FileHandle;

require "mcstas_config.perl";
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

    print $F <<INSTR_END;
DEFINE INSTRUMENT V_sample()
DECLARE
%{
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

/* Our main() function. */
int main(int argc, char *argv[])
{
  vitess_main(argc, argv, &check_finished, dptr, dchr, sptr, schr);
  exit(0);
}
%}
INITIALIZE
%{
  %include "vitess-lib.h"
  /* This double-indirection is necessary here since MC_GETPAR is not
     available in the DECLARE section. */
  check_finished = &MC_GETPAR(vitess_in, finished);
%}
TRACE

COMPONENT vitess_in = Vitess_input(
    file = vitess_infile, repeat_count = vitess_repcnt,
    bufsize = vitess_bufsize)
  AT (0, 0, 0) ABSOLUTE

COMPONENT comp = $comp_name(
    $comp_actuals)
  AT (pos_x, pos_y, pos_z) ABSOLUTE
  ROTATED (0, 90, 90) ABSOLUTE

COMPONENT vitess_out = Vitess_output(
    file = vitess_outfile, bufsize = vitess_bufsize,
    progress = vitess_tracepoints)
  AT (0, 0, 0) ABSOLUTE

END
INSTR_END
}

sub make_tcl_file {
    my ($F, $par, $d) = @_;

    print $F "### $d->{'name'}\n###\n";
    print $F "gSet ", lc($d->{'name'}), "ESET {\n";
    my $dsc = $d->{'identification'}{'short'};
    chomp $dsc;
    $dsc =~ s/\n/\\n/g;
    print $F "  {\"$dsc\" header}\n";
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
                print $F " \"$d->{'parhelp'}{$p}{'default'}\"";
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

if(@ARGV != 1) {
    print STDERR "Usage: mcstas2vitess component\n";
    print STDERR "       This tool enables to convert a single McStas component into\n";
    print STDERR "       a Vitess module. Component string parameters should be declared\n";
    print STDERR "       as 'char*' setting parameters. Default values are allowed.\n";
    print STDERR "SEE ALSO: mcstas, mcdisplay, mcrun, mcplot, mcresplot, mcgui\n";
    exit 1;
}

my $compfile = $ARGV[0];
my $compname = $compfile;
$compname = $1 if $compname =~ /^(.*)\.(comp|cmp|com)$/;

my $data = component_information($compfile);
die "Failed to get information for component '$compfile'" 
    unless defined($data);

# Read the corresponding .vif file if available.
my %vif = ();
my %vifletters = ();
my $VIF = new FileHandle;
my $vifname = "$compname.vif";
if(open($VIF, $vifname)) {
    while(<$VIF>) {
        if(/^\s*([a-zA-ZæøåÆØÅ0-9_]+)\s+-([a-zA-Z0-9])\s+(string|double)\s*$/) {
            $vif{$1} = [$2, $3];
            $vifletters{$2} = $1;
        } elsif(/^\s*([a-zA-ZæøåÆØÅ0-9_]+)\s+-([a-zA-Z0-9])\s*$/) {
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
                 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q',
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
    if($p =~ /(char\s*\*|string)\s+([a-zA-ZæøåÆØÅ0-9_]+)/i) 
      { $typ = 'string'; $p=$2; }
    $typ = 'double' unless $typ;
    push @param, [$p, $let, $typ];
}

# Output the .instr file.
my $INSTR = new FileHandle;
my $instr_name = "${compname}_VITESS.instr";
my $c_name = "${compname}_VITESS.c";
open($INSTR, ">$instr_name") ||
    die "Could not open output instrument file '$instr_name'.";
make_instr_file($INSTR, \@param, $data);
close($INSTR);
print "Wrote instrument file '$instr_name'.\n";

my @mcstas_cmd = ("mcstas", "--no-main", "-o", $c_name, $instr_name);
print join(" ", @mcstas_cmd), "\n";
if(system(@mcstas_cmd)) {
    print "*** Error exit ***\n";
    print STDERR "McStas compilation failed.\n";
    exit 1;
}
print "Wrote C file '$c_name'.\n";

my $out_name = "${compname}_VITESS";
my $cc = $ENV{'MCSTAS_CC'} || $MCSTAS::mcstas_config{CC};
my $cflags = $ENV{'MCSTAS_CFLAGS'} || $MCSTAS::mcstas_config{CFLAGS};
my $vitess_lib_name = "vitess-lib.c";
$vitess_lib_name = $MCSTAS::sys_dir . "/share/" . $vitess_lib_name
    unless -r $vitess_lib_name;
die "Cannot find VITESS library file '$vitess_lib_name'"
    unless -r $vitess_lib_name;
my @cc_cmd = ($cc, split(' ', $cflags), "-o", $out_name,
              "-I$MCSTAS::sys_dir", $c_name, "-lm");
print join(" ", @cc_cmd), "\n";
if(system(@cc_cmd)) {
    print "*** Error exit ***\n";
    print STDERR "C compilation failed.\n";
    exit 1;
}
print "Wrote executable file '$out_name'.\n";

# Output the .tcl file for the VITESS gui.
my $TCL = new FileHandle;
my $tcl_name = "${compname}_VITESS.tcl";
open($TCL, ">$tcl_name") ||
    die "Could not open output Tcl file '$tcl_name'.";
make_tcl_file($TCL, \@param, $data);
close($TCL);
print "Wrote Tcl GUI file '$tcl_name'.\n";

exit 0;
