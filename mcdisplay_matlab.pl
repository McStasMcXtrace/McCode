#! /usr/bin/perl

# Perl wrapper for calling Matlab for 'mcdisplay' style plotting
# Made from stripped down version of mcdisplay.pl
# Note: Calls through pipe on unix style plaforms, needs OLE
# on Win32...
#
# PW 20030314

# In emacs, please make this -*- perl -*- mode. Thanks.

# Determine the path to the McStas system directory. This must be done
# in the BEGIN block so that it can be used in a "use lib" statement
# afterwards.

# Config module needed for Win32 vs unix setup.
# PW 20030314
use Config;

BEGIN {
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
    # If this is Win32, load OLE related modules -
    # we can not talk to Matlab using pipe on Win32 :(
    # PW 20030314
    if ($Config{'osname'} eq 'MSWin32') {
      require Win32::OLE;
      require Win32::OLE::Variant;
    }
  }
use lib $MCSTAS::perl_dir;
require "mcstas_config.perl";

use IPC::Open2;

$magnification = 1;

my (%transformations, @components);


sub read_instrument {
    my ($in) = @_;
    my ($st, $comp, $compcnt);

    $st = 0;
    @components = ();
    while(<$in>) {
        if($st == 0 && /^INSTRUMENT:/) {
            # Start of instrument description.
            $st = 1;
	    # Initialize matlab struct...
	    write_process("addpath('$MCSTAS::sys_dir/tools/matlab');\n");
	    write_process("mcdisplay('Init');\n");
	    write_process("global INSTRUMENT;");
	    write_process("INSTRUMENT.descr='$sim_cmd';\n");
        } elsif($st == 1 && /^COMPONENT:\s*"([a-zA-Z0-9_æøåÆØÅ]+)"\s*/) {
            $comp = $1;
	    @components = (@components, $comp);
	    # Initialize components in matlab struct:
	    write_process("INSTRUMENT.name{length(INSTRUMENT.name)+1}='$comp';\n");
	} elsif($st == 1 && /^POS:(.*)$/) {
            my @T;
            @T = split ",", $1;
	    $compcnt=scalar(@components);
	    write_process("INSTRUMENT.$comp.T=ReshapeTransform([@T]);\n");
	    write_process("INSTRUMENT.$comp.j=$compcnt;\n");
            write_process("INSTRUMENT.$comp.K=cell(0);\n");
	    $transformations{$comp} = \@T;
        } elsif($st == 1 && /^MCDISPLAY: start$/) {
            $st = 2;                # Start of component graphics representation
        } elsif($st == 2 && /^MCDISPLAY: component ([a-zA-Z0-9_æøåÆØÅ]+)/) {
            $comp = $1;
            $compdraw{$comp} = {};
            $compdraw{$comp}{'elems'} = [];
        } elsif($st == 2 && /^MCDISPLAY: magnify\('([xyz]*)'\)$/) {
            my $mag = $1;
            $compdraw{$comp}{'magX'} = 1 if $mag =~ /x/i;
            $compdraw{$comp}{'magY'} = 1 if $mag =~ /y/i;
            $compdraw{$comp}{'magZ'} = 1 if $mag =~ /z/i;
        } elsif($st == 2 && /^MCDISPLAY: multiline\(([0-9]+),([^()\n]+)\)$/) {
            my $count = $1;
            my @coords = split ',', $2;
            push @{$compdraw{$comp}{'elems'}},
                {type => 'multiline',
                 count => $count,
                 coords => \@coords};
	    write_process("coords=[@coords];\n");
	    write_process("x=coords(1:3:length(coords));\n");
	    write_process("y=coords(2:3:length(coords));\n");
	    write_process("z=coords(3:3:length(coords));\n");
	    write_process("coords=[x;y;z;1+0*z];\n");
	    write_process("INSTRUMENT.$comp.K{size(INSTRUMENT.$comp.K,2)+1}=coords;\n");
        } elsif($st == 2 &&
                /^MCDISPLAY: circle\('([xyzXYZ]{2})',([-+0-9.eE]+),([-+0-9.eE]+),([-+0-9.eE]+),([-+0-9.eE]+)\)$/) {
            my ($plane,$x,$y,$z,$r) = ($1,$2,$3,$4,$5);
            # Make a circle using a 25-order multiline.
            my @coords = ();
            my $i;
            for($i = 0; $i <= 24; $i++) {
                my $a = $r*cos(2*3.1415927/24*$i);
                my $b = $r*sin(2*3.1415927/24*$i);
                my ($x1,$y1,$z1) = ($x,$y,$z);
                if($plane =~ /xy|yx/i) {
                    $x1 += $a;
                    $y1 += $b;
                } elsif($plane =~ /xz|zx/i) {
                    $x1 += $a;
                    $z1 += $b;
                } elsif($plane =~ /yz|zy/i) {
                    $y1 += $a;
                    $z1 += $b;
                } else {
                    die "Bad plane specifier in circle: '$plane'";
                }
                push @coords, $x1, $y1, $z1;
            }
            push @{$compdraw{$comp}{'elems'}},
                {type => 'multiline',
                 count => 25,
                 coords => \@coords};
	    write_process("coords=[@coords];\n");
	    write_process("x=coords(1:3:length(coords));\n");
	    write_process("y=coords(2:3:length(coords));\n");
	    write_process("z=coords(3:3:length(coords));\n");
	    write_process("coords=[x;y;z;1+0*z];\n");
	    write_process("INSTRUMENT.$comp.K{size(INSTRUMENT.$comp.K,2)+1}=coords;\n");
        } elsif($st == 2 && /^MCDISPLAY: end$/) {
	    write_process("mcdisplay('Load');\n");
	    write_process("PlotInstrument('init');\n");
	    write_process("wait(INSTRUMENT.fig);\n");
	    $st = 1;                # End of component graphics representation
        } elsif($st == 1 && /^INSTRUMENT END:/) {
            $st = 100;
            last;
        } else {
            print;
        }
    }
    exit if($st != 100);        # Stop when EOF seen before instrument end.
    return $#components + 1;
}


sub make_instrument {
    my (@x, @y, @z, @ori, @dis, @comp);
    my ($i, $c, %instr);
    my ($xmin, $xmax, $ymin, $ymax, $zmin, $zmax);

    $i = 0;
    foreach $c (@components) {
        my (@T, @U);
        @T = @{$transformations{$c}};
        $x[$i] = $T[0];
        $xmin = $x[$i] if !$xmin || $x[$i] < $xmin;
        $xmax = $x[$i] if !$xmax || $x[$i] > $xmax;
        $y[$i] = $T[1];
        $ymin = $y[$i] if !$ymin || $y[$i] < $ymin;
        $ymax = $y[$i] if !$ymax || $y[$i] > $ymax;
        $z[$i] = $T[2];
        $zmin = $z[$i] if !$zmin || $z[$i] < $zmin;
        $zmax = $z[$i] if !$zmax || $z[$i] > $zmax;

        @U = ($T[3], $T[4], $T[5], $T[6], $T[7], $T[8], $T[9], $T[10], $T[11]);
        $ori[$i] = \@U;
        $comp[$i] = $c;
        # Now transform coordinates for component graphics representations.
        if($compdraw{$c}) {
            my $magX = $compdraw{$c}{'magX'};
            my $magY = $compdraw{$c}{'magY'};
            my $magZ = $compdraw{$c}{'magZ'};
            foreach $elem (@{$compdraw{$c}{'elems'}}) {
                if($elem->{'type'} eq 'multiline') {
                    my @coords = @{$elem->{'coords'}};
                    my @xs = ();
                    my @ys = ();
                    my @zs = ();
                    my ($xv,$yv,$zv);
                    while(@coords) {
                        $xv = shift(@coords);
                        $yv = shift(@coords);
                        $zv = shift(@coords);
                        $xv *= $magnification if $magX;
                        $yv *= $magnification if $magY;
                        $zv *= $magnification if $magZ;
                        push @xs, ($xv*$T[3] + $yv*$T[6] + $zv*$T[9]  + $T[0]);
                        push @ys, ($xv*$T[4] + $yv*$T[7] + $zv*$T[10] + $T[1]);
                        push @zs, ($xv*$T[5] + $yv*$T[8] + $zv*$T[11] + $T[2]);
                    }
                    $elem->{'X'} = \@xs;
                    $elem->{'Y'} = \@ys;
                    $elem->{'Z'} = \@zs;
                }
            }
        }
        $i++;
    }
    if($xmin == $xmax) {
        $xmin--;
        $xmax++;
    }
    if($ymin == $ymax) {
        $ymin--;
        $ymax++;
    }
    if($zmin == $zmax) {
        $zmin--;
        $zmax++;
    }
    $xmin -= ($xmax - $xmin) / 6;
    $xmax += ($xmax - $xmin) / 6;
    $ymin -= ($xmax - $xmin) / 6;
    $ymax += ($xmax - $xmin) / 6;
    $zmin -= ($xmax - $xmin) / 6;
    $zmax += ($xmax - $xmin) / 6;
    %instr = ('x' => \@x, 'y' => \@y, z => \@z,
              ori => \@ori, dis => \@dis, comp => \@comp,
              xmin => $xmin, xmax => $xmax,
              ymin => $ymin, ymax => $ymax,
              zmin => $zmin, zmax => $zmax,
              zoom_xmin => $xmin, zoom_xmax => $xmax,
              zoom_ymin => $ymin, zoom_ymax => $ymax,
              zoom_zmin => $zmin, zoom_zmax => $zmax);
    return %instr;
}    
        

sub transform {
    my ($comp, $x, $y, $z, $vx, $vy, $vz, $t, $ph1, $ph2) = @_;
    if(!$comp) {
        return ($x, $y, $z, $vx, $vy, $vz, $t, $ph1, $ph2);
    } else {
        my ($nx, $ny, $nz, $nvx, $nvy, $nvz, $nt, $nph1, $nph2);
        my @T = @{$transformations{$comp}};
        $x *= $magnification if $compdraw{$comp} && $compdraw{$comp}{'magX'};
        $y *= $magnification if $compdraw{$comp} && $compdraw{$comp}{'magY'};
        $z *= $magnification if $compdraw{$comp} && $compdraw{$comp}{'magZ'};
        $nx = $x*$T[3] + $y*$T[6] + $z*$T[9] + $T[0];
        $ny = $x*$T[4] + $y*$T[7] + $z*$T[10] + $T[1];
        $nz = $x*$T[5] + $y*$T[8] + $z*$T[11] + $T[2];
        $nvx = $vx*$T[3] + $vy*$T[6] + $vz*$T[9];
        $nvy = $vx*$T[4] + $vy*$T[7] + $vz*$T[10];
        $nvz = $vx*$T[5] + $vy*$T[8] + $vz*$T[11];
        return ($nx, $ny, $nz, $nvx, $nvy, $nvz, $t, $ph1, $ph2);
    }
}


sub get_inspect_pos {
    my ($inspect, @comps) = @_;
    return 0 unless $inspect;
    my $i;
    for($i = 0; $i < @comps; $i++) {
        return $i if $comps[$i] eq $inspect;
    }
    die "Error: Inspected component $inspect not part of instrument?";
}


sub read_neutron {
    my ($in) = @_;
    my (@x, @y, @z, @vx, @vy, @vz, @t, @ph1, @ph2, @ncomp);
    my ($st, $i);
    my ($comp, $numcomp);

    $st = 0;
    $i = 0;
    $numcomp = 0;
    my $dropit = 1;                # Flag to drop uninteresting neutron states.
    while(<$in>) {
        if($st == 0 && /^ENTER:/) {
            # Neutron enters instrument.
            $st = 1;
        } elsif($st == 0 && /^STATE:/) {
            # State after leaving - should probably be removed in McStas.
            next;
        } elsif($st == 1 && /^COMP:\s*"([a-zA-Z0-9_æøåÆØÅ]+)"\s*$/) {
            # Neutron enters component local coordinate system.
            $comp = $1;
            $numcomp++;
            $dropit = 1;        # Drop the first state (entry point).
        } elsif($st == 1 && (/^STATE:(.*)$/ || /^SCATTER:(.*)$/)) {
            # Neutron state.
            $dropit = 0, next if $dropit; # Skip entry point
            ($x[$i], $y[$i], $z[$i],
             $vx[$i], $vy[$i], $vz[$i],
             $t[$i], $ph1[$i], $ph2[$i]) = split ",", $1;
            ($x[$i], $y[$i], $z[$i],
             $vx[$i], $vy[$i], $vz[$i],
             $t[$i], $ph1[$i], $ph2[$i]) =
                 transform($comp, $x[$i], $y[$i], $z[$i],
                           $vx[$i], $vy[$i], $vz[$i],
                           $t[$i], $ph1[$i], $ph2[$i]);
            $ncomp[$i] = $comp;
            $i++;
        } elsif($st == 1 && /^ABSORB:/) {
            # Neutron was absorbed.
            next;                # No special action needed.
        } elsif($st == 1 && /^LEAVE:/) {
            # Neutron leaves instrument.
            $st = 2;
            last;
        } else {
            # Pass on any output not meant for us.
            print;
        }
    }
    exit unless $st == 2;        # Stop when EOF seen before neutron data end.

    my %neutron = ('x' => \@x, 'y' => \@y, z => \@z,
                   vx => \@vx, vy => \@vy, vz => \@vz,
                   t => \@t, ph1 => \@ph1, ph2 => \@ph2,
                   comp => \@ncomp, numcomp => $numcomp);
    return %neutron
}
    
sub read_process {
    my ($in) = @_;
    while(<$in>) {
	print;
	exit;
    }
}

sub plot_neutron {
    my ($x, $y, $z, $vx, $vy, $vz, $comp) = @_;
    my ($i, $col, $oldcomp);

    write_process("mcdisplay('PlotNeutron',[@$x],[@$y],[@$z]);\n");
    # Show component entry/exit points in same colour as respective component.
#    $i = 0;
#    $col = 4;
#    while($i < scalar(@$x)) {
#        if(!defined($oldcomp) || $oldcomp cmp $comp->[$i]) {
#            $oldcomp = $comp->[$i];
#            pgsci($col++);
#            $col = 4 if $col > 15;
#        }
#        # Exit point.
#        pgpt(1, $x->[$i], $y->[$i], 17);
#        $i++;
#    }
}

sub write_process {
    my ($command) = @_;
    if (!$pid eq 0) {
      (kill 0, $pid) || die "Matlab process terminated - ending...\n";
    }
    if ($Config{'osname'} eq 'MSWin32' && $output_mode eq 0) {
      $ML->Execute($command);
      my $err = Win32::OLE::LastError();
      if ($err) {
	die "Matlab terminated! - Exiting.\n";
      }
    } else {
      print WRITER $command;
    }
}

sub plot_instrument {
    my ($rinstr, $rneutron) = @_;
    my %instr = %$rinstr;
    my %neutron = %$rneutron;
    plot_neutron($neutron{'z'}, $neutron{'x'}, $neutron{'y'}, 
                 $neutron{'vz'}, $neutron{'vx'}, $neutron{'vy'}, $neutron{'comp'});
    write_process("mcdisplay('Timeout');\n");
    return 0;                        # Default: do not repeat this neutron.
}

# Check command line arguments.

undef $inspect;
undef $sim_cmd;
my $int_mode=0; # interactive mode(0), non interactive (1)

# Add: P. Willendrup, Mar 11th, 2003
$output_mode=0; # default mode(0), mcdisplay calls scilab
                   # output mode(1), output to filename

my $output_file;
my $i;

for($i = 0; $i < @ARGV; $i++) {
    if(($ARGV[$i] =~ /^-i([a-zA-ZæøåÆØÅ0-9_]+)$/) ||
            ($ARGV[$i] =~ /^--inspect=([a-zA-ZæøåÆØÅ0-9_]+)$/)) {
        $inspect = $1;
    } elsif($ARGV[$i] =~ /^--output=([a-zA-ZæøåÆØÅ0-9_\.\/\:]+)$/) {
	# Add: P. Willendrup, Mar 11th, 2003 
        # check for --output parameter, defining output filename
        $output_mode = 1;
	$output_file = $1;
    }
    else {
        if (defined($sim_cmd)) { push @cmdline, $ARGV[$i]; }
        else { $sim_cmd = $ARGV[$i]; }
    }
}
die "Usage: mcdisplay [-i] [-output] Instr.out [instr_options] params
 -iCOMP  --inspect=COMP Show only trajectories reaching component COMP
 -output                Take output from running mcstas instrument
SEE ALSO: mcstas, mcplot, mcrun, mcresplot, mcstas2vitess, mcgui\n"
 unless $sim_cmd;

my ($numcomp, %neutron, %instr);

$args = join(" ", @cmdline);
$cmdline = "$sim_cmd --trace $args";
printf STDERR "Starting simulation '$cmdline' ...\n";
open(IN, "$cmdline |") || die "Could not run simulation\n";

# Add: P. Willendrup, Feb 19th, 2003 set OUT - talk to external process...
if ($output_mode eq 0) {
  if ($Config{'osname'} eq 'MSWin32') {
    $pid=0;
    $ML = Win32::OLE->new('Matlab.Application'); 
  } else {
    $pid=open2(READER,WRITER, $MCSTAS::mcstas_config{'MATLAB_COMMAND'}) || die "Could not start Matlab\n";
  }
  print STDERR "Opened up pipe to matlab - pid is $pid\n";
  print STDERR "Building Matlab INSTRUMENT struct, please have patience...\n";
} else { 
    open(WRITER, "> $output_file");
    $pid=0;
}
# Test write to matlab process:
$numcomp = read_instrument(IN);
$inspect_pos = get_inspect_pos($inspect, @components);
%instr = make_instrument;
print STDERR "Matlab INSTRUMENT done, starting gui....\n";
while(!eof(IN)) {
    %neutron = read_neutron(IN);
    next if $neutron{'numcomp'} <= $inspect_pos;
    my $ret;
    $ret = plot_instrument(\%instr, \%neutron);
    next;
}
close(IN);
close(WRITER);
























