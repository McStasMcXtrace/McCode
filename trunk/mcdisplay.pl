#! /usr/bin/perl 
# Removed -w to get rid of "used only once" warnings

# In emacs, please make this -*- perl -*- mode. Thanks.

# Enhanced mcdisplay script for --trace output from McStas simulation.
#
# Implements graphic display using either
# PGPLOT
# Scilab
# Matlab
#
# PW 20030320

# Config module needed for Win32 vs unix setup.
# PW 20030320
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
  # Check if the PGPLOT module can be found, otherwise
  # disable traditional PGPLOT support - output error
  # message...
  # PW 20030320
  $pg_avail=0;
  my $pg_place;
  foreach $inc (@INC) {
    my $where="$inc/PGPLOT.pm";
    if (-e $where) {
      $pg_avail=1;
      $pg_place=$inc;
    }
  }
  if ($pg_avail eq 1) {
    require "PGPLOT.pm";
  }
}

use lib $MCSTAS::perl_dir;
require "mcstas_config.perl";
use Tk;
use Tk::DialogBox;
use IPC::Open2;

#use PGPLOT;

$magnification = 1;
$zooming = 0;

my (%transformations, @components);


sub read_instrument {
    my ($in) = @_;
    my ($st, $comp);

    $st = 0;
    @components = ();
    while(<$in>) {
        if($st == 0 && /^INSTRUMENT:/) {
	    # Start of instrument description.
            $st = 1;
            if ($MCSTAS::mcstas_config{'PLOTTER'} == 1 || $MCSTAS::mcstas_config{'PLOTTER'} == 2) {
	      # Initialize matlab struct...
	      write_process("addpath('$MCSTAS::sys_dir/tools/matlab');\n");
	      write_process("mcdisplay('Init');\n");
	      write_process("global INSTRUMENT;");
	      write_process("INSTRUMENT.descr='$sim_cmd';\n");
              # Possibly, set firstcomp + lastcomp
	      if ($first) {
		write_process("INSTRUMENT.firstcomp='$first';\n");
	      }
	      if ($lasst) {
		write_process("INSTRUMENT.lastcomp='$last';\n");
	      }
	    }
	    if ($MCSTAS::mcstas_config{'PLOTTER'} == 3 || $MCSTAS::mcstas_config{'PLOTTER'} == 4) {
	      # Initialize scilab struct...
	      write_process("exec('$MCSTAS::sys_dir/tools/scilab/mcdisplay.sci',-1);\n");
	      write_process("INSTRUMENT.descr='$sim_cmd';\n");
	      # Possibly, set firstcomp + lastcomp
	      if ($first) {
		write_process("INSTRUMENT.firstcomp='$first';\n");
	      }
	      if ($lasst) {
		write_process("INSTRUMENT.lastcomp='$last';\n");
	      }
              if ($save) {
		write_process("INSTRUMENT.save=1;\n");
	      } else {
		write_process("INSTRUMENT.save=0;\n");
	      }
	    }
	} elsif($st == 1 && /^COMPONENT:\s*"([a-zA-Z0-9_æøåÆØÅ]+)"\s*/) {
            $comp = $1;
            @components = (@components, $comp);
	    if ($MCSTAS::mcstas_config{'PLOTTER'} == 1 || $MCSTAS::mcstas_config{'PLOTTER'} == 2) {
	      # Initialize components in matlab struct:
	      write_process("INSTRUMENT.name{length(INSTRUMENT.name)+1}='$comp';\n");
	    }
	    if ($MCSTAS::mcstas_config{'PLOTTER'} == 3 || $MCSTAS::mcstas_config{'PLOTTER'} == 4) {
	      # Initialize components in scilab struct:
	      write_process("setcomponent('$comp');\n");
	    }
        } elsif($st == 1 && /^POS:(.*)$/) {
            my @T;
            @T = split ",", $1;
            $transformations{$comp} = \@T;
	    $compcnt=scalar(@components);
	    if ($MCSTAS::mcstas_config{'PLOTTER'} == 1 || $MCSTAS::mcstas_config{'PLOTTER'} == 2) {
	      # Component position for matlab struct:
	      write_process("INSTRUMENT.$comp.T=ReshapeTransform([@T]);\n");
	      write_process("INSTRUMENT.$comp.j=$compcnt;\n");
	      write_process("INSTRUMENT.$comp.K=cell(0);\n");
	    }
	    if ($MCSTAS::mcstas_config{'PLOTTER'} == 3 || $MCSTAS::mcstas_config{'PLOTTER'} == 4) {
	      # Component position for scilab struct:
	      write_process("setposition([@T]);\n");
	    }
        } elsif($st == 1 && /^MCDISPLAY: start$/) {
            $st = 2;                # Start of component graphics representation
        } elsif($st == 2 && /^MCDISPLAY: component ([a-zA-Z0-9_æøåÆØÅ]+)/) {
            $comp = $1;
            $compdraw{$comp} = {};
            $compdraw{$comp}{'elems'} = [];
	    if ($MCSTAS::mcstas_config{'PLOTTER'} == 3 || $MCSTAS::mcstas_config{'PLOTTER'} == 4) {
	      # Initialize component variable etc. in scilab
	      write_process("trace_component('$comp');\n");
	    }
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
	    if ($MCSTAS::mcstas_config{'PLOTTER'} == 1 || $MCSTAS::mcstas_config{'PLOTTER'} == 2) {
	      # Line elements for Matlab struct
	      write_process("coords=[@coords];\n");
	      write_process("x=coords(1:3:length(coords));\n");
	      write_process("y=coords(2:3:length(coords));\n");
	      write_process("z=coords(3:3:length(coords));\n");
	      write_process("coords=[x;y;z;1+0*z];\n");
	      write_process("INSTRUMENT.$comp.K{size(INSTRUMENT.$comp.K,2)+1}=coords;\n");
	    }
	    if ($MCSTAS::mcstas_config{'PLOTTER'} == 3 || $MCSTAS::mcstas_config{'PLOTTER'} == 4) {
	      # Line elements for Scilab struct
	      write_process("multiline([$1 @coords]);\n");
	    }
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
	    if ($MCSTAS::mcstas_config{'PLOTTER'} == 1 || $MCSTAS::mcstas_config{'PLOTTER'} == 2) {
	      # Line elements for Matlab struct, circle representation
	      write_process("coords=[@coords];\n");
	      write_process("x=coords(1:3:length(coords));\n");
	      write_process("y=coords(2:3:length(coords));\n");
	      write_process("z=coords(3:3:length(coords));\n");
	      write_process("coords=[x;y;z;1+0*z];\n");
	      write_process("INSTRUMENT.$comp.K{size(INSTRUMENT.$comp.K,2)+1}=coords;\n");
	    }
	    if ($MCSTAS::mcstas_config{'PLOTTER'} == 3 || $MCSTAS::mcstas_config{'PLOTTER'} == 4) {
	      # Line elements for Scilab struct, circle representation
	      write_process("circle('$plane',$x,$y,$z,$r);\n");
	    }
        } elsif($st == 2 && /^MCDISPLAY: end$/) {
	    $st = 1;  # End of component graphics representation
	    if ($MCSTAS::mcstas_config{'PLOTTER'} == 1 || $MCSTAS::mcstas_config{'PLOTTER'} == 2) {
	      # Matlab 'End of instrument'
	      write_process("mcdisplay('Load');\n");
	      write_process("PlotInstrument('init');\n");
	      # Check if we were called with --save option, output matlab figure if so...
	      if ($save) {
		# Clone the graph to another window...
		write_process("ax=gca;\n");
		write_process("h=figure('numbertitle','off','name','$sim_cmd McStas Instrument')\n;");
		write_process("copyobj(ax,h);\n");
		write_process("saveas(h,'$sim_cmd.fig','fig');\n");
		write_process("delete(h);\n");
	      } else {
		write_process("wait(INSTRUMENT.fig);\n");
	      }
	    }
	    if ($MCSTAS::mcstas_config{'PLOTTER'} == 3 || $MCSTAS::mcstas_config{'PLOTTER'} == 4) {
	      # Scilab 'End of instrument'
	      write_process("endtrace();\n");
	    }
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
    my ($comp, $numcomp, $EndFlag);

    $EndFlag = 0;
    $st = 0;
    $i = 0;
    $numcomp = 0;
    $EndFlag = 0;
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
	} elsif (/^Detector:/){
	  if ($MCSTAS::mcstas_config{'PLOTTER'} == 1 || $MCSTAS::mcstas_config{'PLOTTER'} == 3) {
	    # Should only be done if finished, and not called with --save flag...
	    if ($EndFlag == 0 && !$save) {
	      my $main = new MainWindow;
	      $main->Label(-text => 'Simulation ended.'
			  )->pack;
	      $main->Label(-text => 'Press Ok to terminate backend'
			  )->pack;
	      $main->Button(-text => 'Ok',
			    -command => sub{destroy $main}
			   )->pack;
	      MainLoop;
	      $EndFlag = 1;
	    }
	  }
	  print;
        } else {
            # Pass on any output not meant for us.
            print;
        }
    }
    exit unless $st == 2;        # Stop when EOF seen before neutron data end.
    
    my %neutron = ('x' => \@x, 'y' => \@y, z => \@z,
                   vx => \@vx, vy => \@vy, vz => \@vz,
                   t => \@t, ph1 => \@ph1, ph2 => \@ph2,
                   comp => \@ncomp, numcomp => $numcomp, EndFlag => $EndFlag);
    return %neutron
}
    

sub plot_components {
    my ($rx, $ry, $rori, $rdis, $axis1, $axis2) = @_;
    my (@x, @y, @ori);
    my ($i, $col);

    @x = @$rx;
    @y = @$ry;
    @ori = @$rori;

    PGPLOT::pgsci(2);
    PGPLOT::pgline($#x + 1, \@x, \@y);
    PGPLOT::pgpt($#x + 1, \@x, \@y, 20);
    $col = 4;
    for($i = 0; $i <= $#components; $i++) {
        my $comp = $components[$i];
        PGPLOT::pgsci($col++);
        $col = 4 if $col > 15;
        if($compdraw{$comp}) {
            foreach $elem (@{$compdraw{$comp}{'elems'}}) {
                if($elem->{'type'} eq 'multiline') {
                    PGPLOT::pgline($elem->{'count'}, $elem->{$axis1}, $elem->{$axis2});
                }
            }
        } else {
            PGPLOT::pgsch(1.4);
            PGPLOT::pgpt(1, $x[$i], $y[$i], 26);
        }
    }
}


sub plot_neutron {
    my ($x, $y, $z, $vx, $vy, $vz, $comp) = @_;
    my ($i, $col, $oldcomp, $retval);
    if ($MCSTAS::mcstas_config{'PLOTTER'} == 0) {
      # PGPLOT only
      PGPLOT::pgsci(3);
      PGPLOT::pgline(scalar(@$x), $x, $y);
      # Show component entry/exit points in same colour as respective component.
      # This should also be done w/ Matlab/Scilab...
      $i = 0;
      $col = 4;
      while($i < scalar(@$x)) {
        if(!defined($oldcomp) || $oldcomp cmp $comp->[$i]) {
	  $oldcomp = $comp->[$i];
            PGPLOT::pgsci($col++);
	  $col = 4 if $col > 15;
        }
        # Exit point.
        PGPLOT::pgpt(1, $x->[$i], $y->[$i], 17);
        $i++;
      }
    } elsif ($MCSTAS::mcstas_config{'PLOTTER'} == 1 || $MCSTAS::mcstas_config{'PLOTTER'} == 2) {
      # Matlab
      $retval=write_process("mcdisplay('Timeout');\n");
      $retval=write_process("mcdisplay('PlotNeutron',[@$x],[@$y],[@$z]);\n");
      return $retval;
    } elsif ($MCSTAS::mcstas_config{'PLOTTER'} == 3 || $MCSTAS::mcstas_config{'PLOTTER'} == 4) {
      # Scilab
      $retval=write_process("x=[];\n");
      $retval=write_process("y=[];\n");
      $retval=write_process("z=[];\n");
      $i=0;
      while($i < scalar(@$x)) {
	$tmp=$x->[$i];
	$retval=write_process("x=[x $tmp];\n");
	$tmp=$y->[$i];
	$retval=write_process("y=[y $tmp];\n");
	$tmp=$z->[$i];
	$retval=write_process("z=[z $tmp];\n");
	$i++;
      }
      $retval=write_process("PlotNeutron(x,y,z);\n");
      return $retval;
    }
}


sub show_comp_names {
    my ($rinstr) = @_;
    my %instr = %$rinstr;
    my @comps = @{$instr{'comp'}};
    my $count = @comps;
    $count = 8 if $count < 8;
    my $col = 4;
    PGPLOT::pgsch(25/$count);
    my $i;
    for($i = 0; $i < @comps; $i++) {
        PGPLOT::pgsci($col++);
        $col = 4 if $col > 15;
        PGPLOT::pgmtxt('RV', 0.2, 1 - ($i+0.5)/$count, 0.0, $comps[$i]);
    }
}


sub reset_zoom {
    my ($rinstr, $vps) = @_;
    $rinstr->{'zoom_xmin'} = $rinstr->{'xmin'};
    $rinstr->{'zoom_xmax'} = $rinstr->{'xmax'};
    $rinstr->{'zoom_ymin'} = $rinstr->{'ymin'};
    $rinstr->{'zoom_ymax'} = $rinstr->{'ymax'};
    $rinstr->{'zoom_zmin'} = $rinstr->{'zmin'};
    $rinstr->{'zoom_zmax'} = $rinstr->{'zmax'};
    $zooming = 0;
}


sub do_zoom {
    my ($rinstr, $vps, $cx, $cy, $cx1, $cy1) = @_;
    my ($tmp, $a1, $a2);
    $tmp = $cx, $cx = $cx1, $cx1 = $tmp if $cx > $cx1;
    $tmp = $cy, $cy = $cy1, $cy1 = $tmp if $cy > $cy1;

    if($cx == $cx1 || $cy == $cy1) {
        print STDERR "Warning: bad zoom area.\n";
        return;
    }
    if($multi_view) {
        # Convert from screen coordinates to world coordinates.
        # First find which of the three views was choosen.
        if($cx < 0 && $cy < 1) {
            $cx = $cx + 1;
            $cx1 = $cx1 + 1;
            ($a1,$a2) = ("z", "y");
        } elsif($cx < 0 && $cy >= 1) {
            $cx = $cx + 1;
            $cx1 = $cx1 + 1;
            $cy = $cy - 1;
            $cy1 = $cy1 - 1;
            ($a1,$a2) = ("z", "x");
        } elsif($cx >= 0 && $cy >= 1) {
            $cy = $cy - 1;
            $cy1 = $cy1 - 1;
            ($a1,$a2) = ("x", "y");
        } else {
            print STDERR "Warning: bad zoom area.\n";
            return;
        }
        my $idx = "$a1-$a2";
        my $vpx0 = $vps->{$idx}{'VP'}[0];
        my $vpdx = $vps->{$idx}{'VP'}[1] - $vpx0;
        my $wx0 = $vps->{$idx}{'W'}[0];
        my $wdx = $vps->{$idx}{'W'}[1] - $wx0;
        my $vpy0 = $vps->{$idx}{'VP'}[2];
        my $vpdy = $vps->{$idx}{'VP'}[3] - $vpy0;
        my $wy0 = $vps->{$idx}{'W'}[2];
        my $wdy = $vps->{$idx}{'W'}[3] - $wy0;
        $cx = ($cx-$vpx0)/$vpdx*$wdx+$wx0;
        $cx1 = ($cx1-$vpx0)/$vpdx*$wdx+$wx0;
        $cy = ($cy-$vpy0)/$vpdy*$wdy+$wy0;
        $cy1 = ($cy1-$vpy0)/$vpdy*$wdy+$wy0;
    } else {
        ($a1, $a2) = ("z","x");
    }
    $rinstr->{"zoom_${a1}min"} = $cx;
    $rinstr->{"zoom_${a1}max"} = $cx1;
    $rinstr->{"zoom_${a2}min"} = $cy;
    $rinstr->{"zoom_${a2}max"} = $cy1;
    $zooming = 1;
}

sub write_process {
  my ($command) = @_;
  # $pid == 0 covers file output, Scilab/Matlab + Matlab on Win32 (OLE)
  if (!$pid eq 0) {
    (kill 0, $pid) || print STDERR "$plotter process terminated - ending...\n";
    return 2;
  }
  if ($Config{'osname'} eq 'MSWin32' && $MCSTAS::mcstas_config{'PLOTTER'} eq 1) {
    $ML->Execute($command);
    my $err = Win32::OLE::LastError();
    if ($err) {
      print STDERR "Matlab terminated! - Exiting.\n";
      return 2;
    }
  } else { 
    # Simply write data to pipe/file
    print WRITER $command;
    return 1;
  }
}


sub plot_instrument {
    my ($noninteractive, $rinstr, $rneutron) = @_;
    my %instr = %$rinstr;
    my %neutron = %$rneutron;
    my $retval;
    # The following only relevant in PGPLOT mode
    if ($MCSTAS::mcstas_config{'PLOTTER'} == 0) {
      my ($xmin, $xmax, $ymin, $ymax, $zmin, $zmax) =
        ($instr{'zoom_xmin'}, $instr{'zoom_xmax'}, $instr{'zoom_ymin'},
         $instr{'zoom_ymax'}, $instr{'zoom_zmin'}, $instr{'zoom_zmax'});
      my %vps;                        # Viewport/window setup.
      my ($vpx1,$vpx2,$vpy1,$vpy2,$wx1,$wx2,$wy1,$wy2);
      
      PGPLOT::pgbbuf;
      
      # First show instrument from "above" (view in direction of y axis).
      PGPLOT::pgsci(1);
      PGPLOT::pgsch(1.4);
      PGPLOT::pgenv($zmin, $zmax, $xmin, $xmax, ($zooming ? 0 : 1), 0);
      PGPLOT::pglab("Z Axis [m]", "X Axis [m]", "Z-X view");
      show_comp_names($rinstr);
      PGPLOT::pgsch(1.4);
      plot_components($instr{'z'}, $instr{'x'}, $instr{'ori'}, $instr{'dis'},
		      'Z', 'X');
      plot_neutron($neutron{'z'}, $neutron{'x'}, $neutron{'y'},
		   $neutron{'vz'}, $neutron{'vx'}, $neutron{'vy'},$neutron{'comp'});
      
      if($multi_view) {
        # Remember viewport setup for Z-X view.
        PGPLOT::pgqvp(0, $vpx1, $vpx2, $vpy1, $vpy2);
        PGPLOT::pgqwin($wx1, $wx2, $wy1, $wy2);
        $vps{'z-x'} = {VP => [$vpx1,$vpx2,$vpy1,$vpy2],
                       W => [$wx1,$wx2,$wy1,$wy2]};
	
        # Now show instrument viewed in direction of z axis.
        PGPLOT::pgsci(1);
        PGPLOT::pgsch(1.4);
        PGPLOT::pgenv($xmin, $xmax, $ymin, $ymax, ($zooming ? 0 : 1), 0);
        PGPLOT::pglab("X Axis [m]", "Y Axis [m]", "X-Y view");
        plot_components($instr{'x'}, $instr{'y'}, $instr{'ori'}, $instr{'dis'},
                        'X', 'Y');
        plot_neutron($neutron{'x'}, $neutron{'y'}, $neutron{'z'},
                     $neutron{'vx'}, $neutron{'vy'}, $neutron{'vz'} ,$neutron{'comp'});
        # Remember viewport setup for Z-X view.
        PGPLOT::pgqvp(0, $vpx1, $vpx2, $vpy1, $vpy2);
        PGPLOT::pgqwin($wx1, $wx2, $wy1, $wy2);
        $vps{'x-y'} = {VP => [$vpx1,$vpx2,$vpy1,$vpy2],
                       W => [$wx1,$wx2,$wy1,$wy2]};
	
        # Now show instrument viewed in direction of x axis.
        PGPLOT::pgsci(1);
        PGPLOT::pgsch(1.4);
        PGPLOT::pgenv($zmin, $zmax, $ymin, $ymax, ($zooming ? 0 : 1), 0);
        PGPLOT::pglab("Z Axis [m]", "Y Axis [m]", "Z-Y view");
        plot_components($instr{'z'}, $instr{'y'}, $instr{'ori'}, $instr{'dis'},
                        'Z', 'Y');
        plot_neutron($neutron{'z'}, $neutron{'y'}, $neutron{'x'},
                     $neutron{'vz'}, $neutron{'vy'}, $neutron{'vx'}, $neutron{'comp'});
        # Remember viewport setup for Z-Y view.
        PGPLOT::pgqvp(0, $vpx1, $vpx2, $vpy1, $vpy2);
        PGPLOT::pgqwin($wx1, $wx2, $wy1, $wy2);
        $vps{'z-y'} = {VP => [$vpx1,$vpx2,$vpy1,$vpy2],
                       W => [$wx1,$wx2,$wy1,$wy2]};
	
        # Set up viewport & window for mouse zoom.
        PGPLOT::pgpage;
        PGPLOT::pgsvp(0,1,0,1);
        PGPLOT::pgswin(0,1,0,1);
      }
      PGPLOT::pgebuf;
      
      return 0 if $noninteractive;
      
      # Now wait for a keypress in the graphics window.
      my ($cx, $cy, $cc);
      $cx = $cy = 0;
      PGPLOT::pgband(0, 0, 0, 0, $cx, $cy, $cc);
      if($cc =~ /[qQ]/) {
        return 2;                # Finished.
      } elsif($cc =~ /[pP]/) {        # B&W hardcopy.
        return 3;
      } elsif($cc =~ /[cC]/) {        # color hardcopy.
        return 4;
      } elsif($cc =~ /[gG]/) {        # GIF hardcopy.
        return 5;
      } elsif($cc =~ /[hH]/) {        # Help
        print STDERR "McDisplay: q=quit, h=help\n";
        print STDERR "    output p=ps,   c=color ps, g=gif\n";
        print STDERR "      zoom x=reset,z or d=zoom\n";
      } elsif($cc =~ /[zZdD]/) {        # Zoom.
        my ($cx1, $cy1, $cc1) = (0, 0, 0);
        PGPLOT::pgband(2,0,$cx,$cy,$cx1,$cy1,$cc1);
        do_zoom($rinstr, \%vps, $cx, $cy, $cx1, $cy1);
        return 1;
      } elsif($cc =~ /[xX]/) {        # Reset zoom.
        reset_zoom($rinstr, \%vps);
        return 1;
      }
    } else {
      # Leave further checks for plot_neutron
      $retval=plot_neutron($neutron{'z'}, $neutron{'x'}, $neutron{'y'}, 
		   $neutron{'vz'}, $neutron{'vx'}, $neutron{'vy'}, $neutron{'comp'});
      if ($retval==2) {
	return $retval;
      }
    }
    return 0;                        # Default: do not repeat this neutron.
}

sub get_device {
    my ($what) = @_;
    my $dev;
    
    if (defined(&dev)) { $dev = dev($what); }
    else { $dev = PGPLOT::pgopen($what); }
    return $dev if $dev < 0;
    if($multi_view) {
    # We use a 2x2 display format to view the instrument from three angles.
        PGPLOT::pgsubp(2, 2);
    } else {
        # We use a 1x1 display for detail.
        PGPLOT::pgsubp(1, 1);
    }
    return $dev;
}


# Test the code.

# Attempt to locate pgplot directory if unset.
$ENV{'PGPLOT_DIR'} = "/usr/local/pgplot" unless $ENV{'PGPLOT_DIR'};

# Check command line arguments.

undef $inspect;
undef $first;
undef $last;
undef $save;
undef $direct_output;
undef $sim_cmd;
undef $plotter;
undef $file_output;
my $int_mode=0; # interactive mode(0), non interactive (1)
my $i;

for($i = 0; $i < @ARGV; $i++) {
    if(($ARGV[$i] eq "-m") || ($ARGV[$i] eq "--multi")) {
        $multi_view = 1;
    } elsif(($ARGV[$i] =~ /^-z([-0-9+.eE]+)$/) ||
            ($ARGV[$i] =~ /^--zoom=([-0-9+.eE]+)$/)) {
        $magnification = ($1 == 0 ? 1 : $1);
    } elsif(($ARGV[$i] eq "-gif") || ($ARGV[$i] eq "-ps") ||
            ($ARGV[$i] eq "-psc")) {
        $direct_output = $ARGV[$i];
        $int_mode = 1;
    } elsif(($ARGV[$i] =~ /^-i([a-zA-ZæøåÆØÅ0-9_]+)$/) ||
            ($ARGV[$i] =~ /^--inspect=([a-zA-ZæøåÆØÅ0-9_]+)$/)) {
        $inspect = $1;
    } elsif($ARGV[$i] =~ /^--first=([a-zA-ZæøåÆØÅ0-9_]+)$/) {
        $first = $1;
    } elsif($ARGV[$i] =~ /^--last=([a-zA-ZæøåÆØÅ0-9_]+)$/) {
        $last = $1;
    } elsif($ARGV[$i] eq "--save") {
        $save = 1;
    } elsif(($ARGV[$i] =~ /^-p([a-zA-ZæøåÆØÅ0-9_]+)$/) ||
	      ($ARGV[$i] =~ /^--plotter=([a-zA-ZæøåÆØÅ0-9_]+)$/)) {
        $plotter = $1;	
   } elsif(($ARGV[$i] =~ /^-f([a-zA-ZæøåÆØÅ0-9_\/\.\:]+)$/) ||
	      ($ARGV[$i] =~ /^--file=([a-zA-ZæøåÆØÅ0-9_\/\.\:]+)$/)) {
        $file_output = $1;	
   } else {
        if (defined($sim_cmd)) { push @cmdline, $ARGV[$i]; }
        else { $sim_cmd = $ARGV[$i]; }
   }
}
die "Usage: mcdisplay [-mzipf][-gif|-ps|-psc] Instr.out [instr_options] params
 -m        --multi           Show the three instrument side views
 -zZF      --zoom=ZF         Show zoomed view by factor ZF
 -iCOMP    --inspect=COMP    Show only trajectories reaching component COMP
 -pPLOTTER --plotter=PLOTTER Output graphics using {PGPLOT,Scilab,Matlab}
 -fFNAME   --file=FNAME      Outout graphcis commands to file FNAME
                             (Only used when PLOTTER = {Scilab, Matlab})
           --first=COMP      First component to visualize {Scilab, Matlab}
           --last=COMP       Last component to visualize {Scilab, Matlab}
           --save            Output a Scilab/Matlab figure file and exit
                             (Filename is Instr.scf / Instr.fig). Figure
                             files are used by mcgui.pl for visualising the
                             instrument. With PGPLOT, --save is nonfunctional.
 -gif|-ps|-psc               Export figure as gif/b&w ps/color ps and exit
 When using -ps -psc -gif, the program writes the hardcopy file
 and then exits (plotter PGPLOT only).
SEE ALSO: mcstas, mcplot, mcrun, mcresplot, mcstas2vitess, mcgui\n"
 unless $sim_cmd;

# Check value of $plotter and $file_output variables, set 
# $MCSTAS::mcstas_config{'PLOTTER'}
# accordingly
if ($plotter eq "PGPLOT") {
  $MCSTAS::mcstas_config{'PLOTTER'}=0;
} elsif ($plotter eq "Matlab") {
  $MCSTAS::mcstas_config{'PLOTTER'}=1;
  if ($file_output) {
    $MCSTAS::mcstas_config{'PLOTTER'}=2;
  }
} elsif ($plotter eq "Scilab") {
  $MCSTAS::mcstas_config{'PLOTTER'}=3;  
  if ($file_output) {
    $MCSTAS::mcstas_config{'PLOTTER'}=4;
  } else {
    # Check for Win32, only file_output possible :(
    if ($Config{'osname'} eq MSWin32) {
      print STDERR "\n******************************************************\n";
      print STDERR "Sorry, Scilab only possible using file output on Win32\n\n";     print STDERR "Outputting to file mcdisplay_commands.sci\n";
      print STDERR "******************************************************\n\n";
      $file_output="mcdisplay_commands.sci";
      $MCSTAS::mcstas_config{'PLOTTER'}=4;
    }
  }
}

# Final PLOTTER check, is PGPLOT wanted but not possible?
# - Ask user to rerun / set other default
if ($MCSTAS::mcstas_config{'PLOTTER'} eq 0 && $pg_avail eq 0) {
  print STDERR "\n******************************************************\n";
  print STDERR "Default / selected PLOTTER is PGPLOT - Problems:\n\n";
  print STDERR "PGPLOT.pm not found on Perl \@INC path\n\nSolution:\n\n";
  print STDERR "1) Install pgplot + pgperl packages (Unix/Linux/Cygwin) \n";
  print STDERR "2) Rerun mcdisplay with -p/--plotter set to Scilab/Matlab \n";
  print STDERR "3) Modify $MCSTAS::perl_dir/mcstas_config.perl\n";
  print STDERR "   to set a different default plotter\n\n";
  print STDERR "******************************************************\n\n";
  die "PGPLOT problems...\n";
}

# Only set up PGPLOT stuff if needed
if ($MCSTAS::mcstas_config{'PLOTTER'} eq 0) {
  # PGPLOT is plotter!
  my $pg_devname = "/xserv";
  if ($int_mode == 1) 
    { 
      my $ext  = "ps";
      my $type = "ps";
      if($direct_output eq "-gif") { $ext="gif"; $type="gif"; }
      elsif($direct_output eq "-psc") { $type="cps"; }
      $pg_devname = "$sim_cmd.$ext/$type"; 
    }
  my $global_device = get_device($pg_devname);
  if($global_device < 0) {
    print STDERR "Failed to open PGPLOT device $pg_devname\n";
    exit 1;
  }
  my $seq = "";                        # Sequence number for multiple hardcopy
  PGPLOT::pgask(0);
} elsif ($MCSTAS::mcstas_config{'PLOTTER'} eq 1) {
  # Matlab is plotter - open a pipe / OLE connection
  if ($Config{'osname'} eq 'MSWin32') {
    $pid=0;
    $ML = Win32::OLE->new('Matlab.Application') || die "Could not start Matlab\n";
  } else {
    $pid=open2(READER,WRITER, $MCSTAS::mcstas_config{'MATLAB_COMMAND'}) || die "Could not start Matlab\n";
  }
  print STDERR "Opened up pipe to matlab - pid is $pid\n";
  print STDERR "Building Matlab INSTRUMENT struct, please have patience...\n";
} elsif ($MCSTAS::mcstas_config{'PLOTTER'} eq 2) {
  # Matlab w/FILE is plotter - open a file handle
  open(WRITER, "> $file_output");
  $pid=0;
} elsif ($MCSTAS::mcstas_config{'PLOTTER'} eq 3) {
  # Scilab is plotter - open a pipe
  $pid=open2(READER,WRITER, $MCSTAS::mcstas_config{'SCILAB_COMMAND'}) || die "Could not start Scilab\n";
  print STDERR "Opened up pipe to scilab - pid is $pid\n";
  print STDERR "Building Scilab INSTRUMENT struct, please have patience...\n";
} elsif ($MCSTAS::mcstas_config{'PLOTTER'} eq 4) {
  # Scilab w/FILE is plotter - open a file handle
  open(WRITER, "> $file_output");
  $pid=0;
}

my ($numcomp, %neutron, %instr);

$args = join(" ", @cmdline);
$cmdline = "$sim_cmd --trace $args";
printf STDERR "Starting simulation '$cmdline' ...\n";
open(IN, "$cmdline |") || die "Could not run simulation\n";

$numcomp = read_instrument(IN);
$inspect_pos = get_inspect_pos($inspect, @components);
%instr = make_instrument;

if ($int_mode == 0 && $MCSTAS::mcstas_config{'PLOTTER'} == 0)  { printf STDERR "Press H key for help.\n"; }
if ($MCSTAS::mcstas_config{'PLOTTER'} == 1) { print STDERR "Matlab INSTRUMENT done, starting gui....\n"; }
if ($MCSTAS::mcstas_config{'PLOTTER'} == 3) { print STDERR "Scilab INSTRUMENT done, starting gui....\n"; }

while(!eof(IN)) {
    %neutron = read_neutron(IN);

    next if $neutron{'numcomp'} <= $inspect_pos;

    my $ret;
    do {
        $ret = plot_instrument($int_mode, \%instr, \%neutron);
	if ($MCSTAS::mcstas_config{'PLOTTER'} == 0) {
	  if ($int_mode == 1) { $ret =2; print STDERR "Wrote \"$pg_devname\"\n"; }
	  if($ret == 3 || $ret == 4 || $ret == 5) {
            my $ext="ps";
            my $type = $ret == 3 ? "ps" : "cps";
            if($ret == 5) { $type = "gif"; $ext="gif"; }
            my $tmp_pg_devname = "$sim_cmd$seq.$ext/$type";
            my $tmpdev=0;
            $tmpdev = get_device($tmp_pg_devname);
            if($tmpdev < 0) {
	      print STDERR "Warning: could not open PGPLOT output \"$tmp_pg_devname\" for hardcopy output\n";
            } else {
	      plot_instrument(1, \%instr, \%neutron);
	      print STDERR "Wrote \"$tmp_pg_devname\"\n";
	      ++$seq;
            }
            if (defined(&close_window)) { close_window(); }
            else { PGPLOT::pgclos(); };
            PGPLOT::pgslct($global_device);
	  }
	}
    } while($ret != 0 && $ret != 2);
    last if $ret == 2;
  }
close(IN);

# Properly close any open files etc.
if ($MCSTAS::mcstas_config{'PLOTTER'} eq 0) {
  if (defined(&close_window)) { 
    close_window(); 
  }
  else { 
    PGPLOT::pgclos(); 
  }
} else {
  close(WRITER);
}
