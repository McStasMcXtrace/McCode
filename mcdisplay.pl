#! /usr/bin/perl -w

# In emacs, please make this -*- perl -*- mode. Thanks.


use PGPLOT;

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
	} elsif($st == 1 && /^COMPONENT:\s*"([a-zA-Z0-9_Ê¯Â∆ÿ≈]+)"\s*/) {
	    $comp = $1;
	    @components = (@components, $comp);
	} elsif($st == 1 && /^POS:(.*)$/) {
	    my @T;
	    @T = split ",", $1;
	    $transformations{$comp} = \@T;
	} elsif($st == 1 && /^MCDISPLAY: start$/) {
	    $st = 2;		# Start of component graphics representation
	} elsif($st == 2 && /^MCDISPLAY: component ([a-zA-Z0-9_Ê¯Â∆ÿ≈]+)/) {
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
	} elsif($st == 2 && /^MCDISPLAY: end$/) {
	    $st = 1;		# End of component graphics representation
	} elsif($st == 1 && /^INSTRUMENT END:/) {
	    $st = 100;
	    last;
	} else {
	    print;
	}
    }
    exit if($st != 100);	# Stop when EOF seen before instrument end.
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
    my $comp;

    $st = 0;
    $i = 0;
    my $dropit = 1;		# Flag to drop uninteresting neutron states.
    while(<$in>) {
	if($st == 0 && /^ENTER:/) {
	    # Neutron enters instrument.
	    $st = 1;
	} elsif($st == 0 && /^STATE:/) {
	    # State after leaving - should probably be removed in McStas.
	    next;
	} elsif($st == 1 && /^COMP:\s*"([a-zA-Z0-9_Ê¯Â∆ÿ≈]+)"\s*$/) {
	    # Neutron enters component local coordinate system.
	    $comp = $1;
	    $dropit = 1;	# Drop the first state (entry point).
	} elsif($st == 1 && /^STATE:(.*)$/) {
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
	    next;		# No special action needed.
	} elsif($st == 1 && /^LEAVE:/) {
	    # Neutron leaves instrument.
	    $st = 2;
	    last;
	} else {
	    # Pass on any output not meant for us.
	    print;
	}
    }
    exit unless $st == 2;	# Stop when EOF seen before neutron data end.

    my %neutron = ('x' => \@x, 'y' => \@y, z => \@z,
		   vx => \@vx, vy => \@vy, vz => \@vz,
		   t => \@t, ph1 => \@ph1, ph2 => \@ph2, comp => \@ncomp);
    return %neutron
}
    

sub plot_components {
    my ($rx, $ry, $rori, $rdis, $axis1, $axis2) = @_;
    my (@x, @y, @ori);
    my ($i, $col);

    @x = @$rx;
    @y = @$ry;
    @ori = @$rori;

    pgsci(2);
    pgline($#x + 1, \@x, \@y);
    pgpt($#x + 1, \@x, \@y, 20);
    $col = 4;
    for($i = 0; $i <= $#components; $i++) {
	my $comp = $components[$i];
	pgsci($col++);
	$col = 4 if $col > 15;
	if($compdraw{$comp}) {
	    foreach $elem (@{$compdraw{$comp}{'elems'}}) {
		if($elem->{'type'} eq 'multiline') {
		    pgline($elem->{'count'}, $elem->{$axis1}, $elem->{$axis2});
		}
	    }
	} else {
	    pgsch(1.4);
	    pgpt(1, $x[$i], $y[$i], 26);
	}
    }
}


sub plot_neutron {
    my ($rx, $ry, $rvx, $rvy) = @_;
    my (@x, @y);
    my ($i, $col);

    @x = @$rx;
    @y = @$ry;
    pgsci(3);
    pgline($#x + 1, \@x, \@y);
    # Show component entry/exit points in same colour as respective component.
    $i = 0;
    $col = 4;
    while($i <= $#x) {
	pgsci($col++);
	$col = 4 if $col > 15;
	# Exit point.
	pgpt(1, $x[$i], $y[$i], 17);
	$i++;
    }
}


sub show_comp_names {
    my ($rinstr) = @_;
    my %instr = %$rinstr;
    my @comps = @{$instr{'comp'}};
    my $count = @comps;
    $count = 8 if $count < 8;
    my $col = 4;
    pgsch(25/$count);
    my $i;
    for($i = 0; $i < @comps; $i++) {
	pgsci($col++);
	$col = 4 if $col > 15;
	pgmtxt('RV', 0.2, 1 - ($i+0.5)/$count, 0.0, $comps[$i]);
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


sub plot_instrument {
    my ($rinstr, $rneutron) = @_;
    my %instr = %$rinstr;
    my %neutron = %$rneutron;
    my ($xmin, $xmax, $ymin, $ymax, $zmin, $zmax) =
	($instr{'zoom_xmin'}, $instr{'zoom_xmax'}, $instr{'zoom_ymin'},
	 $instr{'zoom_ymax'}, $instr{'zoom_zmin'}, $instr{'zoom_zmax'});
    my %vps;			# Viewport/window setup.
    my ($vpx1,$vpx2,$vpy1,$vpy2,$wx1,$wx2,$wy1,$wy2);

    pgbbuf;

    # First show instrument from "above" (view in direction of y axis).
    pgsci(1);
    pgsch(1.4);
    pgenv($zmin, $zmax, $xmin, $xmax, ($zooming ? 0 : 1), 0);
    pglab("Z Axis [m]", "X Axis [m]", "Z-X view");
    show_comp_names($rinstr);
    pgsch(1.4);
    plot_components($instr{'z'}, $instr{'x'}, $instr{'ori'}, $instr{'dis'},
		    'Z', 'X');
    plot_neutron($neutron{'z'}, $neutron{'x'}, $neutron{'vz'}, $neutron{'vx'});

    if($multi_view) {
	# Remember viewport setup for Z-X view.
	pgqvp(0, $vpx1, $vpx2, $vpy1, $vpy2);
	pgqwin($wx1, $wx2, $wy1, $wy2);
	$vps{'z-x'} = {VP => [$vpx1,$vpx2,$vpy1,$vpy2],
		       W => [$wx1,$wx2,$wy1,$wy2]};

	# Now show instrument viewed in direction of z axis.
	pgsci(1);
	pgsch(1.4);
	pgenv($xmin, $xmax, $ymin, $ymax, ($zooming ? 0 : 1), 0);
	pglab("X Axis [m]", "Y Axis [m]", "X-Y view");
	plot_components($instr{'x'}, $instr{'y'}, $instr{'ori'}, $instr{'dis'},
			'X', 'Y');
	plot_neutron($neutron{'x'}, $neutron{'y'}, $neutron{'vx'}, $neutron{'vy'});
	# Remember viewport setup for Z-X view.
	pgqvp(0, $vpx1, $vpx2, $vpy1, $vpy2);
	pgqwin($wx1, $wx2, $wy1, $wy2);
	$vps{'x-y'} = {VP => [$vpx1,$vpx2,$vpy1,$vpy2],
		       W => [$wx1,$wx2,$wy1,$wy2]};

	# Now show instrument viewed in direction of x axis.
	pgsci(1);
	pgsch(1.4);
	pgenv($zmin, $zmax, $ymin, $ymax, ($zooming ? 0 : 1), 0);
	pglab("Z Axis [m]", "Y Axis [m]", "Z-Y view");
	plot_components($instr{'z'}, $instr{'y'}, $instr{'ori'}, $instr{'dis'},
			'Z', 'Y');
	plot_neutron($neutron{'z'}, $neutron{'y'}, $neutron{'vz'}, $neutron{'vy'});
	# Remember viewport setup for Z-Y view.
	pgqvp(0, $vpx1, $vpx2, $vpy1, $vpy2);
	pgqwin($wx1, $wx2, $wy1, $wy2);
	$vps{'z-y'} = {VP => [$vpx1,$vpx2,$vpy1,$vpy2],
		       W => [$wx1,$wx2,$wy1,$wy2]};

	# Set up viewport & window for mouse zoom.
	pgpage;
	pgsvp(0,1,0,1);
	pgswin(0,1,0,1);
    }
    pgebuf;

    # Now wait for a keypress in the graphics window.
    my ($cx, $cy, $cc);
    $cx = $cy = 0;
    pgband(0, 0, 0, 0, $cx, $cy, $cc);
    if($cc =~ /[qQ]/) {
	exit 0;			# Finished.
    } elsif($cc =~ /[zZdD]/) {	# Zoom.
	my ($cx1, $cy1, $cc1) = (0, 0, 0);
	pgband(2,0,$cx,$cy,$cx1,$cy1,$cc1);
	do_zoom($rinstr, \%vps, $cx, $cy, $cx1, $cy1);
	return 1;
    } elsif($cc =~ /[xX]/) {	# Reset zoom.
	reset_zoom($rinstr, \%vps);
	return 1;
    }
    return 0;			# Default: do not repeat this neutron.
}



# Test the code.

# Attempt to locate pgplot directory if unset.
$ENV{'PGPLOT_DIR'} = "/usr/local/pgplot" unless $ENV{'PGPLOT_DIR'};

# Check command line arguments.

undef $inspect;
for(;;) {
    if(($ARGV[0] eq "-m") || ($ARGV[0] eq "--multi")) {
	$multi_view = 1;
	shift;
    } elsif(($ARGV[0] =~ /^-z([-0-9+.eE]+)$/) ||
	    ($ARGV[0] =~ /^--zoom=([-0-9+.eE]+)$/)) {
	$magnification = ($1 == 0 ? 1 : $1);
	shift;
    } elsif(($ARGV[0] =~ /^-i([a-zA-ZÊ¯Â∆ÿ≈0-9_]+)$/) ||
	    ($ARGV[0] =~ /^--inspect=([a-zA-ZÊ¯Â∆ÿ≈0-9_]+)$/)) {
	$inspect = $1;
	shift;
    } else {
	last;
    }
}

if($multi_view) {
    # We use a 2x2 display format to view the instrument from three angles.
    pgbegin(0, "/xserv", 2, 2);
} else {
    # We use a 1x1 display for detail.
    pgbegin(0, "/xserv", 1, 1);
}
pgask(0);

my ($numcomp, %neutron, %instr);


$sim_cmd = shift;
$args = join(" ", @ARGV);
$cmdline = "$sim_cmd --trace $args";
printf STDERR "Starting simulation '$cmdline' ...\n";
open(IN, "$cmdline |") || die "Could not run simulation\n";

$numcomp = read_instrument(IN);
$inspect_pos = get_inspect_pos($inspect, @components);
%instr = make_instrument;

while(!eof(IN)) {
    %neutron = read_neutron(IN);
    next if @{$neutron{'comp'}} <= $inspect_pos;

    while(plot_instrument(\%instr, \%neutron))
    { }
}

close(IN);

pgend;
