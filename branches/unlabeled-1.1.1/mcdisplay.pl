#!/usr/bin/perl -w

# In emacs, please make this -*- perl -*- mode. Thanks.


use PGPLOT;


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
	} elsif($st == 1 && /^COMPONENT:\s*"([a-zA-Z0-9_]+)"\s*/) {
	    $comp = $1;
	    @components = (@components, $comp);
	} elsif($st == 1 && /^POS:(.*)$/) {
	    my @T;
	    @T = split ",", $1;
	    $transformations{$comp} = \@T;
	} elsif($st == 1 && /^INSTRUMENT END:/) {
	    $st = 2;
	    last;
	} else {
	    print;
	}
    }
    exit if($st != 2);		# Stop when EOF seen before instrument end.
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
	$i++;
    }
    %instr = ('x' => \@x, 'y' => \@y, z => \@z,
	      ori => \@ori, dis => \@dis, comp => \@comp,
	      xmin => $xmin, xmax => $xmax,
	      ymin => $ymin, ymax => $ymax,
	      zmin => $zmin, zmax => $zmax);
    return %instr;
}    
	

sub transform {
    my ($comp, $x, $y, $z, $vx, $vy, $vz, $t, $ph1, $ph2) = @_;
    if(!$comp) {
	return ($x, $y, $z, $vx, $vy, $vz, $t, $ph1, $ph2);
    } else {
	my ($nx, $ny, $nz, $nvx, $nvy, $nvz, $nt, $nph1, $nph2);
	my @T = @{$transformations{$comp}};
	$nx = $x*$T[3] + $y*$T[6] + $z*$T[9] + $T[0];
	$ny = $x*$T[4] + $y*$T[7] + $z*$T[10] + $T[1];
	$nz = $x*$T[5] + $y*$T[8] + $z*$T[11] + $T[2];
	$nvx = $vx*$T[3] + $vy*$T[6] + $vz*$T[9];
	$nvy = $vx*$T[4] + $vy*$T[7] + $vz*$T[10];
	$nvz = $vx*$T[5] + $vy*$T[8] + $vz*$T[11];
	return ($nx, $ny, $nz, $nvx, $nvy, $nvz, $t, $ph1, $ph2);
    }
}


sub read_neutron {
    my ($in) = @_;
    my (@x, @y, @z, @vx, @vy, @vz, @t, @ph1, @ph2, @ncomp);
    my ($st, $i);
    my $comp;

    $st = 0;
    $i = 0;
    while(<$in>) {
	if($st == 0 && /^ENTER:/) {
	    # Neutron enters instrument.
	    $st = 1;
	} elsif($st == 1 && /^COMP:\s*"([a-zA-Z0-9_]+)"\s*$/) {
	    # Neutron enters component local coordinate system.
	    $comp = $1;
	} elsif($st == 1 && /^STATE:(.*)$/) {
	    # Neutron state.
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
    my ($rx, $ry, $rori, $rdis) = @_;
    my (@x, @y, @ori);
    my ($i, $col);

    @x = @$rx;
    @y = @$ry;
    @ori = @$rori;

    pgsci(2);
    pgline($#x + 1, \@x, \@y);
    pgpt($#x + 1, \@x, \@y, 2);
    $col = 4;
    for($i = 0; $i <= $#components; $i++) {
	pgsci($col++);
	$col = 4 if $col > 15;
	pgsch(1.4);
	pgpt(1, $x[$i], $y[$i], 26);
#	pgsch(1.1);
#	pgtext($x[$i], $y[$i], $components[$i]);
    }
}


sub plot_neutron {
    my ($rx, $ry, $rvx, $rvy) = @_;
    my(@x, @y);
    my ($i, $col);

    @x = @$rx;
    @y = @$ry;
    pgsci(3);
    pgline($#x + 1, \@x, \@y);
    # Show component entry/exit points in same colour as respective component.
    $i = 0;
    pgsci(2);
    pgpt(1, $x[$i], $y[$i], 17); # First point.
    $i++;
    $col = 4;
    while($i <= $#x) {
	pgsci($col++);
	$col = 4 if $col > 15;
	# Entry point. Don't plot, since it usually coincides with last exit point.
	$i++;
	# Exit point.
	pgpt(1, $x[$i], $y[$i], 17);
	$i++;
    }
}


sub plot_instrument {
    my ($xmin, $xmax, $ymin, $ymax, $zmin, $zmax, $rinstr, $rneutron) = @_;
    my %instr = %$rinstr;
    my %neutron = %$rneutron;

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

    pgbbuf;

    # First show instrument from "above" (view in direction of y axis).
    pgsci(1);
    pgsch(1.4);
    pgenv($xmin, $xmax, $zmin, $zmax, 1, 0);
    pglab("X Axis", "Z Axis", "X-Z view");
    plot_components($instr{'x'}, $instr{'z'}, $instr{'ori'}, $instr{'dis'});
    plot_neutron($neutron{'x'}, $neutron{'z'}, $neutron{'vx'}, $neutron{'vz'});

    if($multi_view) {
	# Now show instrument viewed in direction of x axis.
	pgsci(1);
	pgsch(1.4);
	pgenv($ymin, $ymax, $zmin, $zmax, 1, 0);
	pglab("Y Axis", "Z Axis", "Y-Z view");
	plot_components($instr{'y'}, $instr{'z'}, $instr{'ori'}, $instr{'dis'});
	plot_neutron($neutron{'y'}, $neutron{'z'}, $neutron{'vy'}, $neutron{'vz'});

	# Now show instrument viewed in direction of z axis.
	pgsci(1);
	pgsch(1.4);
	pgenv($xmin, $xmax, $ymin, $ymax, 1, 0);
	pglab("X Axis", "Y Axis", "X-Y view");
	plot_components($instr{'x'}, $instr{'y'}, $instr{'ori'}, $instr{'dis'});
	plot_neutron($neutron{'x'}, $neutron{'y'}, $neutron{'vx'}, $neutron{'vy'});

	pgpage;
    }
    pgebuf;

    # Now wait for a keypress in the graphics window.
    my ($cx, $cy, $cc);
    $cx = $cy = 0;
    pgband(0, 0, 0, 0, $cx, $cy, $cc);
}



# Test the code.

# Attempt to locate pgplot directory if unset.
$ENV{'PGPLOT_DIR'} = "/usr/local/pgplot" unless $ENV{'PGPLOT_DIR'};

$multi_view = 1 if $ARGV[0];

if($multi_view) {
    # We use a 2x2 display format to view the instrument from three angles.
    pgbegin(0, "/xserv", 2, 2);
} else {
    # We use a 1x1 display for detail.
    pgbegin(0, "/xserv", 1, 1);
}
pgask(0);

my ($numcomp, %neutron, %instr);

$numcomp = read_instrument(STDIN);
print "Number of components: $numcomp\n";
%instr = make_instrument;

while(!eof(STDIN)) {
    %neutron = read_neutron(STDIN);

    plot_instrument($instr{'xmin'},$instr{'xmax'},$instr{'ymin'},
		    $instr{'ymax'},$instr{'zmin'},$instr{'zmax'},
		    \%instr, \%neutron);
}

pgend;
