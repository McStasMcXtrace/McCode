#! /usr/bin/perl -w

use FileHandle;
use PDL;
use PDL::Graphics::PGPLOT;
use PGPLOT;

sub strip_quote {
    my ($str) = @_;
    $str = $1 if($str =~ /^'(.*)'$/); # Remove quotes if present.
    return $str;
}
    
sub read_data_info {
    my ($handle) = @_;
    my ($type,$fname);
    my ($compname,$title,$xlabel,$ylabel) = ("","","","");
    my ($xmin,$xmax,$ymin,$ymax) = (0,1,0,1);
    while(<$handle>) {
	if(/^\s*type:\s*(.*?)\s*$/i) {
	    $type = $1;
	} elsif(/^\s*component:\s*([a-zA-ZæøåÆØÅ_0-9]+)\s*$/i) {
	    $compname = $1;
	} elsif(/^\s*title:\s*(.*?)\s*$/i) {
	    $title = strip_quote($1);
	} elsif(/^\s*filename:\s*(.*?)\s*$/i) {
	    $fname = strip_quote($1);
	} elsif(/^\s*xlabel:\s*(.*?)\s*$/i) {
	    $xlabel = strip_quote($1);
	} elsif(/^\s*ylabel:\s*(.*?)\s*$/i) {
	    $ylabel = strip_quote($1);
	} elsif(/^\s*xylimits:\s*
		([-+0-9.eE]+)\s+
		([-+0-9.eE]+)\s+
		([-+0-9.eE]+)\s+
		([-+0-9.eE]+)\s*$/ix) {
	    ($xmin,$xmax,$ymin,$ymax) = ($1,$2,$3,$4);
	} elsif(/^\s*xlimits:\s*
		([-+0-9.eE]+)\s+
		([-+0-9.eE]+)\s*$/ix) {
	    ($xmin,$xmax) = ($1,$2);
	} elsif(/^\s*end\s+data\s*$/i) {
	    last;
	} else {
	    die "Invalid line in siminfo file:\n'$_'";
	}
    }
    die "Missing type for component $compname"
	unless $type;
    die "Missing filename for component $compname"
	unless $fname;
    return { Type => $type,
	     Component => $compname,
	     Title => $title,
	     Filename => $fname,
	     Xlabel => $xlabel,
	     Ylabel => $ylabel,
	     Limits => [$xmin,$xmax,$ymin,$ymax]
	 };
}

sub read_sim_info {
    my ($handle) = @_;
    my @datalist = ();
    while(<$handle>) {
	if(/^\s*begin\s+data\s*$/i) {
	    push @datalist, read_data_info($handle);
	} elsif(/^\s*$/) {
	    next;
	} else {
	    die "Invalid line in siminfo file:\n'$_'";
	}
    }
    return \@datalist;
}

sub read_sim_file {
    my ($file) = @_;
    my $handle = new FileHandle;
    open $handle, $file or die "Could not open file '$file'";
    read_sim_info($handle);
}

sub plot_array_2d {
    my ($info,$m,$n) = @_;
    my $data = transpose(cat rcols $info->{'Filename'});
    my ($x0,$x1,$y0,$y1) = @{$info->{'Limits'}};
    my ($dx,$dy) = (($x1 - $x0)/$m, ($y1 - $y0)/$n);
    my $tr = pdl [ $x0 + $dx/2, $dx, 0, $y0 + $dy/2, 0, $dy ];
    my ($min, $max) = (min($data), max($data));
    my $numcol = 64;
    my $ramp = pdl [[ 0,  1/8,  3/8,  5/8,  7/8,  8/8],
		    [ 0,    0,    0,    1,    1,   .5],
		    [ 0,    0,    1,    1,    0,    0],
		    [.5,    1,    1,    0,    0,    0]];
    pgpage;
    pgbbuf;
    pgsci(1);
    hold;
    pgvstd;
    pgswin @{$info->{'Limits'}};
    pgbox("BCNSTI", 0.0, 0.0, "BCNSTI", 0.0, 0.0);
    pgscir(16,16+$numcol-1);
    ctab $ramp;
    imag $data, $min, $max, $tr;
    pgwedg("RI", 0.5, 3.0, $min, $max, ' ');
    pglab($info->{'Xlabel'}, $info->{'Ylabel'}, "");
    pgmtxt("T", 2.5, 0.5, 0.5, "$info->{'Title'}     $info->{'Component'}");
    pgmtxt("T", 1.0, 0.5, 0.5, "[$info->{'Filename'}]");
    pgebuf;
    release;
}

sub plot_array_1d {
    my ($info,$npt) = @_;
    my ($x,$N,$p1,$p2) = rcols $info->{'Filename'};
    my ($x0,$x1) = @{$info->{'Limits'}};
    my $N1 = $N + ($N == 0);	# N with 0 entries set to 1
    my $N2 = $N1 + ($N1 == 1);	# N with 0/1 entries set to 2
    my $pmean = $p1/$N1;
    my $err = sqrt(abs($N/($N2 - 1)*($p2 - $pmean*$pmean)));
    my ($min, $max) = (min($p1 - 2*$err), max($p1 + 2*$err));
    if($min == $max) {
	if($min == 0) {
	    ($min, $max) = (0, 1);
	} else {
	    ($min, $max) = (0, $max);
	}
    }
    pgpage;
    pgbbuf;
    hold;
    pgvstd;
    pgswin($x0,$x1,$min,$max);
    line($x, $p1);
    errb($x, $p1, $err);
    pgbox("BCNST", 0.0, 0.0, "BCNST", 0.0, 0.0);
    pglab($info->{'Xlabel'}, $info->{'Ylabel'}, "");
    pgmtxt("T", 2.5, 0.5, 0.5, "$info->{'Title'}     $info->{'Component'}");
    pgmtxt("T", 1.0, 0.5, 0.5, "[$info->{'Filename'}]");
    pgebuf;
    release;
}

# This function computes a 'good' panel size (X x Y) to fit a given
# number of plots.
sub calc_panel_size {
    my ($num) = @_;
    my @panels = ( [1,1], [2,1], [2,2], [3,2], [3,3], [4,3], [5,3], [4,4],
		   [5,4], [6,4], [5,5], [6,5], [7,5], [6,6], [8,5], [7,6],
		   [9,5], [8,6], [7,7], [9,6], [8,7], [9,7], [8,8], [10,7],
		   [9,8], [11,7], [9,9], [11,8], [10,9], [12,8], [11,9],
		   [10,10] );
    my ($nx,$ny, $fit);
    # Default size about sqrt($num) x sqrt($num).
    $ny = int(sqrt($num));
    $nx = int($num/$ny);
    $nx++ if $nx*$ny < $num;
    $fit = $nx*$ny - $num;
    for $panel (@panels) {
	my $d = $panel->[0]*$panel->[1] - $num;
	($fit,$nx,$ny) = ($d, $panel->[0], $panel->[1])
	    if($d >=0 && $d <= $fit);
    }
    return ($nx,$ny);
}

sub plot_dat_info {
    my ($info) = @_;
    my $type = $info->{'Type'};
    if($type =~ /^\s*array_2d\s*\(\s*([0-9]+)\s*,\s*([0-9]+)\s*\)\s*$/i) {
	plot_array_2d($info, $1, $2);
    }elsif($type =~ /^\s*array_1d\s*\(\s*([0-9]+)\s*\)\s*$/i) {
	plot_array_1d($info, $1);
    } else {
	die "Unimplemented plot type '$type'";
    }
}

my ($file) = @ARGV;
$file = "mcstas.sim" unless $file;
my $datalist = read_sim_file($file);

for(;;) {
    my $dev = pgopen("/xserv");
    die "PGOPEN failed!" unless $dev > 0;
    my ($nx, $ny) = calc_panel_size(int(@$datalist));
    pgsubp ($nx,$ny);
    my $info;
    for $info (@$datalist) {
	plot_dat_info($info);
    }
    # Now wait for mouse click on a plot and plot it full-screen.
    pgpanl(1,1);
    pgsvp(0,1,0,1);
    pgswin(0,1,1,0);
    my ($ax,$ay,$cx,$cy,$cc) = (0,0,0,0,"");
    pgband(0, 0, $ax, $ay, $cx, $cy, $cc);
    pgclos;
    last if $cc =~ /[xq]/i;
    if($cc =~ /p/i) {
	$dev = pgopen("mcstas.ps/cps");
	die "PGOPEN failed!" unless $dev > 0;
	my ($nx, $ny) = calc_panel_size(int(@$datalist));
	pgsubp ($nx,$ny);
	my $info;
	for $info (@$datalist) {
	    plot_dat_info($info);
	}
	pgclos;
    }	
    my ($i, $j) = (int($cx), int($cy));
    $i = 0 if $i < 0;
    $j = 0 if $j < 0;
    my $idx = $i + $nx*$j;
    $dev = pgopen("/xserv");
    die "PGOPEN failed!" unless $dev > 0;
    plot_dat_info($datalist->[$idx]);
    pgband(0, 0, $ax, $ay, $cx, $cy, $cc);
    pgclos;
    if($cc =~ /p/i) {
	$dev = pgopen("$datalist->[$idx]{'Component'}.ps/cps");
	die "PGOPEN failed!" unless $dev > 0;
	plot_dat_info($datalist->[$idx]);
	pgclos;
    }	
}
