use PDL;
use PDL::Graphics::PGPLOT;
use PGPLOT;

require "mcfrontlib.pl";

sub plot_array_2d {
    my ($info,$m,$n) = @_;
    my $data = get_detector_data_2D($info);
    my ($x0,$x1,$y0,$y1) = @{$info->{'Limits'}};
    my ($dx,$dy) = (($x1 - $x0)/$m, ($y1 - $y0)/$n);
    my $tr = pdl [ $x0 + $dx/2, $dx, 0, $y0 + $dy/2, 0, $dy ];
    my ($min, $max) = (min($data), max($data));
    if ($min == $max) {
      if($min == 0) {
          $max = 1;
      } else {
          $min = 0.9*$min;
          $max = 0.9*$max;
      }
    }
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
    # If using the black&white postscript driver, swap foreground and
    # background when doing the image to get more printer-friendly
    # output.
    my ($buf, $len);
    my ($r0, $g0, $b0, $r1, $g1, $b1);
    pgqinf("TYPE", $buf, $len);
    if($buf =~ /^V?PS$/i) {
      pgqcr(0, $r0, $g0, $b0);
      pgqcr(1, $r1, $g1, $b1);
      pgscr(0, $r1, $g1, $b1);
      pgscr(1, $r0, $g0, $b0);
    }
    imag $data, $min, $max, $tr;
    pgwedg("RI", 0.5, 3.0, $min, $max, ' ');
    if($buf =~ /^V?PS$/i) {
      pgscr(0, $r0, $g0, $b0);
      pgscr(1, $r1, $g1, $b1);
    }
    pglab("$info->{'Xlabel'} $info->{'Stats'}", $info->{'Ylabel'}, "");
    pgmtxt("T", 2.5, 0.5, 0.5, "$info->{'Title'}     $info->{'Component'}");
    pgmtxt("T", 1.0, 0.5, 0.5, "[$info->{'Filename'}] ");
    pgebuf;
    release;
}

sub plot_array_1d {
    my ($info,$npt) = @_;
    my $r = get_detector_data_1D($info);
    my $x = $r->{$info->{'Xvar'}[0]};
    my $I = $r->{$info->{'Yvar'}[0]};
    my ($x0,$x1) = @{$info->{'Limits'}};
    my ($min, $max, $err);
    if($info->{'Yerr'} && $info->{'Yerr'}[0]) {
      $err = $r->{$info->{'Yerr'}[0]};
      ($min, $max) = (min($I - 2*$err), max($I + 2*$err));
    } else {
      ($min, $max) = (min($I), max($I));
    }
    if($min == $max) {
      if($min == 0) {
          ($min, $max) = (0, 1);
      } else {
          ($min, $max) = (0, $max);
      }
    }
    # Include zero point of Y axis if minimum is close to zero.
    $min = 0 if($min > 0 && $min/$max < 0.2);
    pgpage;
    pgbbuf;
    hold;
    pgvstd;
    pgswin($x0,$x1,$min,$max);
    line($x, $I);
    errb($x, $I, $err) if defined($err);
    pgbox("BCNST", 0.0, 0.0, "BCNST", 0.0, 0.0);
    pglab($info->{'Xlabel'}, $info->{'Ylabel'}, "");
    pgmtxt("T", 2.5, 0.5, 0.5, "$info->{'Title'}     $info->{'Component'}");
    pgmtxt("T", 1, 0.5, 0.5, "[$info->{'Filename'}] $info->{'Stats'}");
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

sub overview_plot {
    my ($devspec, $datalist, $interactive) = @_;
    return unless @$datalist;
    my ($nx, $ny) = calc_panel_size(int(@$datalist));
    my $dev = pgopen("$devspec");
    die "PGOPEN failed!" unless $dev > 0;
    pgsubp ($nx,$ny);
    my $info;
    for $info (@$datalist) {
      plot_dat_info($info);
    }
    if($interactive) {
      # Wait for user to select a plot.
      pgpanl(1,1);
      pgsvp(0,1,0,1);
      pgswin(0,1,1,0);
      my ($ax,$ay,$cx,$cy,$cc) = (0,0,0,0,"");
      pgband(0, 0, $ax, $ay, $cx, $cy, $cc);
      my ($i, $j) = (int($cx), int($cy));
      $i = 0 if $i < 0;
      $j = 0 if $j < 0;
      $i = $nx - 1 if $i >= $nx;
      $j = $ny - 1 if $j >= $ny;
      my $idx = $i + $nx*$j;
      $idx = int(@$datalist) - 1 if $idx >= int(@$datalist);
      pgclos;
      return ($cc,$idx);
    } else {
      pgclos;
      return ();
    }
}

sub single_plot {
    my ($devspec, $info, $interactive) = @_;
    my $dev = pgopen("$devspec");
    die "PGOPEN failed!" unless $dev > 0;
    plot_dat_info($info);
    if($interactive) {
      # Wait for user to press a key.
      my ($ax,$ay,$cx,$cy,$cc) = (0,0,0,0,"");
      pgband(0, 0, $ax, $ay, $cx, $cy, $cc);
      pgclos;
      return ($cc, $cx, $cy);
    } else {
      pgclos;
      return ();
    }
}

# Make sure that the PGPLOT X11 window server is started, by opening
# and immediately closing a window.
sub ensure_pgplot_xserv_started {
    my $olddev;
    pgqid($olddev);
    my $newdev = pgopen("/xserv");
    pgclos();
    pgslct($olddev);
}
1;
