# Library of McStas plotting functions (PGPLOT related)
#
#   This file is part of the McStas neutron ray-trace simulation package
#   Copyright (C) 1997-2006, All rights reserved
#   Risoe National Laborartory, Roskilde, Denmark
#   Institut Laue Langevin, Grenoble, France
#
#   This program is free software; you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation; version 3 of the License.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
#
#   You should have received a copy of the GNU General Public License
#   along with this program; if not, write to the Free Software
#   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
#

use PDL;
use PDL::Graphics::PGPLOT;
use PGPLOT;

require "mcfrontlib2D.pl";

sub plot_array_2d {
    my ($info,$m,$n) = @_;
    my $data_orig;
    $data_orig= get_detector_data_2D($info);
    my $data = $data_orig->copy;
    my ($x0,$x1,$y0,$y1) = @{$info->{'Limits'}};
    my ($dx,$dy) = (($x1 - $x0)/$m, ($y1 - $y0)/$n);
    my $tr = pdl [ $x0 + $dx/2, $dx, 0, $y0 + $dy/2, 0, $dy ];
    if ($info->{'Logmode'} == 1 && min($data) <= 0 && max($data) > 0) {
      my $i=which($data <= 0);
      my $j=which($data >  0);
      my $low_data = $data->flat->index($i);
      $low_data .= min($data->flat->index($j))/10;
    }
    if ($info->{'Logmode'} == 1 && max($data) > 0) { $data = log10(abs($data)); }
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
    if ($info->{'Contour'} == 1) {
      cont $data, {TRANSFORM => $tr};
    } else {
      imag $data, $min, $max, $tr;
    }
    pgwedg("RI", 0.5, 3.0, $min, $max, ' ');
    if($buf =~ /^V?PS$/i) {
      pgscr(0, $r0, $g0, $b0);
      pgscr(1, $r1, $g1, $b1);
    }
    pglab($info->{'Xlabel'}, $info->{'Ylabel'}, "");
    my $title = "$info->{'Component'}"; # removed {'Title'} which is often too long
    if ($info->{'Logmode'} == 1) { $title = "[LOG] $title"; }
    my $fileshort = basename($info->{'Filename'});
    if ($info->{'ShowI'}) {
      my $vars=$info->{'Values'};
      if ($vars->[0]) { pgmtxt("T", 0.25, 0.5, 0.5, "I=$vars->[0] Err=$vars->[1] N=$vars->[2]"); }
      pgmtxt("T", 1.50, 0.5, 0.5, "$info->{'Stats'}");
      pgmtxt("T", 2.75, 0.5, 0.5, "$title [$fileshort]");
    } else {
      pgmtxt("T", 1.75, 0.5, 0.5, $info->{'Title'});
      pgmtxt("T", 0.25, 0.5, 0.5, "$title [$fileshort]");
    }
    pgiden();
    pgebuf;
    release;
}

sub plot_array_1d {
    my ($info,$npt) = @_;
    my $r;
    $r= get_detector_data_1D($info);
    my $nx = $info->{'Xvar'}[0];
    my $ny = $info->{'Yvar'}[0];
    my $x = $r->{$nx};
    my $I_orig = $r->{$ny};
    my $I = $I_orig->copy;
    my ($x0,$x1) = @{$info->{'Limits'}};
    my ($min, $max, $err);
    $min = min($I);
    if ($info->{'Logmode'} == 1 && $min <= 0 && max($I) > 0) {
      my $i=which($I <= 0);
      my $j=which($I >  0);
      my $low_data = $I->flat->index($i);
      $low_data .= min($I->flat->index($j))/10;
    }
    if($info->{'Yerr'} && $info->{'Yerr'}[0]) {
      $err = $r->{$info->{'Yerr'}[0]};
      if ($info->{'Logmode'} == 1 && max($I) > 0) { $err = $err/$I; $I = log10(abs($I)); }
      ($min, $max) = (min($I - 2*$err), max($I + 2*$err));
    } else {
      if ($info->{'Logmode'} == 1 && max($I) > 0) { $I = log($I); }
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
    pgsci(1);
    hold;
    pgvstd;
    pgswin($x0,$x1,$min,$max);
    line($x, $I);
    errb($x, $I, $err) if defined($err);
    pgbox("BCNST", 0.0, 0.0, "BCNST", 0.0, 0.0);
    pglab($info->{'Xlabel'}, $info->{'Ylabel'}, "");
    my $title = "$info->{'Component'}"; # removed {'Title'} which is often too long
    if ($info->{'Logmode'} == 1) { $title = "[LOG] $title"; }
    my $fileshort = basename($info->{'Filename'});
    if ($info->{'ShowI'}) {
      my $vars=$info->{'Values'};
      if ($vars->[0]) { pgmtxt("T", 0.25, 0.5, 0.5, "I=$vars->[0] Err=$vars->[1] N=$vars->[2] ; $info->{'Stats'}"); }
      else { pgmtxt("T", 0.25, 0.5, 0.5, "$info->{'Stats'}"); }
      pgmtxt("T", 1.50, 0.5, 0.5, $info->{'Title'});
      pgmtxt("T", 2.75, 0.5, 0.5, "$title [$fileshort]");
    } else {
      pgmtxt("T", 1.75, 0.5, 0.5, $info->{'Title'});
      pgmtxt("T", 0.25, 0.5, 0.5, "$title [$fileshort]");
    }
    pgiden();
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
      print "Warning: Unimplemented plot type '$type' in file '$info->{Filename}' (plot_dat_info)";
    }
}

sub overview_plot {
    my ($datalist, $interactive) = @_;
    return unless @$datalist;
    my ($nx, $ny) = calc_panel_size(int(@$datalist));
    pgsubp ($nx,$ny);

    my $info;
    for $info (@$datalist) {
      if ($interactive =~ /-log/i) { $info->{'Logmode'} = 1; }
      else { $info->{'Logmode'} = 0; }
      if ($interactive =~ /-contour/i) { $info->{'Contour'} = 1; }
      else { $info->{'Contour'} = 0; }
      if ($nx*$ny > 4) { $info->{'ShowI'} = 0; }
      else { $info->{'ShowI'} = 1; }
      plot_dat_info($info);
    }
    if($interactive =~ /interactive/i) {
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
      return ($cc,$idx);
    } else {
      return ();
    }
}

sub single_plot {
    my ($info, $interactive) = @_;
    calc_panel_size(1);
    pgsubp (0,0);
    if ($interactive =~ /-log/i) { $info->{'Logmode'} = 1; }
    else { $info->{'Logmode'} = 0; }
    if ($interactive =~ /-contour/i) { $info->{'Contour'} = 1; }
    else { $info->{'Contour'} = 0; }
    $info->{'ShowI'} = 1;
    plot_dat_info($info);
    if($interactive =~ /interactive/i) {
      # Wait for user to press a key.
      my ($ax,$ay,$cx,$cy,$cc) = (0,0,0,0,"");
      pgband(0, 0, $ax, $ay, $cx, $cy, $cc);
      return ($cc, $cx, $cy);
    } else {
      return ();
    }
}

# Make sure that the PGPLOT X11 window server is started, by opening
# and immediately closing a window.
sub ensure_pgplot_xserv_started {
    my $olddev;
    pgqid($olddev);
    my $newdev;
    if (defined(&dev)) { $newdev = dev($MCSTAS::mcstas_config{'PGDEV'}); }
    else { $newdev = pgopen($MCSTAS::mcstas_config{'PGDEV'}); }
    die "DEV/PGOPEN $MCSTAS::mcstas_config{'PGDEV'} failed!" unless $newdev > 0;
    if (defined(&close_window)) { close_window(); }
    else { pgclos(); }
    if ($olddev) {
	pgslct($olddev);
    } 
    return 1;
}

sub get_device { # mcdisplay style deviceselection
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
