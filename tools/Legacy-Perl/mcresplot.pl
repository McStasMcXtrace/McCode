#! /usr/bin/perl
#
# Implements plotting of McStas resolution function data using PGPLOT
#
#
#   This file is part of the McStas neutron ray-trace simulation package
#   Copyright (C) 1997-2004, All rights reserved
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

use PDL;
use PDL::Core;
use PDL::Math;
use PDL::Slatec;
use PDL::IO::FastRaw;
use PGPLOT;
use PDL::Graphics::PGPLOT;

# Determine the path to the McStas system directory. This must be done
# in the BEGIN block so that it can be used in a "use lib" statement
# afterwards.
BEGIN {
    ENV_HEADER
}
use lib $MCSTAS::perl_dir;

require "mcfrontlib2D.pl";

$PI = 3.14159265358979323846;

sub read_mcstas_info {
  my ($file) = @_;
  my $basedir;
  $basedir = $1 if $file && $file =~ m|^(.*)/[^/]*$|;
  my $handle = new FileHandle;
  open $handle, $file or die "Could not open file '$file'";
  $info = read_simulation_info($handle);
  close($handle);
  return ($info);
}

sub read_mcstas_res {
    my ($filename) = @_;
    my ($data,$kix,$kiy,$kiz,$kfx,$kfy,$kfz,$x,$y,$z,$pi,$pf);
    my ($size,$ki,$kf,$q,$qx,$qy,$qz,$p,$Ei,$Ef,$w);
    my ($r,$qx_mc,$qy_mc,$qz_mc,$w_mc, $npts,$cntr,$gaus);
    my ($ave_q,$unit_q,$unit_n,$unit_z,$tmat,$q_t);
    my ($A,$ave_A,$mid_A,$C,$umat,$C_t,$res_mat);
    my ($pos);

    # Read data from file (either raw or ascii).
    if($filename =~ /\.raw$/) {
        $data = readfraw($filename);
        ($kix,$kiy,$kiz,$kfx,$kfy,$kfz,$x,$y,$z,$pi,$pf) = dog $data;
    } else {
        ($kix,$kiy,$kiz,$kfx,$kfy,$kfz,$x,$y,$z,$pi,$pf) = rcols($filename);
        $data = cat ($kix,$kiy,$kiz,$kfx,$kfy,$kfz,$x,$y,$z,$pi,$pf);
    }
    # Compute some basic entities
    ($size) = $kix->dims;
    $ki = cat($kix, $kiy, $kiz);
    $kf = cat($kfx, $kfy, $kfz);
    $q = $ki - $kf;
    $Ei = 2.072*($kix*$kix+$kiy*$kiy+$kiz*$kiz);
    $Ef = 2.072*($kfx*$kfx+$kfy*$kfy+$kfz*$kfz);
    $w = $Ei-$Ef;
    $p = $pi*$pf;
    # Compute coordinate change: X along average Q vector projected
    # into plane, Y perpendicular to X in plane, Z upwards.
    $ave_q = sumover($q*$p->dummy(1,3)) / (sum($p));
    $unit_q = $ave_q->copy;
    $unit_q->set(1,0);                # Force into scattering plane.
    $unit_q /= sqrt(inner($unit_q,$unit_q));
    $unit_n = pdl($unit_q->at(2), 0, -$unit_q->at(0));
    $unit_z = pdl(0,1,0);
    # Build orthogonal transformation matrix, and change coordinates of Q.
    $tmat = cat ($unit_q, $unit_n, $unit_z);
    $q_t = xchg(PDL::Primitive::matmult($tmat,$q->dummy(2)),1,2);
    $q_t = $q_t->clump(2);
    ($qx,$qy,$qz) = dog $q_t;

    # Now compute resolution matrix.
    $A = append($q->transpose, $w->dummy(0));
    $ave_A = sumover($A->transpose*$p->dummy(1,4)) / sum($p);
    $mid_A = $A - $ave_A->dummy(1);
    # Get the covariance matrix in original coordinates.
    $C = PDL::Primitive::matmult
        ($mid_A->transpose, $mid_A*$p->dummy(0,4)) / sum($p);
    # Change coordinates, and compute the resolution matrix.
    $umat = transpose(append(transpose(append($tmat,pdl [0])),
                             pdl [[0],[0],[0],[1]]));
    $C_t = inner2t($umat->transpose,$C,$umat);
    $res_mat = $C_t->matinv;
    print "The covariance matrix is\n";
    print $C_t;
    print "and the resolution matrix is\n";
    print $res_mat;
    print "Gaussian half width [Qx Qy Qz En] in Angs-1 and meV are\n";
    $gqx = int(2.3548/sqrt($res_mat->at(0,0))*1e4)/1e4;
    $gqy = int(2.3548/sqrt($res_mat->at(1,1))*1e4)/1e4;
    $gqz = int(2.3548/sqrt($res_mat->at(2,2))*1e4)/1e4;
    $gen = int(2.3548/sqrt($res_mat->at(3,3))*1e4)/1e4;
    print "[$gqx $gqy $gqz $gen]\n";

    return($qx,$qy,$qz,$w,$p,$C_t,$res_mat,$size);
}

sub plot_mcstas_res {
  my ($filename,$device,$qx,$qy,$qz,$w,$p,$C_t,$res_mat,$size,$interactive,$si) = @_;
  # Plot histograms for the four 1-d projections.
    if (defined(&dev)) { $dev = dev "$device",4,2; }
    else { $dev = pgopen("$device"); pgsubp(4,2); }
    die "DEV/PGOPEN $device failed!" unless $dev > 0;

    pgsch(2.1);
    pgsci(1);

    # Make a 3d visualization of the resolution elipsoid. Use MC
    # choice to eliminate the weights.
    $r = random $size;
    $qx_mc = $qx->where($p > $r*max($p));
    $qy_mc = $qy->where($p > $r*max($p));
    $qz_mc = $qz->where($p > $r*max($p));
    $w_mc = $w->where($p > $r*max($p));
    $npts = $w_mc->nelem;
    $R0 = 1;
    $NP = $res_mat;

    # plot 2D histograms, and add the gaussian ellipsoid on top of each
    pgpanl(1,1);
    q_hist2($qx_mc, $w_mc, "Q\\dx\\u [\\A\\u-1\\d]","\\gw [meV]",50,0);
    mcs_proj($R0,$NP,1, $qx_mc->sum/$npts, $w_mc->sum/$npts, pdl([0,1,3]),pdl([0,3]));

    pgpanl(2,1);
    q_hist2($qy_mc, $w_mc, "Q\\dy\\u [\\A\\u-1\\d]","\\gw [meV]",50,0);
    mcs_proj($R0,$NP,0, $qy_mc->sum/$npts, $w_mc->sum/$npts, pdl([0,1,3]),pdl([1,3]));

    pgpanl(3,1);
    q_hist2($qz_mc, $w_mc, "Q\\dz\\u [\\A\\u-1\\d]","\\gw [meV]",50,0);
    mcs_proj($R0,$NP,0, $qz_mc->sum/$npts, $w_mc->sum/$npts, pdl([0,2,3]),pdl([2,3]));

    pgpanl(4,1);
    q_hist2($qx_mc, $qy_mc, "Q\\dx\\u [\\A\\u-1\\d]","Q\\dy\\u [\\A\\u-1\\d]",50,1);
    mcs_proj($R0,$NP,2, $qx_mc->sum/$npts, $qy_mc->sum/$npts,pdl([0,1,3]),pdl([0,1]));


    pgpanl(1,2);
    my $offset=-1;
    pgmtxt("t",$offset-0*1.2,.0,0.0,"Bragg (Gaussian) Half Widths");
    pgmtxt("t",$offset-1*1.2,0.2,0.0,"\\gDQ\\dx\\u = " .
           int(2.3548/sqrt($res_mat->at(0,0))*1e4)/1e4 . " \\A\\u-1\\d");
    pgmtxt("t",$offset-2*1.2,0.2,0.0,"\\gDQ\\dy\\u = " .
           int(2.3548/sqrt($res_mat->at(1,1))*1e4)/1e4 . " \\A\\u-1\\d");
    pgmtxt("t",$offset-3*1.2,0.2,0.0,"\\gDQ\\dz\\u = " .
           int(2.3548/sqrt($res_mat->at(2,2))*1e4)/1e4 . " \\A\\u-1\\d");
    pgmtxt("t",$offset-4*1.2,0.2,0.0,"\\gD\\gw = " .
           int(2.3548/sqrt($res_mat->at(3,3))*1e4)/1e4 . " meV");

    pgmtxt("t",$offset-6*1.2,0.0,0.0,"Resolution matrix [Q\\dx\\u Q\\dy\\u Q\\dz\\u \\gw]:");
    $pos = matout($offset-7*1.2, $res_mat);
    pgmtxt("t",$pos+$offset-0*1.2,.0,0.0,"Covariance matrix [Q\\dx\\u Q\\dy\\u Q\\dz\\u \\gw]:");
    $pos = matout($pos+$offset-1*1.2, $C_t);

    pgpanl(2,2);
    pgmtxt("t",$offset-0*1.2,0.0,0.0,"File: $filename");
    my $time=gmtime;
    pgmtxt("t",$offset-1*1.2,0.0,0.0,"Date: $time");
    pgmtxt("t",$offset-2*1.2,0.0,0.0,"X along <Q> in plane");
    pgmtxt("t",$offset-3*1.2,0.0,0.0,"Y perp. to X in plane, Z upwards");

    pgpanl(3,2);

    my $i;
    my $j=0;
    my $shift=0.0;
    pgmtxt("t",$offset,0.0,0.0,"Instrument simulation parameters:");
    foreach $i (keys %{$si->{'Params'}}) {
      $j = $j+1;
      pgmtxt("t",$offset-$j*1.2,$shift,0.0,$i . " = " . $si->{'Params'}{$i});
      if ($j > 20) { $shift = $shift+0.5; $j=0; }
    }
    if ($j == 0 && $shift == 0) { pgmtxt("t",$offset-2*1.2,0.0,0.0,"None"); }
}

sub chol {
    my ($A) = @_;
    my ($i,$j,$k,$L,$n,$n2,$li,$lj,$v);

    ($n,$n2) = $A->dims;
    die "Must be square matrix" unless $n==$n2;
    $L = zeroes $n,$n;
    $li = $lj = pdl [];                # Handle special case for i=0
    for($i=0; $i<$n; $i++) {
        $li = $L->mslice([0,$i-1],[$i]) if $i;
        $v = $A->at($i,$i) - sum($li*$li);
        die "Not positive definite" unless $v >= 0;
        $L->set($i,$i, sqrt($v));
        for($j=$i+1; $j<$n; $j++) {
            $lj = $L->mslice([0,$i-1],[$j]) if $i;
            $L->set($i,$j, ($A->at($i,$j) - sum($li*$lj))/$L->at($i,$i));
        }
    }
    return $L;
}

sub q_hist2 {
    my ($x,$y,$xl,$yl,$npts,$plot_wedge) = @_;
    ($xmin,$xmax) = minmax($x);
    ($ymin,$ymax) = minmax($y);
    $dx=($xmax-$xmin)/$npts;
    $dy=($ymax-$ymin)/$npts;
    my $tr;
    if (defined(&label_axes))
    { $tr = pdl [$xmin + $dx/2, $dx, 0, $ymin + $dy/2, 0, $dy]; }
    else
    { $tr = cat $xmin + $dx/2, $dx, pdl(0), $ymin + $dy/2, pdl(0), $dy; }
    $hxy = histogram2d($x, $y, $dx, $xmin, $npts, $dy, $ymin, $npts);
    my ($min, $max) = (min($hxy), max($hxy));
    if ($min == $max) {
      if($min == 0) {
          $max = 1;
      } else {
          $min = 0.9*$min;
          $max = 0.9*$max;
      }
    }
    my $ramp = pdl [[ 0,  1/8,  3/8,  5/8,  7/8,  8/8],
                [ 0,    0,    0,    1,    1,   .5],
                [ 0,    0,    1,    1,    0,    0],
                [.5,    1,    1,    0,    0,    0]];
    my $numcol = 64;
    # now do the plottings
    pgswin($xmin,$xmax,$ymin,$ymax);
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
    imag $hxy, $min, $max, $tr;
    if ($plot_wedge) { pgwedg("RI", 0.5, 3.0, $min, $max, ' '); }
    if($buf =~ /^V?PS$/i) {
      pgscr(0, $r0, $g0, $b0);
      pgscr(1, $r1, $g1, $b1);
    }
    pglab($xl, $yl,"");
}

sub matout {
    my ($pos,$x) = @_;
    my @lines = split("\n","$x");
#    shift(@lines);shift(@lines);pop(@lines);
    for(@lines) {
        if(m'\[([^]]*)\]') {
            pgmtxt("t",$pos,0.0,0.0,$1);
            $pos-= 1.2;
        }
    }
    return $pos;
}

# The rest of this file is converted from rescal5 matlab code.
sub rot_elip {
    my ($a,$b,$phi) = @_;
    my($n,$x,$y,$s,$c,$th);

    $n = 100;
    $th = sequence($n+1)/$n*2*$PI;
    $x = $a*cos($th);
    $y = $b*sin($th);
    $c = cos($phi);
    $s = sin($phi);
    $th = $x*$c - $y*$s;
    $y = $x*$s + $y*$c;
    $x = $th;
    return ($x,$y);
}

sub rc_int {
    my ($i,$r0,$m) = @_;
    my ($n1,$n2,$r,$sel,$b,$mp,$new);

    ($n1,$n2) = $m->dims;
    die "Must have square input matrix" unless $n1==$n2;
    $r = sqrt(2*$PI/$m->at($i,$i))*$r0;
    $sel = pdl [0..$i-1,$i+1..$n1-1];
    $b = $m->slice(",($i)") + $m->slice("($i),");
    $b = $b->dice($sel);

    $mp = zeroes $n1-1,$n2-1;
    if($i > 0) {
        $mp = $mp->ins($m->mslice([0,$i-1],[0,$i-1]),0,0);
    }
    if($i < $n1 - 1) {
        $mp = $mp->ins($m->mslice([$i+1,$n1-1],[$i+1,$n2-1]),$i,$i);
    }
    if($i > 0 && $i < $n1 - 1) {
        $mp = $mp->ins($m->mslice([0,$i-1],[$i+1,$n2-1]),0,$i);
        $mp = $mp->ins($m->mslice([$i+1,$n1-1],[0,$i-1]),$i,0);
    }
    $new = $mp - 1/(4*$m->at($i,$i))*
      PDL::Primitive::matmult($b->dummy(0),$b->dummy(1));
    return ($r, $new);
}



sub mcs_proj {
    my ($R0,$A,$index,$x0,$y0,$sel1,$sel2) = @_;
    my($B,$R0P,$MP,$x,$y);

    $B = $A->dice($sel1,$sel1);
    ($R0P,$MP) = rc_int($index,$R0,$B);
    ($x,$y) = proj_elip($MP);
    #poly($x,$y, {COLOUR => RED});
    hold;
    line($x+$x0,$y+$y0,{COLOUR => BLACK});
    ($x,$y) = proj_elip($A->dice($sel2,$sel2));
    #poly($x,$y, {COLOUR => GREEN});
    line($x+$x0,$y+$y0,{COLOUR => BLACK, LINESTYLE => 'DOT-DASH'});
    rel;
}

sub proj_elip {
    my ($MP) = @_;
    my ($const,$theta,$S,$MP2,$hwhm_xp,$hwhm_yp,$x,$y);

    $const = 1.17741;
    $theta = 0.5*atan(2*$MP->at(0,1)/($MP->at(0,0)-$MP->at(1,1)));
    $S = cat(cat(cos($theta),  sin($theta)),
             cat(-sin($theta), cos($theta)));
    $MP2 = inner2t($S->transpose,$MP,$S);
    $hwhm_xp=$const/sqrt($MP2->at(0,0));
    $hwhm_yp=$const/sqrt($MP2->at(1,1));
    ($x,$y) = rot_elip($hwhm_xp,$hwhm_yp,$theta);
    return ($x,$y);
}

# Start of mcresplot program =====================================================

my $cc;
my $filename;
my $interactive=1;
for($i = 0; $i < @ARGV; $i++) {
        $_ = $ARGV[$i];
  # Options specific to mcplot.
        if(/^-psc$/ || /^-c$/) {
            $cc = "c"; $interactive=0;
        } elsif(/^-ps$/ || /^-p$/) {
            $cc = "p"; $interactive=0;
        } elsif(/^-gif$/ || /^-g$/) {
            $cc = "g"; $interactive=0;
  } elsif(/^--help$/ || /^-h$/ || /^-v$/) {
      print "mcresplot [-ps|-psc|-gif|-v] <FILE from Res_monitor>\n";
      print "  The FILE to be used by mcresplot is generated when using Res_sample\n";
      print "  at the sample position, and Res_monitor afterwards.\n";
      print "  Plots the instrument resolution function (projections).\n";
      print "  When using -ps -psc -gif, the program writes the hardcopy file\n";
      print "  and then exits.\n";
      print "SEE ALSO: mcstas, mcdoc, mcplot, mcrun, mcgui, mcresplot, mcstas2vitess\n";
      print "DOC:      Please visit http://www.mcstas.org/\n";
      exit;
        } else {
      $filename = $ARGV[$i];
  }
}
die "mcresplot <file name from Res_monitor>\n" unless @ARGV;

#read resolution data
my ($qx,$qy,$qz,$w,$p,$C_t,$res_mat,$size) = read_mcstas_res($filename);
# now get parameter list (if any)
my $simulation_info;
$simulation_info =read_mcstas_info($filename);
# now either output direct and exit, or plot xwin and wait for exit
if ($interactive) {
  print "Type 'P' 'C' or 'G' (in graphics window) for hardcopy, 'Q' to quit.\n";
}
for (;;) {
  if($cc =~ /[pcg]/i) {        # Hardcopy?
        my $ext="ps";
        my $dev = ($cc =~ /c/i) ? "cps" : "ps";
        if($cc =~ /g/i) { $dev = "gif"; $ext="gif"; }
        my $fileout = "$filename.$ext";
        plot_mcstas_res($filename, "$fileout/$dev", $qx,$qy,$qz,$w,$p,$C_t,$res_mat,$size,0,$simulation_info);
        print "Wrote file '$fileout' ($dev)\n";
      }
  if ($interactive == 0) { $cc = "q"; }
  else {
    my ($ax,$ay,$cx,$cy) = (0,0,0,0);
    plot_mcstas_res($filename, "/xwin", $qx,$qy,$qz,$w,$p,$C_t,$res_mat,$size,1,$simulation_info);
    pgband(0, 0, $ax, $ay, $cx, $cy, $cc);
  }
  last if $cc =~ /[xq]/i;
}
if (defined(&close_window)) { close_window(); }
      else { pgclos(); }

1;
