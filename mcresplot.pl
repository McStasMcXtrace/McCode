#! /usr/bin/perl

use PDL;
use PDL::Math;
use PDL::Slatec;
use PDL::IO::FastRaw;
use PGPLOT;
use PDL::Graphics::TriD;
use PDL::Graphics::PGPLOT;

$PI = 3.14159265358979323846;

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
    $unit_q->set(1,0);		# Force into scattering plane.
    $unit_q /= sqrt(inner($unit_q,$unit_q));
    $unit_n = pdl($unit_q->at(2), 0, -$unit_q->at(0));
    $unit_z = pdl(0,1,0);
    # Build ortogonal transformation matrix, and change coordinates of Q.
    $tmat = cat ($unit_q, $unit_n, $unit_z);
    $q_t = clump(xchg(PDL::Primitive::matmult($tmat,$q->dummy(2)),1,2),2);
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

    # Plot histograms for the four 1-d projections.
    dev "/xwin", 4, 3;
    pgsch(2.1);
    pgsci(1);
    q_hist($qx, $p, "Q\\dx\\u [\\A\\u-1\\d]");
    q_hist($qy, $p, "Q\\dy\\u [\\A\\u-1\\d]");
    pgpage;
    pgmtxt("t",-1,0.0,0.0,"Resolution matrix:");
    $pos = matout(-3.0, $res_mat);
    pgmtxt("t",$pos-1,0.0,0.0,"Bragg Widths");
    pgmtxt("t",$pos-3-0*1.2,0.2,0.0,"Q\\dx\\u = " .
	   2.3548/sqrt($res_mat->at(0,0)));
    pgmtxt("t",$pos-3-1*1.2,0.2,0.0,"Q\\dy\\u = " .
	   2.3548/sqrt($res_mat->at(1,1)));
    pgmtxt("t",$pos-3-2*1.2,0.2,0.0,"Q\\dz\\u = " .
	   2.3548/sqrt($res_mat->at(2,2)));
    pgmtxt("t",$pos-3-3*1.2,0.2,0.0,"\\gw = " .
	   2.3548/sqrt($res_mat->at(3,3)));
    pgmtxt("t",$pos-9,0.0,0.0,"Covariance matrix:");
    $pos = matout($pos-11.0, $C_t);
    pgpage;
    q_hist($qz, $p, "Q\\dz\\u [\\A\\u-1\\d]");
    q_hist($w,  $p, "\\gw [meV]");
    pgpage;pgpage;
    mcs_projs(1,$res_mat);

    # Make a 3d visualization of the resolution elipsoid. Use MC
    # choice to eliminate the weights.
    $r = random $size;
    $qx_mc = $qx->where($p > $r*max($p));
    $qy_mc = $qy->where($p > $r*max($p));
    $qz_mc = $qz->where($p > $r*max($p));
    $w_mc = $w->where($p > $r*max($p));
    $npts = $w_mc->nelem;
    print "\nPress 'Q' in 3D window to continue ...\n";
    points3d [$qx_mc,$qy_mc,$w_mc],
	     [0.8*ones($npts),0.8*ones($npts),zeroes($npts)];

    # Now overlay with points generated from the corresponding gaussian.
    $cntr = PDL::Primitive::matmult($umat,$ave_A->dummy(0));
    $gaus = PDL::Primitive::matmult(chol($C_t),grandom($npts,4)) + $cntr;
    print "Adding gausian distribution\nPress 'Q' in 3D window to end ...\n";
    hold3d();
    points3d [$gaus->slice(",(0)"),$gaus->slice(",(1)"),$gaus->slice(",(3)")],
	     [0.9*ones($npts),zeroes($npts),zeroes($npts)];
    release3d();
}

sub chol {
    my ($A) = @_;
    my ($i,$j,$k,$L,$n,$n2,$li,$lj,$v);

    ($n,$n2) = $A->dims;
    die "Must be square matrix" unless $n==$n2;
    $L = zeroes $n,$n;
    $li = $lj = pdl [];		# Handle special case for i=0
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

sub q_hist {
    my ($v,$p,$t) = @_;
    my ($m,$h) = weighted_hist($v,$p,50);
    bin(($m,$h/max($h)),{COLOUR => YELLOW});
    pglab($t,"R(Q,\\gw) [A.U]", "");
}

sub weighted_hist {
    my ($x,$p,$n) = @_;
    my ($xmin,$xmax,$h,$bin,$xmid);

    ($xmin,$xmax) = minmax($x);
    $h = zeroes $n;
    $bin = long(($x - $xmin)/($xmax - $xmin)*$n);
    $xmid = sequence($n)/$n*($xmax - $xmin) + $xmin + ($xmax - $xmin)/(2*$n);
    for(0..($n-1)) {
	$h->set($_, sum($p->where($bin==$_)));
    }
    $h->set($n-1, $h->at($n-1) + sum($p->where($bin==$n)));
    return ($xmid, $h);
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

sub bad_rc_int {
    my ($i,$r0,$m) = @_;
    my ($n1,$n2,$r,$sel,$b,$mp,$new);

    ($n1,$n2) = $m->dims;
    die "Must have square input matrix" unless $n1==$n2;
    $r = sqrt(2*$PI/$m->at($i,$i))*$r0;
    $sel = pdl [0..$i-1,$i+1..$n1-1];
    $b = $m->slice(",($i)") + $m->slice("($i),");
    $b = $b->dice($sel);
    $mp = $m->dice($sel,$sel);
    $new = $mp - 1/(4*$m->at($i,$i))*
      PDL::Primitive::matmult($b->dummy(0),$b->dummy(1));
    return ($r, $new);
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

sub mcs_projs {
    my ($R0,$NP) = @_;

    mcs_proj($R0,$NP,2,"Q\\dx\\u [\\A\\u-1\\d]",
	     "Q\\dy\\u [\\A\\u-1\\d]",pdl([0,1,3]),pdl([0,1]));
    mcs_proj($R0,$NP,1,"Q\\dx\\u [\\A\\u-1\\d]",
	     "\\gw [meV]",pdl([0,1,3]),pdl([0,3]));
    mcs_proj($R0,$NP,0,"Q\\dy\\u [\\A\\u-1\\d]",
	     "\\gw [meV]",pdl([0,1,3]),pdl([1,3]));

    mcs_proj($R0,$NP,0,"Q\\dz\\u [\\A\\u-1\\d]",
	     "\\gw [meV]",pdl([0,2,3]),pdl([2,3]));
}

sub mcs_proj {
    my ($R0,$A,$index,$xlabel,$ylabel,$sel1,$sel2) = @_;
    my($B,$R0P,$MP,$x,$y);

    $B = $A->dice($sel1,$sel1);
    ($R0P,$MP) = rc_int($index,$R0,$B);
    ($x,$y) = proj_elip($MP);
    poly($x,$y, {COLOUR => RED});
    hold;
    line($x,$y,{COLOUR => BLACK});
    ($x,$y) = proj_elip($A->dice($sel2,$sel2));
    poly($x,$y, {COLOUR => GREEN});
    line($x,$y,{COLOUR => BLACK});
    pglab($xlabel,$ylabel,"");
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

read_mcstas_res($ARGV[0]);

1;
