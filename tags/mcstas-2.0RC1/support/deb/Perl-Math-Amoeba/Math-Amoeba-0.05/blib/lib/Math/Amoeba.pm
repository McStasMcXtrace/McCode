# This library file contains Amoeba n-D Minimisation routine for Perl
# $Id: Amoeba.pm,v 1.2 1995/12/24 12:37:46 willijar Exp $ 
# $Id: Amoeba.pm,v 1.2 1995/12/24 12:37:46 willijar Exp $

package Math::Amoeba;

use strict;
use warnings;

our $VERSION = 0.05;

use Carp;
use constant TINY => 1e-16;

use Exporter;
our @ISA=qw(Exporter);
our @EXPORT_OK=qw(ConstructVertices EvaluateVertices Amoeba MinimiseND);

=head1 NAME

    Math::Amoeba - Multidimensional Function Minimisation

=head1 SYNOPSIS

    use Math::Amoeba qw(ConstructVertices EvaluateVertices Amoeba MinimiseND);
    my ($vertice,$y)=MinimiseND(\@guess,\@scales,\&func,$tol,$itmax,$verbose);
    my @vertices=ConstructVertices(\@vector,\@offsets);
    my @y=EvaluateVertices(\@vertices,\&func);
    my ($vertice,$y)=Amoeba(\@vertices,\@y,\&func,$tol,$itmax,$verbose);

=head1 DESCRIPTION

This is an implimenation of the Downhill Simpex Method in
Multidimensions (Nelder and Mead) for finding the (local) minimum of a
function. Doing this in Perl makes it easy for that function to
actually be the output of another program such as a simulator.

Arrays and the function are passed by reference to the routines.

=over

=item C<MinimiseND>

The simplest use is the B<MinimiseND> function. This takes a reference
to an array of guess values for the parameters at the function
minimum, a reference to an array of scales for these parameters
(sensible ranges around the guess in which to look), a reference to
the function, a convergence tolerence for the minimum, the maximum
number of iterations to be taken and the verbose flag (default ON). 
It returns an array consisting of a reference to the function parameters 
at the minimum and the value there.

=item C<Amoeba>

The B<Amoeba> function is the actual implimentation of the Downhill
Simpex Method in Multidimensions. It takes a reference to an array of
references to arrays which are the initial n+1 vertices (where n is
the number of function parameters), a reference to the function
valuation at these vertices, a reference to the function, a
convergence tolerence for the minimum, the maximum number of
iterations to be taken and the verbose flag (default ON). 
It returns an array consisting of a reference to the function parameters 
at the minimum and the value there.

=item C<ConstructVertices>

The B<ConstructVertices> is used by B<MinimiseND> to construct the
initial vertices for B<Amoeba> as the initial guess plus the parameter
scale parameters as vectors along the parameter axis.

=item C<EvaluateVertices>

The B<EvaluateVertices> takes these set of vertices, calling the
function for each one and returning the vector of results.

=back

=head1 EXAMPLE

    use Math::Amoeba qw(MinimiseND);
    sub afunc {
      my ($a,$b)=@_;
      print "$a\t$b\n";
      return ($a-7)**2+($b+3)**2;
    }
    my @guess=(1,1);
    my @scale=(1,1);
    ($p,$y)=MinimiseND(\@guess,\@scale,\&afunc,1e-7,100);
    print "(",join(',',@{$p}),")=$y\n";

produces the output

(6.99978191653352,-2.99981241563247)=1.00000008274829

=head1 LICENSE

PERL

=cut

my ($ALPHA,$BETA,$GAMMA)=(1.0,0.5,2.0);

sub MinimiseND {
	my ($guesses,$scales,$func,$tol,$itmax, $verbose)=@_;
	my @p=ConstructVertices($guesses,$scales);
	my @y=EvaluateVertices(\@p,$func);
	return Amoeba(\@p,\@y,$func,$tol,$itmax, $verbose);
}

sub ConstructVertices {
	# given 2 vector references constructs an amoeba
	# returning the vertices
	my ($vector,$ofs)=@_;
	my $n=$#{$vector};
	my @vector=@{$vector};
	my (@p,@y,$i);

	$p[0]=[]; @{$p[0]}=@{$vector};
	for($i=0; $i<=$n; $i++) {
		my $v=[]; @{$v}=@{$vector};
		$v->[$i]+=$ofs->[$i];
		$p[$i+1]=$v;
	}
	return @p;
}

sub EvaluateVertices {
	# evaluates functions for all vertices of the amoeba
	my ($p,$func)=@_;
	my ($i,@y);
	for($i=0; $i<=$#{$p}; $i++) {
		$y[$i]=&$func(@{$p->[$i]});
	}
	return @y;
}

sub Amoeba {

    my ($p,$y,$func,$ftol,$itmax, $verbose)=@_;
	
    my $n=$#{$p}; # no points
    
	# Default parameters
	$verbose = (defined($verbose)) ? $verbose : 1;
	if (!$itmax) { $itmax=200; }
    if (!$ftol) { $ftol=1e-6; }

	# Member variables
    my ($i,$j);
    my $iter=0;
    my ($ilo, $inhi, $ihi);

	my ($pbar, $pr, $pe, $pc, $ypr, $ype, $ypc);

	# To control the recalculation of centroid
	my $recalc = 1;
	my $ihi_o;

	# Loop until any of stopping conditions hit 
	while (1)
	{
    	($ilo, $inhi, $ihi) = _FindMarkers($y);

		# Stopping conditions	
		my $rtol = 2*abs($y->[$ihi]-$y->[$ilo])/(abs($y->[$ihi])+abs($y->[$ilo])+TINY);
		if ($rtol<$ftol) { last; } 
		if ($iter++>$itmax) {
		  	carp "Amoeba exceeded maximum iterations\n" if ($verbose); 
		  	last;
		}

		# Determine the Centroid
		if ($recalc) {
			$pbar = _CalcCentroid($p, $ihi);
		} else {
			_AdjustCentroid($pbar, $p, $ihi_o, $ihi);
		}

		# Reset the re-calculation flag, and remember the current highest
		$recalc = 0;

		# Determine the reflection point, evaluate its value
		$pr = _CalcReflection($pbar, $p->[$ihi], $ALPHA);
		$ypr = &$func(@$pr);

		 # if it gives a better value than best point, try an
		 # additional extrapolation by a factor gamma, accept best
		if ($ypr < $y->[$ilo]) {

			$pe = _CalcReflection($pbar, $pr, -$GAMMA);
		    $ype=&$func(@$pe);
		    if ($ype < $y->[$ilo]) { 
				$p->[$ihi] = $pe; $y->[$ihi] = $ype; 
			}
		    else { 
				$p->[$ihi] = $pr; $y->[$ihi] = $ypr; 
			}
		}
		# if reflected point worse than 2nd highest
		elsif ($ypr >= $y->[$inhi]) {

			# if it is better than highest, replace it
		    if ($ypr < $y->[$ihi] ) {
 		        $p->[$ihi] = $pr; $y->[$ihi] = $ypr; 
		    }

		    # look for an intermediate lower point by performing a
		    # contraction of the simplex along one dimension
		    $pc = _CalcReflection($pbar, $p->[$ihi], -$BETA);
		    $ypc = &$func(@$pc);
		    
			# if contraction gives an improvement, accept it
			if ($ypc < $y->[$ihi]) { 		        
				$p->[$ihi] = $pc; $y->[$ihi] = $ypc;
		    }
			# otherwise cant seem to remove high point
			# so contract around lo (best) point
		    else {
				for($i=0; $i<=$n; $i++) {
					if ($i!=$ilo) {
						$p->[$i] = _CalcReflection($p->[$ilo], $p->[$i], -$BETA);
						$y->[$i] = &$func(@{$p->[$i]});
					}
		        }
				$recalc = 1;
		    }
		}
		# if reflected point is in-between lowest and 2nd highest 
		else {
		    $p->[$ihi] = $pr; $y->[$ihi] = $ypr;
		}
		
		# Remember the replacing position and its value
		$ihi_o = $ihi;
	}

	return ($p->[$ilo],$y->[$ilo]);
}


# Helper function - find the lowest, 2nd highest and highest position
sub _FindMarkers
{
	my $y = shift;
    
	my ($ilo, $inhi, $ihi);
	my ($i, $n);

	$n = @$y - 1;
	
	$ilo=0;
	if ($y->[0]>$y->[1]) {
	    $ihi=0; $inhi=1;
	}
	else {
	    $ihi=1; $inhi=0;
	}

	for($i = 0; $i <= $n; $i++) {
	    if ($y->[$i] < $y->[$ilo]) { $ilo = $i; }
	    if ($y->[$i] > $y->[$ihi]) { $inhi = $ihi; $ihi = $i; }
	    elsif ($y->[$i] > $y->[$inhi] && $ihi != $i) { $inhi = $i; }
	}

	return ($ilo, $inhi, $ihi);
}

# Determine the centroid (except the highest point)		
sub _CalcCentroid
{
	my ($p, $ihi) = @_;
	my ($i, $j, $n);
	
	$n = @$p - 1; 

	my $pbar = [];
	for($j=0; $j<$n; $j++) {
		for($i=0; $i<=$n; $i++) {
		    if ($i!=$ihi) {
				$pbar->[$j] += $p->[$i][$j];
	        }
	    }
		$pbar->[$j] /= $n;
	}

	return $pbar;
}

# Adjust the centroid only
sub _AdjustCentroid
{
	my ($pbar, $p, $ihi_o, $ihi) = @_;
	my ($j, $n);

	$n = @$pbar;
	
	if ($ihi_o != $ihi) {
		for($j=0; $j<$n; $j++) {
			$pbar->[$j] += ($p->[$ihi_o][$j] - $p->[$ihi][$j]) / $n;
		}
	}
}	

# Determine the reflection point
sub _CalcReflection
{
	my ($p1, $p2, $scale) = @_;
	my $j;

	my $n = @$p1;

	my $pr = [];
	for($j=0; $j<$n; $j++) {
	    $pr->[$j] = $p1->[$j] + $scale*($p1->[$j]-$p2->[$j]);
	}

	return $pr;
}

return 1;

__END__

=head1 HISTORY

See "REAME".

=head1 BUGS

Let me know.

=head1 AUTHOR

John A.R. Williams <J.A.R.Williams@aston.ac.uk>

Tom Chau <tom@cpan.org>

=head1 SEE ALSO

"Numerical Recipies: The Art of Scientific Computing"
W.H. Press, B.P. Flannery, S.A. Teukolsky, W.T. Vetterling.
Cambridge University Press. ISBN 0 521 30811 9.

=head1 COPYRIGHT

Copyright (c) 1995 John A.R. Williams. All rights reserved.
This program is free software; you can redistribute it and/or
modify it under the same terms as Perl itself.

Since 2005, this module was co-developed with Tom.

