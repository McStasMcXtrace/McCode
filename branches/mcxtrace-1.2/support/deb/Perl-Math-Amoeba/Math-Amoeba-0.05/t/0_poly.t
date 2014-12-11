use strict;

########################################################################
# This script is to test the ability of the optimizer to tune:
# 
# 	f(x) = sum_i (x_i - i) 	where i from 1 to N.
# 
########################################################################

use Test::More (tests => 11);

sub BEGIN
{
	use_ok('Math::Amoeba', 0.02, 'MinimiseND');
}

sub testPoly
{
	my $tol = 1e-5;

	my $N = 9;
	my $y = [1..$N];

	my $func = sub {
		my $x = [@_];

		my $cost = 0;
		foreach my $i (0..$N-1) {
			$cost += ($x->[$i] - $y->[$i])**2;	
		}
		return $cost;
	};
	
	my $guess = [ map { rand()*2 - 1 } (1..$N) ];
	my $scale = [ map { 1 } (1..$N) ]; 
	my ($opt, $cost) = MinimiseND($guess, $scale, \&$func, $tol, 10000);

	ok(abs($cost) < $tol, "Optimization success ($cost vs 0)");
	foreach my $i (0..$N-1) {
		ok(
			abs($opt->[$i] - $y->[$i]) < 1e-2, 
			sprintf(
				qq{The %d th parameter error okay (%.5f vs %.5f).},
				$i, $opt->[$i], $y->[$i]
			)
		);
	}
}

&testPoly();
