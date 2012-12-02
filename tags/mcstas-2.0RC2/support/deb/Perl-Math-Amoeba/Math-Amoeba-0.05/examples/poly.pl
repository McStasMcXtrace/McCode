use strict;

########################################################################
# This script is to test the ability of the optimizer to tune:
# 
# 	f(x) = sum_i (x_i - i) 	where i from 1 to N.
# 
########################################################################

use Math::Amoeba qw(MinimiseND);

&main();

sub main
{
	my $N = 10;
	
	my $tol = 1e-5;
	my $maxiter = 10000;

	my $y = [1..$N];
	my $guess = [ map { rand()*2 - 1 } (1..$N) ];
	my $scale = [ map { 1 } (1..$N) ]; 

	my $func = sub {
		my $x = [@_];

		my $cost = 0;
		foreach my $i (0..$N-1) {
			$cost += ($x->[$i] - $y->[$i])**2;	
		}
		return $cost;
	};
	
	my ($opt, $cost) = MinimiseND($guess, $scale, \&$func, $tol, $maxiter);

	printf "[%s]: $cost\n", join(',', map { sprintf("%.2f", $_) } @$opt);
}

