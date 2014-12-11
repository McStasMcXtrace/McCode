my $divisor;
use Test::More (tests => 6);
use Tk;
use strict;
BEGIN {
    eval 'use Time::HiRes qw(time)';
    $divisor = $@ ? 1 : 10;
}
my $mw = MainWindow->new;
$mw->withdraw;
my $start = time;

local $TODO;
if ($^O =~ m{^(MSWin32|cygwin)$}) {
    $TODO = "May fail on Windows-like systems, see https://rt.cpan.org/Ticket/Display.html?id=57009";
}

$mw->after(1000/$divisor,sub { my $t = time;
			       isnt($t,$start);
			       my $expected_min = $start+1/$divisor;
			       my $expected_max = $start+3/$divisor;
			       cmp_ok($t, ">=", $expected_min, "short after: $t >= $expected_min");
			       if ($t <= $expected_max) {
				   pass("$t <= $expected_max");
			       } else {
				   local $TODO = "Probably loaded machine";
				   fail("$t is not <= $expected_max");
			       }
			   });
$mw->after(2000/$divisor,sub { my $t = time;
			       my $expected_min = $start+2/$divisor;
			       my $expected_max = $start+4/$divisor;
			       cmp_ok($t, ">=", $expected_min, "longer after: $t >= $expected_min");
			       if ($t <= $expected_max) {
				   pass("$t <= $expected_max");
			       } else {
				   local $TODO = "Probably loaded machine";
				   fail("$t is not <= $expected_max");
			       }
			   });
$mw->after(3000/$divisor,[destroy => $mw ]);
MainLoop;
cmp_ok(time, ">=", $start+3/$divisor);

