#!/usr/bin/perl -w

use Proc::Simple;

package EmptySubclass;
@ISA = qw(Proc::Simple);
1;


package Main;

###
### check(1) -> print #testno ok
### check(O) -> print #testno not ok
###
sub check {
    my ($yesno) = @_;

    $nu = 1 unless defined $nu;
    print($yesno ? "ok $nu\n" : "not ok $nu\n");
    $nu++;
}

$| = 1;

print "1..2\n";

###
### Empty Subclass test
###
$psh  = EmptySubclass->new();

check($psh->start("sleep 10"));        # 1

while(!$psh->poll) { 
    sleep 1; }

check($psh->kill());                   # 2

while($psh->poll) { 
    sleep 1; }

1;
