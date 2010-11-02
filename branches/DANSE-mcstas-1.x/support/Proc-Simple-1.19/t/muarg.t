#!/usr/bin/perl -w

use Proc::Simple;

### Test the new multi arg methods
$| = 1;

print "1..4\n";

$psh  = Proc::Simple->new();

check($psh->start("sleep", "1"));      # 1
while($psh->poll) { 
    sleep 1; }
check(!$psh->poll());                  # 2 Must be dead

sub mysleep { sleep(@_); }

check($psh->start(\&mysleep, 1));      # 3
while($psh->poll) {
    sleep 1; }
check(!$psh->poll());                  # 4 Must have been terminated

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

1;
