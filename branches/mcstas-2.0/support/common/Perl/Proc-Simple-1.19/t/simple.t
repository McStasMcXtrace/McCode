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

print "1..10\n";

###
### Simple Test
###

### Shell commands

$psh  = Proc::Simple->new();

check($psh->start("sleep 1"));         # 1
while($psh->poll) { 
    sleep 1; }
check(!$psh->poll());                  # 2 Must have been terminated

check($psh->start("sleep 10"));        # 3
while(!$psh->poll) { 
    sleep 1; }
check($psh->kill());                   # 4
while($psh->poll) { 
    sleep 1; }
check(!$psh->poll());                  # 5 Must have been terminated


### Perl subroutines
$psub  = Proc::Simple->new();

check($psub->start(sub { sleep 1 }));  # 6
while($psub->poll) { 
    sleep 1; }
check(!$psub->poll());                 # 7 Must have been terminated

check($psub->start(sub { sleep 10 })); # 8
while(!$psub->poll) { 
    sleep 1; }

check($psub->kill("SIGTERM"));         # 9
while($psub->poll) { 
    sleep 1; }
check(!$psub->poll());                 # 10 Must have been terminated

1;
