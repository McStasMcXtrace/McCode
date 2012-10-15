#!/usr/bin/perl -w

use Proc::Simple;

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

print "1..80\n";

###
### Multiple Processes Test
###
#Proc::Simple->debug(1);

foreach $i (0..19) {
    $psh[$i] = Proc::Simple->new();  
}

foreach $i (@psh) {
    check($i->start("sleep 60"));        # 1-20
}

foreach $i (@psh) {
    while(!$i->poll) { 
        sleep 1; }
    check($i->poll());                   # Check each process, kill it
    check($i->kill());                   # and check again: 21-80
    while($i->poll) { 
        sleep 1; }
    check(!$i->poll());                  
}

1;

