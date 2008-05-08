#!/usr/bin/perl -w
##################################################
# Check the exit status feature
##################################################

use Proc::Simple;

#Proc::Simple::debug(1);

$proc = Proc::Simple->new();

print "1..1\n";

$proc->start("ls . >/dev/null");
while($proc->poll()) {
    sleep(1);
}

if(defined $proc->exit_status()) {
    $stat = $proc->exit_status();
} else {
    $stat = "undef";
}
Proc::Simple->dprt("EXIT: '$stat'");

open PIPE, "ls |" or die "Cannot open pipe";
my $data = <PIPE>;
close PIPE or die "Cannot close pipe";

if(defined $proc->exit_status()) {
    $stat = $proc->exit_status();
} else {
    $stat = "undef";
}
Proc::Simple->dprt("EXIT: '$stat'");

if($stat eq 0) {
    print "ok 1\n";
} else {
    print "not ok 1\n";
}
