#!/usr/bin/perl -w
# -*- perl -*-

#
# $Id: $
# Author: Slaven Rezic
#

use strict;
use FindBin;
use File::Spec;

BEGIN {
    if (!eval q{
	use Test::More;
	use POSIX ":sys_wait_h";
	1;
    }) {
	print "1..0 # skip no Test::More and/or POSIX module\n";
	exit;
    }
    if ($^O eq 'MSWin32') {
	print "1..0 # skip not on Windows (because of fork, waitpid...)\n";
	exit;
    }
}

our $DEBUG = 0;

my @cmd = (
	   ['ptksh'],
	   ['ptked'],
	   ['ptked', "-encoding", "utf8", File::Spec->rel2abs($0)],
	   ['ptked', "-encoding", "iso-8859-1", File::Spec->rel2abs($0)],
	   ['gedi'],
	   ['gedi', File::Spec->rel2abs($0)],
	  );

chdir "$FindBin::RealBin/.."
    or die "Can't change to ptk directory: $!";

plan tests => scalar(@cmd);

OPT:
for my $opt (@cmd) {
    my $script = shift @$opt;
    my $testname = "Executing $script with " . (@$opt ? "@$opt" : "no args");
    my @cmd = ($^X, "-Mblib", "blib/script/$script", "-geometry", "+10+10", @$opt);
    my $pid = fork;
    if ($pid == 0) {
	warn "@cmd\n" if $DEBUG;
	open(STDERR, ">" . File::Spec->devnull);
	exec @cmd;
	die $!;
    }
    for (1..10) {
	select(undef,undef,undef,0.1);
	my $kid = waitpid($pid, WNOHANG);
	if ($kid) {
	    is($?, 0, "$testname (exited spontaneously)")
		or diag "@cmd";
	    next OPT;
	}
    }
    kill TERM => $pid;
    for (1..20) {
	select(undef,undef,undef,0.1);
	my $kid = waitpid($pid, WNOHANG); # reap zombie
	if (!kill 0 => $pid) {
	    pass("$testname (killed with TERM)");
	    next OPT;
	}
    }
    kill KILL => $pid;
    pass("$testname (killed with KILL)");
}

__END__
