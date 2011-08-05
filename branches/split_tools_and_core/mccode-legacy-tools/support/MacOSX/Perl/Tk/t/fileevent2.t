#!/usr/bin/perl -w
# -*- perl -*-

#
# $Id: $
# Author: Slaven Rezic
#

use strict;

use Tk;

BEGIN {
    if (!eval q{
	use 5.006; # three-arg open
	use Test::More;
	1;
    }) {
	print "1..0 # skip: no Test::More module\n";
	exit;
    }
}

plan tests => 1;

my @fh;
my $callback_called = 0;

my $mw = tkinit;
$mw->geometry("+10+10");
$mw->idletasks;

# A variant of the problem reported in
# http://rt.cpan.org/Ticket/Display.html?id=32034
#
# tclUnixNotify.c used to do bit-handling for the select() mask
# itself, but this was broken for 64bit machines.
for (1..100) {
    open my $dup, "<&", \*STDIN or die "Can't dup STDIN: $!";
    push @fh, $dup;
    $mw->fileevent($dup, "readable", sub { $callback_called++ });
}

$mw->after(300, sub { $mw->destroy });
MainLoop;

local $TODO;
$TODO = "Known to break on $^O" if $^O eq 'cygwin';

is($callback_called, 0, "Fileevent callback should never be called");

__END__
