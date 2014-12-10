#!/usr/bin/perl -w
# -*- perl -*-

# See https://rt.cpan.org/Ticket/Display.html?id=7474
# Marked as unimportant

use strict;

use Tk;

BEGIN {
    if (!eval q{
	use Test::More;
	1;
    }) {
	print "1..0 # skip no Test::More module\n";
	exit;
    }
}

plan tests => 1;

my $mw = MainWindow->new;
my $button = $mw->Button->grid;
$button->destroy;
# segfaults:
$button->grid;
pass 'No segfault if placing a destroyed widget with grid()';

__END__
