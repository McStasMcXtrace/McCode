#!/usr/bin/perl -w

use strict;

use Tk;

BEGIN {
    if (!eval q{
	use Test::More;
	1;
    }) {
	print "1..0 # skip: no Test::More module\n";
	exit;
    }
}

plan tests => 2;

sub main {
    my $mw = MainWindow->new;
    $mw->geometry('+10+10');
    my $w = $mw->Label(-text => 'bob');
    $w->bind('<ButtonPress-1>' => \&bump_flag);

    event_test($w, 1);
    my $junk = $w->id; # vivify the widget XID, by provoking Tk_MakeWindowExist
    event_test($w, 0);
}

my $flag;
sub bump_flag {
    $flag ++;

    return;
}

sub event_test {
    my ($w, $early) = @_;
    $flag = 0;
    my $got = do {
        $w->eventGenerate('<ButtonPress-1>');
        "flag=$flag";
    };

    if ($early) {
        is($got, 'flag=0', 'early event is ineffective');
    } else {
        is($got, 'flag=1', 'late event should bump_flag');
    }

    return;
}


main();
