BEGIN { $^W = 1; $| = 1;}
use strict;
use Test;
use Tk;
use Tk::Photo;


my $mw  = MainWindow->new();
$mw->geometry('+100+100');

plan tests => 8;

my $c = $mw->Canvas->pack;
my $p = $mw->Photo(-height => 10, -width => 10);

$c->configure(-offset => "ne");
ok($c->cget(-offset), "ne", "-offset wrong (anchor)");

$c->configure(-offset => [10,20]);
my $a = $c->cget(-offset);
ok($a->[0], 10, "-offset wrong (x value)");
ok($a->[1], 20, "-offset wrong (y value)");

$c->configure(-offset => ['#',10,20]);
$a = $c->cget(-offset);
ok($a->[0], '#', "-offset wrong (relative)");
ok($a->[1], 10, "-offset wrong (x value)");
ok($a->[2], 20, "-offset wrong (y value)");

eval { $c->configure(-offset => "wrong") };
ok($@ =~ /bad offset/, 1, "no error detected");

eval { $c->configure(-tile => $p) };
ok($@, '', "cannot set -tile");
