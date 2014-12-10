#!/usr/bin/perl -w
use strict;
use Test;
BEGIN { plan tests =>18, todo => [] }

use Tk;
my $mw = MainWindow->new;
$mw->geometry('+100+100');
my @l;
my $lb = $mw->Listbox()->pack;
ok(defined($lb),1,"Widget Created");
$lb->insert(0,qw(p q r));
my $v = $lb->cget('-listvariable');
ok(!defined($v));
ok($lb->get(0),'p');
ok($lb->get($lb->index('end')-1),'r');

$lb->configure(-listvariable => \@l);
ok($lb->cget('-listvariable'),\@l);

@l = (qw/a b c d e f/);
ok($l[0],'a');
ok($l[5],'f');
ok(@l,6);
ok($#l,5);
ok($lb->index('end'),@l);
push(@l,qw(more things));
for my $i (reverse 0..$#l)
 {
  my $e = $l[$i];
  my $v = $lb->get($i);
  ok($v,$e,"Value at $i is $e");
 }

$lb->after(1000,[destroy => $mw]);
MainLoop;




