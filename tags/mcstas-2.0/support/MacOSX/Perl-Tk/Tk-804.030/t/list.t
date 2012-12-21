BEGIN { $^W = 1; $| = 1;}
use strict;
use Test;
use Tk;
use Tk::Font;
my $mw = new MainWindow;
my $l1 = $mw->Label( -text => " 6 point font", -font => ['Helvetica',6,'bold']);
my $f = $l1->cget('-font');
my %expect = $f->actual;
my @k = keys(%expect);
my $l2 = $mw->Label( -text => " 6 point font", -font => '{Helvetica}  6  {bold}');
plan tests => scalar(@k);
my $f2 = $l2->cget('-font');
my %a  = $f2->actual;
foreach my $k (@k)
 {
  ok($a{$k},$expect{$k},"Wrong $k");
 }
