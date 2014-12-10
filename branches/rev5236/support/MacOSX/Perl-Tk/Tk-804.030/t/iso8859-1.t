use Tk;
use Test;
#use utf8;
plan tests => (0x100-0xa0)*1;
my $mw  = MainWindow->new;
my @but;

for my $i (0xa0..0xff)
 {
  my $s = chr($i);
  push(@but,$mw->Button(-text => $s));
  my $x = $but[-1]->cget('-text');
  ok(ord($x),$i,"Number not equal");
  # ok($x,$s,"Text not equal");
  if ($i % 16 == 15)
   {
    Tk::grid(splice(@but,0,16));
   }
 }
$mw->geometry("+20+20");
$mw->after(2000,[destroy => $mw]);
MainLoop;

