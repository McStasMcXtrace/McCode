package Tk::Stats;

($lu,$ls) = times;


use vars qw($VERSION);
$VERSION = '4.004'; # $Id: //depot/Tkutf8/Tk/Stats.pm#4 $

sub stats
 {
  my ($u,$s) = times;
  my $du = $u-$lu;
  my $ds = $s-$ls;
  $ls = $s;
  $lu = $u;
  print sprintf(' dt=%4.2f du=%4.2f  ds=%4.2f',$du+$ds,$du,$ds);
  print sprintf('  t=%4.2f u=%4.2f   s=%4.2f',$u+$s,$u,$s);
  print ' ',shift,"\n";
 }

sub import
{
 stats($_[1]);
}

1;
