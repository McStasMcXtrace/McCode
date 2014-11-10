BEGIN { $^W = 1; $| = 1;}
use strict;
use FindBin;
use lib $FindBin::RealBin;
use Test::More;
use Tk;
use Tk::widgets qw(ProgressBar);

use TkTest qw(create_placeholder_widget);

plan tests => 27;

my $mw  = MainWindow->new();
$mw->geometry('+100+100');

create_placeholder_widget $mw;

my $var = 0;

my $pb  = $mw->ProgressBar(-bd => 3, -relief => 'raised', -fg => 'blue', -variable => \$var)->pack;
ok defined($pb), "Create progress bar";

ok defined(tied($var)), "Variable tied";
is $pb->cget('-from'), 0, "from";
is $pb->cget('-to'), 100, "to";

for my $v (map(10*$_+3,1..10))
 {
  $var = $v;
  is $pb->cget('-value'), $v, "Value per cget is $v";
  is $pb->value, $v, "Value per method is $v";
  $mw->update;
 }

# reconfigure widget
{
 $pb->configure(-colors => [0,'red'], -length => 100);
 $pb->update;
 my $w1 = $pb->Width;
 $pb->configure(-length => 200);
 $pb->update;
 my $w2 = $pb->Width;
 cmp_ok $w2, ">", $w1, "width was $w1 and is now $w2";

 $pb->configure(-colors => [0,'blue']);
 $pb->configure(-borderwidth => 0);
 $pb->update;
}

{
 # The progress bar from SYNOPSIS
 my $percent_done;
 my $progress = $mw->ProgressBar(
	-width => 20,
	-length => 200,
        -anchor => 's',
	-from => 0,
	-to => 100,
	-blocks => 10,
	-colors => [0, 'green', 50, 'yellow' , 80, 'red'],
	-variable => \$percent_done
 );
 $progress->value(50);
 is $percent_done, 50;
}

$mw->destroy;
ok !defined(tied($var)), "Variable is not tied anymore";

