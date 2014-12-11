BEGIN { $|=1; $^W=1; }
use strict;
use Test;
use Tk;
plan test => 7;
my $var = 'One';
my $mw = MainWindow->new;
my $e  = $mw->Entry(-textvariable => \$var)->pack;
ok($e->get,$var,"Entry not initialized from variable");
$e->delete(0,'end');
ok($var,'',"Delete does not change variable");
$e->insert(0,'Two');
ok($var,'Two',"Insert does not change variable");
$var = 'Three';
ok($e->get,$var,"Entry does not track variable assignment");
chop($var);
ok($e->get,'Thre',"Entry does not track chop-ing variable");

my $nv;
$mw->Entry(-textvariable => \$nv);
$nv = 3/2;
ok($nv, 3/2, "IV should not override NV");

my $pv_chop = 421;
chop($pv_chop);
$mw->Entry(-textvariable => \$pv_chop);
ok($pv_chop, "42", "PV flag not set");

__END__
