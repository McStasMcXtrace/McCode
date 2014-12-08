# Tiler, arrange widgets in rows.

use strict;
use Tk;
use Tk::Tiler;

my $mw = MainWindow->new();
my $tiler = $mw->Scrolled('Tiler');
my $num = $tiler->cget('-rows') * $tiler->cget('-columns');
$mw->Label(-text => "Tiler with $num widgets")->pack;
foreach (1 .. $num)   {
    $tiler->Manage( $tiler->Label(-text => "**$_**") );
}
$tiler->pack(qw/-expand yes -fill both/);
MainLoop;
