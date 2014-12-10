use blib;
use Tk;
my $mw = MainWindow->new;
my $kind = shift;
die "usage: $0 widgetclass" if !$kind;
require "Tk/$kind.pm";
my $w = $mw->$kind();
if ($w->isa('Tk::Wm'))
 {
  $w->deiconify;
 }
else
 {
  $w->pack;
 }
$mw->update;
$mw->after(1000);
$mw->after(5000,[destroy => $mw]);
MainLoop;
