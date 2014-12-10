# Form, window management by Tix->form.

use Tk;

my $Main = MainWindow->new();

my $box4 = $Main->Label(-text => 'box4', -borderwidth => 1, -relief => "raised");
my $box1 = $Main->Label(-text => 'box1',-borderwidth => 1, -relief => "raised");
my $box2 = $Main->Label(-text => 'box2',-borderwidth => 1, -relief => "raised");
my $box3 = $Main->Label(-text => 'box3',-borderwidth => 1, -relief => "raised");

$box1->form(-top => '%0', -left => '%0', -right => '%100');
$box2->form(-top => $box1, -left => '%0', -right => '%50', -bottom =>
$box4);
$box3->form(-top => $box1, -left => $box2, -right => '%100', -bottom =>
$box4);
$box4->form(-left => '%0', -right => '%100', -bottom => '%100');

MainLoop;
__END__

