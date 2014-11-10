# DirTree, display directory tree.

use Tk;
use Tk::DirTree;
my $top = MainWindow->new;
my $dl  = $top->Scrolled('DirTree')->pack(-expand => 1 , -fill => 'both');
MainLoop;
