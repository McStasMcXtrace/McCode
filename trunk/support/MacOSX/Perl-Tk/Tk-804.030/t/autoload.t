BEGIN { $^W = 1; $| = 1;}
use Test;
plan tests => 5;
use Tk;
my $method;

sub warn_handler
{
 local $_ = shift;
 ok($_ =~ /^Assuming 'require Tk::$method;/,1,"Wrong warning:$_");
}

$SIG{'__WARN__'} = \&warn_handler;

my $mw = MainWindow->new;
$method = 'Nonwidget';
Tk::catch { $mw->$method() };
ok($@ =~ /Can't locate/,1,"Wrong error:$@");
$method = 'BrowseEntry';
$mw->$method();
ok(defined(&Tk::Widget::BrowseEntry),1,"Autoload failed");
$method = 'Entry';
$mw->$method();
ok(defined(&Tk::Widget::Entry),1,"Autoload failed");



