# -*- perl -*-
BEGIN { $|=1; $^W=1; }
use strict;
use Test;
use Tk;

BEGIN {
    if ($Tk::platform eq 'MSWin32') {
	plan test => 1;
    } else {
	plan test => 7; #, todo => [1];
    }
}

my $mw;
$mw = Tk::MainWindow->new();
$mw->geometry('+10+10');  # This works for mwm and interactivePlacement

if ($Tk::platform eq 'MSWin32') {
    my $curfile = "demos/demos/images/cursor.cur";
    $mw->configure(-cursor => '@'.$curfile);
    $mw->update;
    ok($mw->cget(-cursor), '@'.$curfile);
} else {
    $mw->configure(-cursor => ['@demos/demos/images/cursor.xbm','black']);
    $mw->update;
    $mw->after(200);
    my $ret = $mw->cget(-cursor);
    ok(ref($ret) eq 'ARRAY');
    ok($ret->[0],'@demos/demos/images/cursor.xbm');
    ok($ret->[1],'black');
    eval { $mw->configure(-cursor => $ret) };
    ok($@,'');
    my $tclcurspec = '@demos/demos/images/cursor.xbm blue';
    $mw->configure(-cursor => $tclcurspec);
    $ret = $mw->cget(-cursor);
    ok(ref($ret) eq 'ARRAY');
    ok($ret->[0],'@demos/demos/images/cursor.xbm');
    ok($ret->[1],'blue');
}

__END__
