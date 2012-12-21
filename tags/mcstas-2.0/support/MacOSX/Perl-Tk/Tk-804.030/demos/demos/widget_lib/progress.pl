# ProgressBar - display various progress bars.

use strict;
use Tk;
use Tk::ProgressBar;
use Tk::Scale;

my $mw = MainWindow->new;

my $status_var = 0;

my($fromv,$tov) = (0,100);
foreach my $loop (0..1) {
    my $res = 0;
    my $blks = 10;
    my @p = qw(top bottom left right);
    foreach my $dir (qw(n s w e)) {
        $mw->ProgressBar(
	    -borderwidth => 2,
	    -relief => 'sunken',
	    -width => 20,
	    -padx => 2,
	    -pady => 2,
	    -variable => \$status_var,
	    -colors => [0 => 'green', 50 => 'yellow' , 80 => 'red'],
	    -resolution => $res,
	    -blocks => $blks,
	    -anchor => $dir,
	    -from => $fromv,
	    -to => $tov
        )->pack(
	    -padx => 10,
	    -pady => 10,
	    -side => pop(@p),
	    -fill => 'both',
	    -expand => 1
	);
	$blks = abs($blks - ($res * 2));
	$res = abs(5 - $res);
    }
    ($fromv,$tov) = ($tov,$fromv);
}

$mw->Scale(-from => 0, -to => 100, -variable => \$status_var)->pack;

MainLoop;
