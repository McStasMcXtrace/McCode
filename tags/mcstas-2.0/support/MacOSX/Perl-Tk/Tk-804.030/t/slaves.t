BEGIN { $|=1; $^W=1; }
use Test;
use Tk;

plan test => 12;

my $mw = MainWindow->new;

$x=$y=0;
foreach $geom ( qw/ pack place grid form / ) {
    $f=$mw->Toplevel(qw/-bd 4 -relief solid /);
    $f->title($geom);
    foreach ( 1 .. 3 ) {
	@p = ();
	if ($geom eq 'place') {
	    $x+=20;
	    $y=$x;
	    @p=('-x', $x, '-y', $y);
	}
	$f->Label(-text => $_)->$geom(@p);
    }
    $s="${geom}Slaves";
    #print "$geom slaves=", join(' ', $f->$s), "!\n";
    foreach $s ($f->$s) {
	ok( sub { return ( ref( $s ) ) ? 1 : 0 }, 1, "$geom slave '$s' not a reference.");
    }
}
