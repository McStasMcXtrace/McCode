package Tk::Panedwindow;
use strict;

use vars qw/$VERSION/;
$VERSION = '4.004'; # sprintf '4.%03d', q$Revision: #3 $ =~ /#(\d+)/;

# A Panedwindow widget (similar to Adjuster).

use Tk qw/Ev/;
use base qw/Tk::Widget/;

Construct Tk::Widget 'Panedwindow';

sub Tk_cmd { \&Tk::panedwindow }

Tk::Methods('add', 'forget', 'identify', 'proxy', 'sash', 'panes');

use Tk::Submethods (
    'proxy' => [qw/coord forget place/],
    'sash'  => [qw/coord mark place/],
);

sub ClassInit {

    my ($class,$mw) = @_;

    $class->SUPER::ClassInit($mw);

    $mw->bind($class, '<Button-1>' => ['MarkSash' => Ev('x'), Ev('y'), 1]);
    $mw->bind($class, '<Button-2>' => ['MarkSash' => Ev('x'), Ev('y'), 0]);
    $mw->bind($class, '<B1-Motion>' => ['DragSash' => Ev('x'), Ev('y'), 1]);
    $mw->bind($class, '<B2-Motion>' => ['DragSash' => Ev('x'), Ev('y'), 0]);
    $mw->bind($class, '<ButtonRelease-1>' => ['ReleaseSash' => 1]);
    $mw->bind($class, '<ButtonRelease-2>' => ['ReleaseSash' => 0]);
    $mw->bind($class, '<Motion>' => ['Motion' => Ev('x'), Ev('y')]);
    $mw->bind($class, '<Leave>' => ['Leave']);

    return $class;

} # end ClassInit

sub MarkSash {

    # MarkSash
    #
    # Handle marking the correct sash for possible dragging
    #
    # Arguments:
    #   w	the widget
    #   x	widget local x coord
    #   y	widget local y coord
    #   proxy	whether this should be a proxy sash
    # Results:
    #   None

    my ($w, $x, $y, $proxy) = @_;

    my @what = $w->identify($x, $y);
    if ( @what == 2 ) {
	my ($index, $which) = @what[0 .. 1];
	if (not $Tk::strictMotif or $which eq 'handle') {
	    $w->sashMark($index, $x, $y) if not $proxy;
	    $w->{_sash} = $index;
	    my ($sx, $sy) = $w->sashCoord($index);
	    $w->{_dx} = $sx - $x;
	    $w->{_dy} = $sy - $y;
	}
    }

} # end MarkSash

sub DragSash {

    # DragSash
    #
    # Handle dragging of the correct sash
    #
    # Arguments:
    #   w	the widget
    #   x	widget local x coord
    #   y	widget local y coord
    #   proxy	whether this should be a proxy sash
    # Results:
    #   Moves sash

    my ($w, $x, $y, $proxy) = @_;

    if ( exists $w->{_sash} ) {
	if ($proxy) {
	    $w->proxyPlace($x + $w->{_dx}, $y + $w->{_dy});
	} else {
	    $w->sashPlace($w->{_sash}, $x + $w->{_dx}, $y + $w->{_dy});
	}
    }

} # end DragSash

sub ReleaseSash {

    # ReleaseSash
    #
    # Handle releasing of the sash
    #
    # Arguments:
    #   w	the widget
    #   proxy	whether this should be a proxy sash
    # Results:
    #   Returns ...

    my ($w, $proxy) = @_;

    if ( exists $w->{_sash} ) {
	if ($proxy) {
	    my ($x, $y) = $w->proxyCoord;
	    $w->sashPlace($w->{_sash}, $x, $y);
	    $w->proxyForget;
	}
	delete $w->{'_sash', '_dx', '_dy'};
    }

} # end ReleaseSash

sub Motion {

    # Motion
    #
    # Handle motion on the widget. This is used to change the cursor
    # when the user moves over the sash area.
    #
    # Arguments:
    #   w	the widget
    #   x	widget local x coord
    #   y	widget local y coord
    # Results:
    #   May change the cursor. Sets up a timer to verify that we are still
    #   over the widget.

    my ($w, $x, $y) = @_;

    my @id = $w->identify($x, $y);
    if ( (@id == 2) and
	 (not $Tk::strictMotif or $id[1] eq 'handle') ) {
	if ( not exists $w->{_panecursor} ) {
	    $w->{_panecursor} = $w->cget(-cursor);
	    if ( not defined $w->cget(-sashcursor) ) {
		if ( $w->cget(-orient) eq 'horizontal' ) {
		    $w->configure(-cursor => 'sb_h_double_arrow');
		} else {
		    $w->configure(-cursor => 'sb_v_double_arrow');
		}
	    } else {
		$w->configure(-cursor => $w->cget(-sashcursor));
	    }
	    if ( exists $w->{_pwAfterId} ) {
		$w->afterCancel($w->{_pwAfterId});
	    }
	    $w->{_pwAfterId} = $w->after(150 => ['Cursor' => $w]);
	}
	return
    }
    if ( exists $w->{_panecursor} ) {
	$w->configure(-cursor => $w->{_panecursor});
	delete $w->{_panecursor};
    }

} # end Motion

sub  Cursor {

    # Cursor
    #
    # Handles returning the normal cursor when we are no longer over the
    # sash area.  This needs to be done this way, because the panedwindow
    # won't see Leave events when the mouse moves from the sash to a
    # paned child, although the child does receive an Enter event.
    #
    # Arguments:
    #   w	the widget
    # Results:
    #   May restore the default cursor, or schedule a timer to do it.

    my ($w) = @_;

    if ( exists $w->{_panecursor} ) {
	if ( $w->containing($w->pointerx, $w->pointery) == $w ) {
	    $w->{_pwAfterId} = $w->after(150 => ['Cursor' => $w]);
	} else {
	    $w->configure(-cursor => $w->{_panecursor});
	    delete $w->{_panecursor};
	    if ( exists $w->{_pwAfterId} ) {
		$w->afterCancel($w->{_pwAfterId});
		delete $w->{_pwAfterId};
	    }
	}
    }

} # end Cursor

sub Leave {

    # Leave
    #
    # Return to default cursor when leaving the pw widget.
    #
    # Arguments:
    #   w	the widget
    # Results:
    #   Restores the default cursor

    my ($w) = @_;

    if ( exists $w->{_panecursor} ) {
        $w->configure(-cursor => $w->{_panecursor});
        delete $w->{_panecursor};
    }

} # end Leave


1;
__END__
