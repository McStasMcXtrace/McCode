# form.pl

use vars qw/$TOP/;

sub form {

    # Create a top-level window that displays a bunch of entries with
    # tabs set up to move between them.

    my($demo) = @_;
    $TOP = $MW->WidgetDemo(
        -name     => $demo,
        -text     => 'This window contains a simple form where you can type in the various entries and use tabs to move circularly between the entries.',
        -title    => 'Form Demonstration',
        -iconname => 'form',
    );
    my $f = $TOP->Frame->pack(-fill => 'both');
    my $row = 0;
    foreach ('Name:', 'Address:', '', '', 'Phone:') {
	my $e = $f->Entry(qw/-relief sunken -width 40/);
	my $l = $f->Label(-text => $_, -anchor => 'e', -justify => 'right');
        $l->grid(-row => $row, -column => 0, -sticky => 'e');
        $e->grid(-row => $row++, -column => 1,-sticky => 'ew');
        $f->gridRowconfigure(1,-weight => 1);
	$e->focus if $_ eq 'Name:';
    }
    $TOP->bind('<Return>' => [$TOP => 'destroy']);

} # end form

1;
