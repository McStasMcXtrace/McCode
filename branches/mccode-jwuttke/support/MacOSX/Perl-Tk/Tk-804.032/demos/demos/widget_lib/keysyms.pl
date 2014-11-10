use strict;

sub keysyms {

    my( $demo ) = @_;

    my $mw = $MW->WidgetDemo(
        -name             => $demo,
        -text             => 'This demonstration displays the keysym for any keyboard character.',
        -title            => 'Display Keysyms',
        -iconname         => 'keysyms',
    );

    $mw->Label( qw/ -relief solid -width 20 /, -textvariable => \my $k )->pack;

    $mw->bind( '<KeyPress>' => sub {
        $k = sprintf( "%s", $Tk::event->K );
    });

} # end keysyms
