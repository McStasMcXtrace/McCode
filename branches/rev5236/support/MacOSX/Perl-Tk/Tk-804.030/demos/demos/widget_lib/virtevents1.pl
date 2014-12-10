use strict;

sub virtevents1 {

    my( $demo ) = @_;

    my $mw = $MW->WidgetDemo(
        -name             => $demo,
        -text             => [ "This demonstration shows how you can use keysyms (keyboard symbols) to programmatically synthesize events that simulate a person typing on the keyboard. To learn about keyboard keysyms, run to previous demonstration, \"Show keyboard symbols\".

A virtual event named <<pTkRules>> is defined that is activated by pressing the \"caps lock\" key (go ahead, press \"caps lock\"). A callback is bound to that virtual event - the callback synthesizes physicals events that \"type\" into the Entry widget displayed below.  Pressing the \"Synthesize\" Button calls eventGenerate(), which synthesizes the virtual event <<pTkRules>> directly.

Warning:  it's easy to make this demonstration recurse indefinitely because synthesized physical events behave just like the real thing. So, it's possible for the <<pTkRules>> callback to eventGenerate() the keysym that activates the <<pTkRules>> virtual event, which invokes the <<pTkRules>> callback to eventGenerate() the keysym that activates the <<pTkRules>> virtual event, which ...", -wraplength => '6i' ],
        -title            => 'Simulate KeyPress events.',
        -iconname         => 'vevents1',
    );

    # Define a virtual event - <<pTkRules>> - that is activated when
    # the physical event - pressing the "caps lock" key - occurs. 

    $mw->eventAdd( qw/ <<pTkRules>> <Caps_Lock> / );

    # Alphabetics are their own keysyms.  The %keysyms hash maps other
    # characters to their keysym string. To see the keysyms associated
    # with keyboard characters run the previous widget demonstration.

    my %keysyms = (' ' => 'space', '/' => 'slash', '!' => 'exclam' );

    # Create an Entry widget for a person or this program to type into.
    # The Button explicitly generates the virtual event.

    my $e = $mw->Entry->pack;
    my $b = $mw->Button(
	-command => sub { $mw->eventGenerate( '<<pTkRules>>' ) },
        -text    => 'Synthesize <<pTkRules>>',
    )->pack;

    # Now bind the virtual event to a callback that "types" for us.
    
    $mw->bind( qw/ <<pTkRules>> / => sub {

	# This subroutine is invoked whenever the "caps lock" key is
	# pressed or the virtual event <<pTkRules>> is programatically
	# generated via eventGenerate.

	$e->focus;
	$mw->update;
	my $string_to_type = 'Perl/Tk rules!';

	foreach ( split '', $string_to_type ) {
	    $_ = $keysyms{$_} if exists $keysyms{$_};
	    $e->eventGenerate( '<KeyPress>', -keysym => $_ );
	    $mw->idletasks;
	    $mw->after( 100 );

	} # end sub type characters
    } );

} # end virtevents1
