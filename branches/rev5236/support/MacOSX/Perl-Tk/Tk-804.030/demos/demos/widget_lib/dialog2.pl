# dialog2.pl

use subs qw/see_code/;
use vars qw/$DIALOG2/;

sub dialog2 {

    my($demo) = @_;

    my($ok, $can, $see) = ('OK', 'Cancel', 'See Code');
    if (not Exists($DIALOG2)) {
	$DIALOG2 = $MW->Dialog(
	    -title          => 'Dialog with global grab',
            -text           => '',
            -bitmap         => 'info',
            -default_button => $ok,
            -buttons        => [$ok, $can, $see],
        );
	$DIALOG2->configure(
            -wraplength => '4i',
            -text       => 'This dialog box uses a global grab, so it prevents you from interacting with anything on your display until you invoke one of the buttons below.  Global grabs are almost always a bad idea; don\'t use them unless you\'re truly desperate.',
        );
    }

    my $button = $DIALOG2->Show('-global');

    print "You pressed OK\n" if $button eq $ok;
    print "You pressed Cancel\n" if $button eq $can;
    see_code 'dialog2' if $button eq $see;

} # end dialog2

1;
