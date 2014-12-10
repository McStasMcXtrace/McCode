# dialog1.pl

use subs qw/see_code/;
use vars qw/$DIALOG1/;

sub dialog1 {

    my($demo) = @_;

    my($ok, $can, $see) = ('OK', 'Cancel', 'See Code');
    if (not Exists($DIALOG1)) {
	$DIALOG1 = $MW->Dialog(
	    -title          => 'Dialog with local grab',
            -text           => '',
            -bitmap         => 'info',
            -default_button => $ok,
            -buttons        => [$ok, $can, $see],
        );
	$DIALOG1->configure(
            -wraplength => '4i',
            -text       => 'This is a modal dialog box.  It uses Tk\'s "grab" command to create a "local grab" on the dialog box.  The grab prevents any pointer-related events from getting to any other windows in the application until you have answered the dialog by invoking one of the buttons below.  However, you can still interact with other applications.',
        );
    }

    my $button = $DIALOG1->Show;

    print "You pressed OK\n" if $button eq $ok;
    print "You pressed Cancel\n" if $button eq $can;
    see_code 'dialog1' if $button eq $see;

} # end dialog1

1;
