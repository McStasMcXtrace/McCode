package Tk::Dialog;

use vars qw($VERSION);
$VERSION = '4.005'; # $Id: //depot/Tkutf8/Tk/Dialog.pm#4 $

# Dialog - a translation of `tk_dialog' from Tcl/Tk to TkPerl (based on
# John Stoffel's idea).
#
# Stephen O. Lidie, Lehigh University Computing Center.  94/12/27
# lusol@Lehigh.EDU

# Documentation after __END__

use Carp;
use strict;
use base qw(Tk::DialogBox);

Construct Tk::Widget 'Dialog';

sub Populate
{

    # Dialog object constructor.  Uses `new' method from base class
    # to create object container then creates the dialog toplevel.

    my($cw, $args) = @_;

    $cw->SUPER::Populate($args);

    my ($w_bitmap,$w_but,$pad1,$pad2);

    # Create the Toplevel window and divide it into top and bottom parts.

    my (@pl) = (-side => 'top', -fill => 'both');

    ($pad1, $pad2) =
        ([-padx => '3m', -pady => '3m'], [-padx => '3m', -pady => '2m']);


    $cw->iconname('Dialog');

    my $w_top = $cw->Subwidget('top');

    # Fill the top part with the bitmap and message.

    @pl = (-side => 'left');

    $w_bitmap = $w_top->Label(Name => 'bitmap');
    $w_bitmap->pack(@pl, @$pad1);

    my $w_msg = $w_top->Label( -wraplength => '3i', -justify    => 'left' );

    $w_msg->pack(-side => 'right', -expand => 1, -fill => 'both', @$pad1);

    $cw->Advertise(message => $w_msg);
    $cw->Advertise(bitmap  => $w_bitmap );

    $cw->ConfigSpecs( -image      => ['bitmap',undef,undef,undef],
                      -bitmap     => ['bitmap',undef,undef,undef],
                      -font       => ['message','font','Font', '-*-Times-Medium-R-Normal--*-180-*-*-*-*-*-*'],
                      DEFAULT     => ['message',undef,undef,undef]
                     );
}

1;

__END__

