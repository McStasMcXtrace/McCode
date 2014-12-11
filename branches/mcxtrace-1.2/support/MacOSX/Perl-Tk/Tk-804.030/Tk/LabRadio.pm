# Class LabeledRadiobutton

package Tk::LabRadiobutton;

use vars qw($VERSION);
$VERSION = '4.004'; # $Id: //depot/Tkutf8/Tk/LabRadio.pm#4 $

require Tk::Frame;
use base  qw(Tk::Frame);

Construct Tk::Widget 'LabRadiobutton';


# Although there is no fundamental reason why -radiobuttons
# should be fixed at create time converting to METHOD form
# is extra work an this can serve as an example of CreateArgs
# checking.

sub CreateArgs
{
 my ($package,$parent,$args) = @_;
 $parent->BackTrace("Must specify -radiobuttons for $package")
    unless (defined $args->{'-radiobuttons'});
 return $package->SUPER::CreateArgs($parent,$args);
}

sub Populate
{
    require Tk::Radiobutton;

    my ($cw,$args) = @_;
    $cw->SUPER::Populate($args);

    # LabeledRadiobutton(s) constructor.
    #
    # Advertised subwidgets:  the name(s) of your radiobutton(s).



    my (@widgets) = ();

    my $rl;
    foreach $rl (@{$args->{'-radiobuttons'}})
     {
       my $r = $cw->Component( Radiobutton => $rl,
                               -text     => $rl,
                               -value    => $rl );
       $r->pack(-side => 'left', -expand => 1, -fill => 'both');
       push(@widgets,$r);
       $cw->{Configure}{-value} = $rl;
     }

    $cw->BackTrace('No buttons') unless (@widgets);

    $cw->ConfigSpecs('-variable'     => [ \@widgets, undef, undef, \$cw->{Configure}{-value} ],
                     '-radiobuttons' => [ 'PASSIVE', undef, undef, undef ],
                     '-value'        => [ 'PASSIVE', undef, undef, $cw->{Configure}{-value} ],
                     'DEFAULT'       => [ \@widgets ]
                    );
}


1;
