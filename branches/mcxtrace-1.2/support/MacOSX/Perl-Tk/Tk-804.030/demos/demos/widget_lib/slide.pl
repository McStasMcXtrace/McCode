# slide.pl

$Tk::SlideSwitch::VERSION = '1.1';

package Tk::SlideSwitch;

use Tk;
use Tk::widgets qw/Label Scale/;
use base qw/Tk::Frame/;
use strict;

Construct Tk::Widget 'SlideSwitch';

sub Populate {

    my($self, $args) = @_;

    $self->SUPER::Populate($args);

    my $ll = $self->Label->pack(-side => 'left');
    my $sl = $self->Scale->pack(-side => 'left');
    my $rl = $self->Label->pack(-side => 'left');

    $self->ConfigSpecs(
        -command      => [$sl,        qw/command      Command            /],  
        -from         => [$sl,        qw/from         From              0/],
        -highlightthickness => [$sl,
            qw/highlightThickness HighlightThickness                    0/],
        -length       => [$sl,        qw/length       Length           30/],
        -llabel       => [qw/METHOD      llabel       Llabel             /],
        -orient       => [$sl,        qw/orient       Orient   horizontal/],
        -rlabel       => [qw/METHOD      rlabel       Rlabel             /],  
        -showvalue    => [$sl,        qw/showValue    ShowValue         0/],
        -sliderlength => [$sl,        qw/sliderLength SliderLength     15/],
        -sliderrelief => [$sl,        qw/sliderRelief SliderRelief raised/],
        -to           => [$sl,        qw/to           To                1/],
        -troughcolor  => [$sl,        qw/troughColor  TroughColor        /],
        -width        => [$sl,        qw/width        Width             8/],
        -variable     => [$sl,        qw/variable     Variable           /],
        'DEFAULT'     => [$ll, $rl],
    );

    $self->{ll} = $ll;
    $self->{sl} = $sl;
    $self->{rl} = $rl;

    $self->bind('<Configure>' => sub {
	my ($self) = @_;
	my $orient = $self->cget(-orient);
	return if $orient eq 'horizontal';
	my ($ll, $sl, $rl) = ($self->{ll}, $self->{sl}, $self->{rl});
	$ll->packForget;
	$sl->packForget;
	$rl->packForget;
	$ll->pack;
	$sl->pack;
	$rl->pack;
    });

} # end Populate

# Private methods and subroutines.

sub llabel {
    my ($self, $args) = @_;
    $self->{ll}->configure(@$args);
} # end llabel

sub rlabel {
    my ($self, $args) = @_;
    $self->{rl}->configure(@$args);
} # end rlabel

1;

package main;

use vars qw / $TOP /;
use strict;

sub slide {

    my( $demo ) = @_;

    $TOP = $MW->WidgetDemo(
        -name             => $demo,
        -text             => "This demonstration creates a new composite SlideSwitch widget that can be either on or off. The widget is really a customized Scale widget.",
        -title            => 'A binary sliding switch',
        -iconname         => 'slide',
    );

    my $mw = $TOP;

    my $sl = $mw->SlideSwitch(
        -bg          => 'gray',
        -orient      => 'horizontal',
        -command     => sub {print "Switch value is @_\n"},
        -llabel      => [-text => 'OFF', -foreground => 'blue'],
        -rlabel      => [-text => 'ON',  -foreground => 'blue'],
        -troughcolor => 'tan',
    )->pack(qw/-side left -expand 1/);

} # end slide

__END__

=head1 NAME

Tk::SlideSwitch - a 2 position horizontal or vertical switch.

=head1 SYNOPSIS

 use Tk::SlideSwitch;

 my $sl = $frame1->SlideSwitch(
     -bg          => 'gray',
     -orient      => 'horizontal',
     -command     => [$self => 'on'],
     -llabel      => [-text => 'OFF', -foreground => 'blue'],
     -rlabel      => [-text => 'ON',  -foreground => 'blue'],
     -troughcolor => 'tan',
 )->pack(qw/-side left -expand 1/);

=head1 DESCRIPTION

Tk::SlideSwitch is a Frame based composite mega-widget featuring a binary Scale
widget surrounded by two Label widgets.  The Scale's value can be either 0 or
1. The Labels are positioned to the left and right of the Scale if its
orientation is horizontal, else on the top and bottom of the Scale.

=head1 OPTIONS

In addition to all Scale options, the following option/value pairs are
also supported:

=over 4

=item B<-llabel>

A reference to an array of left (or top) Label configuration options.

=item B<-rlabel>

A reference to an array of right (or bottom) Label configuration options.

=back

=head1 METHODS

There are no special methods.

=head1 ADVERTISED WIDGETS

Component subwidgets can be accessed via the B<Subwidget> method.
This mega widget has no advertised subwidgets.

=head1 EXAMPLE

See Synopsis.

=head1 BUGS

This widget uses only the pack geometry manager.

=head1 AUTHOR

sol0@Lehigh.EDU

Copyright (C) 2002 - 2003, Steve Lidie. All rights reserved.

This program is free software; you can redistribute it and/or
modify it under the same terms as Perl itself.

=head1 KEYWORDS

SlideSwitch, Scale

=cut
