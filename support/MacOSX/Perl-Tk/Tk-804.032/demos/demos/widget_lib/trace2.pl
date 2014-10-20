# trace2.pl

$Tk::TraceText::VERSION = '1.0';

package Tk::TraceText;

use Tk::widgets qw/ Trace /;
use base qw/ Tk::Derived Tk::Text /;
use strict;

Construct Tk::Widget 'TraceText';

sub Populate {

    my( $self, $args ) = @_;

    $self->SUPER::Populate( $args );

    $self->ConfigSpecs(
        -textvariable => [ 'METHOD', 'textVariable', 'TextVariable', undef ],
    );

    $self->OnDestroy( sub {
	my $vref = $self->{_vref};
	$self->traceVdelete ( $vref ) if defined $vref;
    } );

} # end Populate

# Private methods.

sub insdel {

    my( $self, $sub, @args ) = @_;
    
    $self->{_busy} = 1;
    $self->$sub( @args );
    my $vref = $self->{_vref};
    $$vref = $self->get( qw/1.0 end/ );
    $self->{_busy} = 0;

} # end insedel

sub textvariable {

    my ( $self, $vref ) = @_;

    $self->traceVariable( $vref, 'w', [ \&tracew => $self, $vref ] );
    $self->{_vref} = $vref;
    
} # end textvariable

sub tracew {

    my ( $index, $value, $op, $self, $vref ) = @_;

    return unless defined $self;	# if app is being destroyed
    return if $self->{_busy};

    if ( $op eq 'w' ) {
	$self->delete( qw/1.0 end/ );
	$self->insert( '1.0', $value );
    } elsif ( $op eq 'r' ) {
    } elsif ( $op eq 'u' ) {
	$self->traceVdelete ( $vref );
    }

} # end tracew

# Overridden methods.

sub delete {

    my ( $self, @args ) = @_;

    $self->insdel( 'SUPER::delete', @args )

} # end delete

sub insert {

    my ( $self, @args ) = @_;

    $self->insdel( 'SUPER::insert', @args );

} # end insert

1;

package main;

use Tk::widgets qw/ Trace /;
use vars qw / $TOP /;
use strict;

sub trace2 {

    my( $demo ) = @_;

    $TOP = $MW->WidgetDemo(
        -name             => $demo,
        -text             => "This demonstration derives a new Text widget whose contents are modified using a normal Perl variable.",
        -title            => 'Contents of a Text widget tied to a variable',
        -iconname         => 'trace2',
    );

    my $mw = $TOP;
    my $tt = $mw->Scrolled( 'TraceText', -textvariable => \my $frog )->grid;
    $tt->focus;

    $mw->traceVariable( \$frog, 'wu', [ \&trace2_tracefrog, $mw, \$frog ] );

    $frog = "Frogs lacking lipophores are blue.";

} # end trace2

sub trace2_tracefrog {

    my( $index, $value, $op ) = @_;

    print "Final " if $op eq 'u';
    print "User trace: $value";
    return $value;

}

__END__

=head1 NAME

Tk::TraceText - Text contents defined by a traced variable.

=for pm Tk/TraceText.pm

=for category Text

=head1 SYNOPSIS

 $tt = $parent->TraceText(-option => value, ... );

=head1 DESCRIPTION

Create a new B<TraceText> widget that is derived from the standard B<Text>
widget. Because it inherits all the base options and methods it behaves
just like a B<Text> widget.  Additionally, B<TraceText> adds a -textvariable
option, which is a reference to a Perl scalar that defines the contents of
the widget.

Based on the Tcl/Tk TracedText "overridden widget" by Kevin Kenny.

=over 4

=item B<-textvariable>

A scalar reference.  The value of the variable defines the contents of the
TraceText widget.  Using the keyboard to insert or delete text changes the 
value of the variable, and changing the variable alters the contents of the
TraceText widget.

=back

=head1 METHODS

Standard Text widget methods.

=head1 ADVERTISED SUBWIDGETS

None.

=head1 EXAMPLE

 my $tt = $mw->TraceText( -textvariable => \$scalar );

=head1 AUTHOR

Stephen.O.Lidie@Lehigh.EDU

Copyright (C) 2003 - 2004, Steve Lidie. All rights reserved.

This program is free software; you can redistribute it
and/or modify it under the same terms as Perl itself.

=head1 KEYWORDS

text, trace

=cut

