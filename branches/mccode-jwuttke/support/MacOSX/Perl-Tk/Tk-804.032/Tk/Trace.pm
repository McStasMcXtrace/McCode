package Tk::Trace;

use vars qw($VERSION);
$VERSION = '4.009'; # was: sprintf '4.%03d', q$Revision: #7 $ =~ /\D(\d+)\s*$/;

use Carp;
use Tie::Watch;
use strict;

# The %TRACE hash is indexed by stringified variable reference. Each hash
# bucket contains an array reference having two elements:
#
# ->[0] = a reference to the variable's Tie::Watch object
# ->[1] = a hash reference with these keys: -fetch, -store, -destroy
#         ->{key} = [ active flag, [ callback list ] ]
#         where each callback is a normalized callback array reference
#
# Thus, each trace type (r w u ) may have multiple traces.

my %TRACE;                      # watchpoints indexed by stringified ref

my %OP = (			# trace to Tie::Watch operation map
    r => '-fetch',
    w => '-store',
    u => '-destroy',
);

sub fetch {

    # fetch() wraps the user's callback with necessary tie() bookkeeping
    # and invokes the callback with the proper arguments. It expects:
    #
    # $_[0] = Tie::Watch object
    # $_[1] = undef for a scalar, an index/key for an array/hash
    #
    # The user's callback is passed these arguments:
    #
    #   $_[0]        = undef for a scalar, index/key for array/hash
    #   $_[1]        = current value
    #   $_[2]        = operation 'r'
    #   $_[3 .. $#_] = optional user callback arguments
    #
    # The user callback returns the final value to assign the variable.

    my $self = shift;                          # Tie::Watch object
    my $val  = $self->Fetch(@_);               # get variable's current value
    my $aref = $self->Args('-fetch');          # argument reference
    my $call = $TRACE{$aref->[0]}->[1]->{-fetch}; # active flag/callbacks
    return $val unless $call->[0];             # if fetch inactive

    my $final_val;
    foreach my $aref (reverse  @$call[ 1 .. $#{$call} ] ) {
        my ( @args_copy ) = @$aref;
        my $sub = shift @args_copy;            # user's callback
        unshift @_, undef if scalar @_ == 0;   # undef "index" for a scalar
        my @args = @_;                         # save for post-callback work
        $args[1] = &$sub(@_, $val, 'r', @args_copy); # invoke user callback
        shift @args unless defined $args[0];   # drop scalar "index"
        $final_val = $self->Store(@args);      # update variable's value
    }
    $final_val;

} # end fetch

sub store {

    # store() wraps the user's callback with necessary tie() bookkeeping
    # and invokes the callback with the proper arguments. It expects:
    #
    # $_[0] = Tie::Watch object
    # $_[1] = new value for a scalar, index/key for an array/hash
    # $_[2] = undef for a scalar, new value for an array/hash
    #
    # The user's callback is passed these arguments:
    #
    #   $_[0]        = undef for a scalar, index/key for array/hash
    #   $_[1]        = new value
    #   $_[2]        = operation 'w'
    #   $_[3 .. $#_] = optional user callback arguments
    #
    # The user callback returns the final value to assign the variable.

    my $self = shift;                          # Tie::Watch object
    my $val  = $self->Store(@_);               # store variable's new value
    my $aref = $self->Args('-store');          # argument reference
    my $call = $TRACE{$aref->[0]}->[1]->{-store}; # active flag/callbacks
    return $val unless $call->[0];             # if store inactive

    foreach my $aref ( reverse @$call[ 1 .. $#{$call} ] ) {
        my ( @args_copy ) = @$aref;
        my $sub = shift @args_copy;            # user's callback
        unshift @_, undef if scalar @_ == 1;   # undef "index" for a scalar
        my @args = @_;                         # save for post-callback work
        $args[1] = &$sub(@_, 'w', @args_copy); # invoke user callback
        shift @args unless defined $args[0];   # drop scalar "index"
        $self->Store(@args);                   # update variable's value
    }

} # end store

sub destroy {

    # destroy() wraps the user's callback with necessary tie() bookkeeping
    # and invokes the callback with the proper arguments. It expects:
    #
    # $_[0] = Tie::Watch object
    #
    # The user's callback is passed these arguments:
    #
    #   $_[0]        = undef for a scalar, index/key for array/hash
    #   $_[1]        = final value
    #   $_[2]        = operation 'u'
    #   $_[3 .. $#_] = optional user callback arguments

    my $self = shift;                          # Tie::Watch object
    my $val  = $self->Fetch(@_);               # variable's final value
    my $aref = $self->Args('-destroy');        # argument reference
    my $call = $TRACE{$aref->[0]}->[1]->{-destroy}; # active flag/callbacks
    return $val unless $call->[0];             # if destroy inactive

    foreach my $aref ( reverse @$call[ 1 .. $#{$call} ] ) {
        my ( @args_copy ) = @$aref;
        my $sub = shift @args_copy;            # user's callback
        my $val = $self->Fetch(@_);            # get final value
        &$sub(undef, $val, 'u', @args_copy);   # invoke user callback
        $self->Destroy(@_);                    # destroy variable
    }

} # end destroy

sub Tk::Widget::traceVariable {

    my( $parent, $vref, $op, $callback ) = @_;

    {
	$^W = 0;
	croak "Illegal parent '$parent', not a widget" unless ref $parent;
	croak "Illegal variable '$vref', not a reference" unless ref $vref;
	croak "Illegal trace operation '$op'" unless $op;
	croak "Illegal trace operation '$op'" if $op =~ /[^rwu]/;
	croak "Illegal callback ($callback)" unless $callback;
    }

    # Need to add our internal callback to user's callback arg list
    # so we can call ours first, followed by the user's callback and
    # any user arguments. Trace callbacks are activated as requied.

    my $trace = $TRACE{$vref};
    if ( not defined $trace ) {
        my $watch = Tie::Watch->new(
            -variable => $vref,
            -fetch    => [ \&fetch,   $vref ],
            -store    => [ \&store,   $vref ],
            -destroy  => [ \&destroy, $vref ],
        );
        $trace = $TRACE{$vref} =
            [$watch,
             {
                 -fetch   => [ 0 ],
                 -store   => [ 0 ],
                 -destroy => [ 0 ],
             }
            ];
    }

    $callback =  [ $callback ] if ref $callback eq 'CODE';

    foreach my $o (split '', $op) {
	push @{$trace->[1]->{$OP{$o}}}, $callback;
	$trace->[1]->{$OP{$o}}->[0] = 1; # activate
    }

    return $trace;		# for peeking

} # end traceVariable

sub Tk::Widget::traceVdelete {

    my ( $parent, $vref, $op_not_honored, $callabck_not_honored ) = @_;

    if ( defined $vref && defined $TRACE{$vref} && defined $TRACE{$vref}->[0] ) {
	$$vref = $TRACE{$vref}->[0]->Fetch;
	$TRACE{$vref}->[0]->Unwatch;
	delete $TRACE{$vref};
    }

} # end traceVdelete

sub Tk::Widget::traceVinfo {

    my ( $parent, $vref ) = @_;

    return ( defined $TRACE{$vref}->[0] ) ? $TRACE{$vref}->[0]->Info : undef;

} # end traceVinfo

=head1 NAME

Tk::Trace - emulate Tcl/Tk B<trace> functions.

=head1 SYNOPSIS

 use Tk::Trace

 $mw->traceVariable(\$v, 'wru' => [\&update_meter, $scale]);
 %vinfo = $mw->traceVinfo(\$v);
 print "Trace info  :\n  ", join("\n  ", @{$vinfo{-legible}}), "\n";
 $mw->traceVdelete(\$v);

=head1 DESCRIPTION

This class module emulates the Tcl/Tk B<trace> family of commands by
binding subroutines of your devising to Perl variables using simple
B<Tie::Watch> features.

Callback format is patterned after the Perl/Tk scheme: supply either a
code reference, or, supply an array reference and pass the callback
code reference in the first element of the array, followed by callback
arguments.

User callbacks are passed these arguments:

 $_[0]        = undef for a scalar, index/key for array/hash
 $_[1]        = variable's current (read), new (write), final (undef) value
 $_[2]        = operation (r, w, or u)
 $_[3 .. $#_] = optional user callback arguments

As a Trace user, you have an important responsibility when writing your
callback, since you control the final value assigned to the variable.
A typical callback might look like:

 sub callback {
    my($index, $value, $op, @args) = @_;
    return if $op eq 'u';
    # .... code which uses $value ...
    return $value;     # variable's final value
 }

Note that the callback's return value becomes the variable's final value,
for either read or write traces.

For write operations, the variable is updated with its new value before
the callback is invoked.

Multiple read, write and undef callbacks can be attached to a variable,
which are invoked in reverse order of creation.

=head1 METHODS

=over 4

=item $mw->traceVariable(varRef, op => callback);

B<varRef> is a reference to the scalar, array or hash variable you
wish to trace.  B<op> is the trace operation, and can be any combination
of B<r> for read, B<w> for write, and B<u> for undef.  B<callback> is a
standard Perl/Tk callback, and is invoked, depending upon the value of
B<op>, whenever the variable is read, written, or destroyed.

=item %vinfo = $mw->traceVinfo(varRef);

Returns a hash detailing the internals of the Trace object, with these
keys:

 %vinfo = (
     -variable =>  varRef
     -debug    =>  '0'
     -shadow   =>  '1'
     -value    =>  'HELLO SCALAR'
     -destroy  =>  callback
     -fetch    =>  callback
     -store    =>  callback
     -legible  =>  above data formatted as a list of string, for printing
 );

For array and hash Trace objects, the B<-value> key is replaced with a
B<-ptr> key which is a reference to the parallel array or hash.
Additionally, for an array or hash, there are key/value pairs for
all the variable specific callbacks.

=item $mw->traceVdelete(\$v);

Stop tracing the variable.

=back

=head1 EXAMPLES

 # Trace a Scale's variable and move a meter in unison.

 use Tk;
 use Tk::widgets qw/Trace/;

 $pi = 3.1415926;
 $mw = MainWindow->new;
 $c = $mw->Canvas( qw/-width 200 -height 110 -bd 2 -relief sunken/ )->grid;
 $c->createLine( qw/100 100 10 100 -tag meter -arrow last -width 5/ );
 $s = $mw->Scale( qw/-orient h -from 0 -to 100 -variable/ => \$v )->grid;
 $mw->Label( -text => 'Slide Me for 5 Seconds' )->grid;

 $mw->traceVariable( \$v, 'w' => [ \&update_meter, $s ] );

 $mw->after( 5000 => sub {
     print "Untrace time ...\n";
     %vinfo = $s->traceVinfo( \$v );
     print "Watch info  :\n  ", join("\n  ", @{$vinfo{-legible}}), "\n";
     $c->traceVdelete( \$v );
 });

 MainLoop;

 sub update_meter {
     my( $index, $value, $op, @args ) = @_;
     return if $op eq 'u';
     $min = $s->cget( -from );
     $max = $s->cget( -to );
     $pos = $value / abs( $max - $min );
     $x = 100.0 - 90.0 * ( cos( $pos * $pi ) );
     $y = 100.0 - 90.0 * ( sin( $pos * $pi ) );
     $c->coords( qw/meter 100 100/, $x, $y );
     return $value;
 }

 # Predictive text entry.

 use Tk;
 use Tk::widgets qw/ LabEntry Trace /;
 use strict;

 my @words =  qw/radio television telephone turntable microphone/;

 my $mw = MainWindow->new;

 my $e = $mw->LabEntry(
     qw/ -label Thing -width 40 /,
     -labelPack    => [ qw/ -side left / ],
     -textvariable => \my $thing,
 );
 my $t = $mw->Text( qw/ -height 10 -width 50 / );;

 $t->pack( $e, qw/ -side top / );

 $e->focus;
 $e->traceVariable( \$thing, 'w', [ \&trace_thing, $e, $t ] );

 foreach my $k ( 1 .. 12 ) {
     $e->bind( "<F${k}>" => [ \&ins, $t, Ev('K') ] );
 }
 $e->bind( '<Return>' =>
           sub {
               print "$thing\n";
               $_[0]->delete( 0, 'end' );
           }
 );

 MainLoop;

 sub trace_thing {

     my( $index, $value, $op, $e, $t ) = @_;

     return unless $value;

     $t->delete( qw/ 1.0 end / );
     foreach my $w ( @words ) {
         if ( $w =~ /^$value/ ) {
             $t->insert( 'end', "$w\n" );
         }
     }

     return $value;

 } # end trace_thing

 sub ins {

     my( $e, $t, $K ) = @_;

     my( $index ) = $K =~ /^F(\d+)$/;

     $e->delete( 0, 'end' );
     $e->insert( 'end', $t->get( "$index.0", "$index.0 lineend" ) );
     $t->delete( qw/ 1.0 end / );

 } # end ins

=head1 HISTORY

 Stephen.O.Lidie@Lehigh.EDU, Lehigh University Computing Center, 2000/08/01
 . Version 1.0, for Tk800.022.

 sol0@Lehigh.EDU, Lehigh University Computing Center, 2003/09/22
 . Version 1.1, for Tk804.025, add support for multiple traces of the same
   type on the same variable.

=head1 COPYRIGHT

Copyright (C) 2000 - 2003 Stephen O. Lidie. All rights reserved.

This program is free software; you can redistribute it and/or modify it under
the same terms as Perl itself.

=cut

1;
