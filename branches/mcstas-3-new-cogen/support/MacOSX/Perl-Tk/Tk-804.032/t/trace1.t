BEGIN { $|=1; $^W=1; }
use Test;
use Tk;
use Tk::Trace;
use strict;

plan test => 18;

my $mw = MainWindow->new;
$mw->geometry("+10+10");
my $v = 0;

my $e = $mw->Entry(-textvariable => \$v)->pack;

my ($r, $w, $u ) = (0, 0, 0 );
$e->traceVariable( \$v, 'rwu', [ \&trace_v, $e, 123 ] );

foreach my $k ( 1 .. 5 ) {
    $mw->after(100);
    $v++;
    $mw->update;
}

$e->traceVdelete( \$v );
ok( $v, 5, 'traced variable != 5' );
ok( $r, 5, 'read trace not called 5 times' );
ok( $w, 6, 'write trace not called 5 times' );
ok( $u, 1, 'undef trace not called once' );

my $read_only = $v;
$e->traceVariable( \$v, 'w', sub { $read_only } );
$v = 777;
$e->traceVdelete( \$v );
ok( $v, 5, 'read-only variable failed != 5' );

ok( $v, 5, 'final value != 5' );

if ($Tk::VERSION < 804.027501) {
    warn "# This test segfaults in Tk804.027\n";
    ok(1);
} else {
    # Similar code is part of the CPAN module Tk::LCD
    my $c = $mw->Canvas->pack;
    my $foo;
    my $vref = \$foo;
    my $st = [sub {
        my ($index, $new_val, $op, $lcd) = @_;
        return unless $op eq 'w';
	# Problem: $c is not alive (or half-alive only) here
	# and internal data structures seem not to be valid
        $c->move("foo", 20,30);
        $new_val;
    }, $c];
    $c->traceVariable($vref, 'w' => $st);
    $c->{watch} = $vref;
    $c->createPolygon(10,10,20,10,20,20,10,20,-tags=>"foo");
    $c->OnDestroy( [sub {$_[0]->traceVdelete($_[0]->{watch})}, $c] );
    $c->update;
    $c->destroy;
    ok(1);
}

sub trace_v {

     my( $index, $value, $op, $ent, $num ) = @_;

     if ( $op eq 'r' ) {
	 ok( $e, $ent, 'arguments out of order' );
	 $r++;
     } elsif ( $op eq 'w' ) {
	 ok( $num, 123, '$num != 123' );
	 $w++;
     } elsif ( $op eq 'u' ) {
	 $u++;
     }
     
     $value;

 }

