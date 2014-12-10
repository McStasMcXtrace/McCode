#!/usr/bin/perl -w
# -*- perl -*-

use strict;

use Tk;

BEGIN {
    if (!eval q{
	use Test::More;
	1;
    }) {
	print "1..0 # skip: no Test::More module\n";
	exit;
    }
}

BEGIN { plan tests => 5 }

my $mw = MainWindow->new;
$mw->geometry("+10+10");

{
    my $cb = $mw->Checkbutton->pack;
    is(ref $cb, "Tk::Checkbutton", "It's a checkbutton");
    is($cb->{Value}, undef, "No value at beginning");
    $cb->select;
    is($cb->{Value}, 1, "... but now");
}

{
    # new Button options
    my $f = $mw->Frame->pack(-fill => 'x');
    my $incr = 0;
    $f->Button(-text => "Repeat & ridge",
	       -image => $mw->Photo(-file => Tk->findINC("Xcamel.gif")),
	       -compound => 'left',
	       -overrelief => 'ridge',
	       -repeatdelay => 200,
	       -repeatinterval => 100,
	       -command => sub { $incr++ },
	      )->pack(-side => 'left');
    $f->Label(-text => "increments:")->pack(-side => 'left');
    $f->Label(-textvariable => \$incr)->pack(-side => 'left');
    pass("Button with new options");
}

{
    # Label and -state
    my $f = $mw->Frame->pack(-fill => 'x');
    $f->Label(-state => "normal", -text => "normal: red on white",
	      -background => 'white', -foreground => 'red')->pack(-fill => 'x');
    $f->Label(-state => "active", -text => "active: green on white",
	      -activebackground => 'white', -activeforeground => 'green')->pack(-fill => 'x');
    $f->Label(-state => "disabled", -text => "disabled: blue on white",
	      -background => 'white', -disabledforeground => 'blue')->pack(-fill => 'x');
    pass("Label with states");
}

if ($ENV{PERL_INTERACTIVE_TEST}) {
    MainLoop;
}

__END__
