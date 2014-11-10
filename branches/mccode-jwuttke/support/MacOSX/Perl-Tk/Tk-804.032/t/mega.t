# -*- perl -*-
BEGIN { $|=1; $^W=1; }

use strict;
use Test;

BEGIN { plan tests => 8 };

use Tk;
use Tk::Widget;
use Tk::Derived;
use Tk::Frame;
use Tk::Button;


my $mw = Tk::MainWindow->new;
eval { $mw->geometry('+10+10'); };  # This works for mwm and interactivePlacement

##
## Tests Component and therefore Subwidget, Delegate a bit
##
{
    print "testing Component() method\n";

    eval <<'EOTEST';

	package Tk::tests::Composite;
        use vars '@ISA';
	@ISA=qw/Tk::Derived Tk::Frame/;
	Construct Tk::Widget 'testComponent';
	sub Populate
	  {
	    my ($cw,$args) = @_;
	    $cw->SUPER::Populate($args);
	    my $b1 = $cw->Component('Button'=>'b1', -delegate=>["invoke"]);
	    $b1->pack;
            $cw;
          }
        package main;
EOTEST
    ok ($@, '', "Can't define widget to test Component()");
    eval { $mw->update; };
    ok ($@, '', "Idletask problem of define widget to test Component()");

    my $test;
    eval { $test = $mw->testComponent; };
    ok ($@, '', "Can't create testCompoment widget");
    eval { $mw->update; };
    ok ($@, '', "Idletask problem after creation of  testComponent");

    my $subw;
    eval { $subw = $test->Subwidget('b1'); };
    ok ($@, '', "Ooops, problem with Subwidget method");
    # This relies on the fact that testComponent has only one child
    my $child = ($test->children)[0];
    ok($subw, $child, "Ooops, Advertise problem in Component();");

    #clean up
    eval { $test->destroy; };
    ok($@, '', "problem destroying testComponent widget");
    eval { $mw->update; };
    ok($@, '', "Idletask problem after destroy of testComponent");
}

1;
__END__

