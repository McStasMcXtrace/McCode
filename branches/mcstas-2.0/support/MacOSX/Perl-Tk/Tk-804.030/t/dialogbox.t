# -*- perl -*-
BEGIN { $|=1; $^W=1; }
use strict;

BEGIN {
    if (!eval q{
	use Test::More;
	1;
    }) {
	print "1..0 # skip: no Test::More module\n";
	exit;
    }
}

my $interactive_tests = 4;
plan tests => 8 + $interactive_tests;

if (!defined $ENV{BATCH}) { $ENV{BATCH} = 1 }

use_ok("Tk");
use_ok("Tk::DialogBox");

my $top = new MainWindow;
$top->Message(-font => "Helvetica 24",
	      -text => "This is the Main Window")->pack;
$top->withdraw unless $^O eq 'MSWin32';
eval { $top->geometry('+10+10'); };  # This works for mwm and interactivePlacement

{
    my $d = $top->DialogBox;
    $d->add("Label",
	    -text => "A dialog box with some widgets." . 
	    (!$ENV{BATCH} ? "\nClick OK to continue." : ""),
	   )->pack;
    my $e = $d->add("Entry")->pack;
    $d->configure(-focus => $e,
		  -showcommand => sub {
		      my $w = shift;
		      is($w, $d, "Callback parameter check");
		      $d->update;
		      my $fc = $d->focusCurrent || "";
		      ok($fc eq "" || $fc eq $e,
			 "Check -focus option (current focus is on `$fc')");
		      my $ok_b = $d->Subwidget("B_OK");
		      ok(Tk::Exists($ok_b), "Check default button");
		      isa_ok($ok_b, "Tk::Button");
		      $ok_b->after(300, sub { $ok_b->invoke }) if $ENV{BATCH};
		  });
    is($d->Show, "OK", "Expected result");
}

{
    my $d = $top->DialogBox(-buttons => [qw(OK Cancel), "I don't know"],
			    -default_button => "Cancel");
    my $e = $d->add("Label", -text => "Hello, world!\nPlease click the default button (Cancel)")->pack;
    $d->configure(-showcommand => sub {
		      $d->update;
		      my $d_b = $d->{default_button};
		      $d->after(300, sub { $d_b->invoke }) if $ENV{BATCH};
		  });
    is($d->Show, "Cancel", "Expected default button result");
}

SKIP: {
    skip("Needs non-BATCH mode (env BATCH=0 $^X -Mblib t/dialogbox.t)", $interactive_tests)
	if $ENV{BATCH};

    my $close_text = "Please close the dialog by using the window manager's close button";

    {
	my $d = $top->DialogBox(-buttons => [qw(OK)]);
	my $e = $d->add("Label", -text => $close_text)->pack;
	is($d->Show, "OK", "One button dialog - only button is cancel_button");
    }

    {
	my $d = $top->DialogBox(-buttons => [qw(OK Cancel)], -cancel_button => 'Cancel');
	my $e = $d->add("Label", -text => $close_text)->pack;
	is($d->Show, "Cancel", "Explicite cancel button");
    }    

    {
	my $d = $top->DialogBox(-buttons => [qw(OK Cancel)]);
	my $e = $d->add("Label", -text => $close_text)->pack;
	is($d->Show, undef, "No implicite cancel button with more than one buttons");
    }

    # This should be the last test
    {
	$top->raise;
	$top->deiconify;
	my $d = $top->DialogBox(-buttons => [qw(Ok Cancel)]);
	my $e = $d->add("Message", -text => "Please close the ***Main window*** using the window manager's close button. The test program must not hang.")->pack;
	ok(!$d->Show, "No hanging program after destroying main window");
    }
}

1;
__END__
