#!/usr/bin/perl -w
# -*- perl -*-

#
# $Id: $
# Author: Slaven Rezic
#

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

plan tests => 5;

use_ok 'Tk::ErrorDialog';

my $mw = tkinit;
$mw->geometry("+10+10");

my $errmsg = "Intentional error.";
$mw->afterIdle(sub { die "$errmsg\n" });

my $ed;
$mw->after(100, sub {
	       my $dialog = search_error_dialog($mw);
	       isa_ok($dialog, "Tk::Dialog", "dialog");
	       $ed = $dialog;
	       my $error_stacktrace_toplevel = search_error_stacktrace_toplevel($mw);
	       isa_ok($error_stacktrace_toplevel, 'Tk::ErrorDialog', 'Found stacktrace window');
	       is($error_stacktrace_toplevel->state, 'withdrawn', 'Stacktrace not visible');
	       $error_stacktrace_toplevel->geometry('+0+0'); # for WMs with interactive placement
	       $dialog->SelectButton('Stack trace');
	       second_error();
	   });

$mw->after(20*1000, sub {
	       if (Tk::Exists($mw)) {
		   $mw->destroy;
		   fail "Timeout - destroyed main window";
	       }
	   });
MainLoop;

sub second_error {
    $mw->afterIdle(sub { die "$errmsg\n" });
    $mw->after(100, sub {
		   my $dialog = search_error_dialog($mw);
		   is($ed, $dialog, "ErrorDialog reused");
		   $dialog->Exit;
		   $mw->after(100, sub { $mw->destroy });
	       });
}

sub search_error_dialog {
    my $w = shift;
    my $dialog;
    $w->Walk(sub {
		 return if $dialog;
		 for my $opt (qw(text message)) {
		     my $val = eval { $_[0]->cget("-$opt") };
		     if (defined $val && $val =~ m{\Q$errmsg}) {
			 $dialog = $_[0]->toplevel;
		     }
		 }
	     });
    $dialog;
}

sub search_error_stacktrace_toplevel {
    my $w = shift;
    my $toplevel;
    $w->Walk(sub {
		 return if $toplevel;
		 if ($_[0]->isa('Tk::ErrorDialog')) {
		     $toplevel = $_[0];
		 }
	     });
    $toplevel;
}

__END__
