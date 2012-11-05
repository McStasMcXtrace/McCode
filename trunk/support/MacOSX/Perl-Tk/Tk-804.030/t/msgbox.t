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

plan tests => 17;

use_ok('Tk::MsgBox');

my $mw = MainWindow->new;
$mw->geometry("+10+10");

my @opts = (-icon    => "info",
	    -detail  => "Some details...",
	    -message => "A message",
	    -type    => "ok",
	    -title   => "Dialog title",
	   );

for my $icon (qw(info warning error question)) {
    my $w = $mw->MsgBox(@opts, -icon => $icon);
    ok(Tk::Exists($w), "Setting -icon to $icon");
    is($w->state, "withdrawn", "Initially invisible");
    $w->after(100, sub { $w->destroy });
    $w->Show;
}

for my $type (qw(abortretryignore ok okcancel retrycancel yesno yesnocancel)) {
    my $w = $mw->MsgBox(@opts, -type => $type);
    ok(Tk::Exists($w), "Setting -type to $type");
    $w->after(100, sub { $w->destroy });
    $w->Show;
}

{
    my $w = $mw->MsgBox(-type => "okcancel");
    ChooseMsg($w,'ok');
    is($w->Show, 'ok', "Emulating mouse click to ok");
    ChooseMsg($w,'cancel');
    is($w->Show, 'cancel', "Emulating mouse click to cancel");
}

if (0) { # XXX this probably only works after the grab stuff...
    my $w = $mw->MsgBox(-type => "okcancel");
    ChooseMsgByKey($w,'ok');
    is($w->Show, 'ok', "Emulating key press to ok");
    ChooseMsgByKey($w,'cancel');
    is($w->Show, 'cancel', "Emulating key press to cancel");
}

sub ChooseMsg {
    my($w,$btn) = @_;
    $w->after(100, sub {SendEventToMsg($w,$btn,'mouse')});
}

sub ChooseMsgByKey {
    my($w,$btn) = @_;
    $w->after(100, sub {SendEventToMsg($w,$btn,'key')});
}

sub PressButton {
    my($b) = @_;
    $b->eventGenerate('<Enter>');
    $b->eventGenerate('<ButtonPress-1>', '-x' => 5, '-y' => 5);
    $b->eventGenerate('<ButtonRelease-1>', '-x' => 5, '-y' => 5);
}

sub SendEventToMsg {
    my($w, $btn, $type) = @_;
    my $b = $w->Subwidget($btn);
    if (!$b->ismapped) {
	$b->update;
    }
    if ($type eq 'mouse') {
	PressButton($b);
    } else {
	$w->eventGenerate('<Enter>');
	$w->focus;
	$b->eventGenerate('<Enter>');
	$w->eventGenerate('<KeyPress>', -keysym => 'Return');
    }
}

__END__
