#!/usr/bin/perl -w
# -*- perl -*-

#
# Author: Slaven Rezic
#

use strict;

use Tk;
use Tk::BrowseEntry;

BEGIN {
    if (!eval q{
	use Test::More;
	1;
    }) {
	print "1..0 # skip: no Test::More module\n";
	exit;
    }
}

BEGIN { plan tests => 3 }

if (!defined $ENV{BATCH}) { $ENV{BATCH} = 1 }

{
    package Tk::SpinboxBrowseEntry;
    use base qw(Tk::BrowseEntry);
    use Tk::Spinbox;
    Construct Tk::Widget 'SpinboxBrowseEntry';
    sub LabEntryWidget { "Spinbox" }
}

my $mw = my $top = tkinit;
$mw->geometry("+10+10");
my $ne = $mw->SpinboxBrowseEntry(-from => -10,
			     -to => +10,
			     -choices => [-6,-3,0,3,6],
			    )->pack;
isa_ok $ne, 'Tk::SpinboxBrowseEntry';


{
    package Tk::MyLabEntry;
    use base qw(Tk::Frame);
    Construct Tk::Widget 'MyLabEntry';

    sub Populate {
	my($cw, $args) = @_;
	$cw->SUPER::Populate($args);
	my $e = $cw->Component(Entry => 'entry');
	$e->pack('-expand' => 1, '-fill' => 'both');
	$cw->ConfigSpecs(DEFAULT => [$e]);
	$cw->Delegates(DEFAULT => $e);
	$cw->AddScrollbars($e) if (exists $args->{-scrollbars});
	$cw->ConfigSpecs(-background => ['SELF', 'DESCENDANTS'],
			 DEFAULT => [$e],);
    }
}

{
    package Tk::MyLabEntryBrowseEntry;
    use base qw(Tk::BrowseEntry);
    Construct Tk::Widget 'MyLabEntryBrowseEntry';
    sub LabEntryWidget { "MyLabEntry" }
}

$mw->optionAdd("*MyLabEntryBrowseEntry*Entry.background", "red");
my $le = $mw->MyLabEntryBrowseEntry(-label => "My LabEntry:")->pack;
isa_ok $le, 'Tk::MyLabEntryBrowseEntry';
is $le->Subwidget('entry')->Subwidget('entry')->cget('-background'), 'red', 'option db value for subclass';

$top->Button(-text => "Ok",
	     -command => sub {
		$top->destroy;
	    })->pack;
$top->after(60*1000, sub { $top->destroy });

if (!$ENV{BATCH}) {
    MainLoop;
}
else {
  $mw->update;
  $top->after(500);
}

__END__
