#!/usr/bin/perl -w
# -*- perl -*-

#
# $Id: browseentry2.t,v 1.9 2003/04/21 19:49:35 eserte Exp $
# Author: Slaven Rezic
#

use strict;

use Tk;
use Tk::BrowseEntry;

BEGIN {
    if (!eval q{
	use Test;
	1;
    }) {
	print "# tests only work with installed Test module\n";
	print "1..1\n";
	print "ok 1\n";
	exit;
    }
}

BEGIN { plan tests => 6 }

if (!defined $ENV{BATCH}) { $ENV{BATCH} = 1 }

my $top = new MainWindow;
$top->geometry("+10+10");
my $var;
my $robe = $top->BrowseEntry
    (
     -label      => "readonly, classic style",
     -state      => 'readonly',
     -autolistwidth   => 1,     # list width is dynamically calculated
     -autolimitheight => 1,     # limit height of browseentry to number
                                # of items
     -browsecmd  => sub { warn "-browsecmd:  @_\n" }, # old plain callback
     -browse2cmd => sub { warn "-browse2cmd: @_\n" }, # -browsecmd with index as argument
     -variable   => \$var,
    )->pack;

$robe->insert("end", @INC);

ok(ref $robe, 'Tk::BrowseEntry');
ok($robe->isa('Tk::BrowseEntry'), 1);

my $var2;
my $robe2 = $top->BrowseEntry
    (
     -label      => "normal, windows style",
     -autolistwidth   => 1,     # list width is dynamically calculated
     -autolimitheight => 1,     # limit height of browseentry to number
                                # of items
     -style => 'MSWin32',
     -variable   => \$var2,
    )->pack;
$robe2->insert("end", 1, 2, 3, "a very long entry exceeding the normal width");

ok(ref $robe2, 'Tk::BrowseEntry');
ok($robe2->isa('Tk::BrowseEntry'), 1);

{
    my $var;
    my $robe = $top->BrowseEntry
	(
	 -label      => "readonly, windows style",
	 -autolistwidth   => 1,     # list width is dynamically calculated
	 -autolimitheight => 1,     # limit height of browseentry to number
	                            # of items
	 -style => 'MSWin32',
	 -state => 'readonly',
	 -variable   => \$var,
	)->pack;
    $robe->insert("end", 1, 2, 3, "a very long entry exceeding the normal width");

    ok(ref $robe, 'Tk::BrowseEntry');
    ok($robe->isa('Tk::BrowseEntry'), 1);

    $robe->configure(-variable => \$var);
}

$top->Button(-text => "Ok",
	     -command => sub {
		$top->destroy;
	    })->pack;
$top->after(60*1000, sub { $top->destroy });

if (!$ENV{BATCH}) {
    MainLoop;
}

__END__
