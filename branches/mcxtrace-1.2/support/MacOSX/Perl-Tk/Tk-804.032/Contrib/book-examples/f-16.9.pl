#!/usr/local/bin/perl -w

#
# Figure 16.8, p166
#


use Tk;

my $mw = MainWindow->new;
my $msg = $mw->Message (
			-width => '8c', -justify => 'left',
			-relief => 'raised', -bd => 2,
			-font => '-Adobe-Helvetica-Bold-R-Normal--*-180-*-*-*-*-*-*',
			-text => 'You have made changes to this document since the last time it was saved. Is it OK to discard the changes?',
		       );
$msg->pack;

MainLoop;
