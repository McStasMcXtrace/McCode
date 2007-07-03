#!/usr/bin/perl

use strict;
use blib;
use Tk;
require Tk::MainWindow;
require Tk::HList;
require Tk::CodeText;

my $main = new MainWindow;
my $ed;
my $pl = $main->Scrolled('HList',
	-scrollbars => 'osoe',
	-browsecmd => sub {
		my $stx = shift;
		$ed->configure(-syntax => $stx);
		$ed->Load("samples/$stx.test");
	},
)->pack(
	-side => 'left', 
	-fill => 'y'
);
$ed = $main->Scrolled('CodeText',
	-wrap => 'none',
	-syntax => 'Bash',
	-scrollbars => 'se',
)->pack(
	-side => 'left',
	-expand => 1,
	-fill => 'both',
);

my @plugs = $ed->highlightPlugList;
foreach my $p (@plugs) {
	$pl->add($p,
		-text => $p,
	);
}
$main->configure(-menu => $ed->menu);
$main->MainLoop;
