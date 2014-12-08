#!/usr/local/bin/perl -w

#
# Figure 16.1, p159
#

use Tk;

my $mw = MainWindow->new;
for $relief('raised','sunken','flat','groove','ridge') {
  print "$relief";
  $$relief = $mw->Frame(
			-width       => '15m',
			-height      => '10m',
			-relief      => $relief,
			-borderwidth => 4,
		       );
  $$relief->pack(-side => 'left', -padx => '2m', -pady => '2m');
}
$flat->configure(-background => 'black');  #perl complains here

MainLoop;
