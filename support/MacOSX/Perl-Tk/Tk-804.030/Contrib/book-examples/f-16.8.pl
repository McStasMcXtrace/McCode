#!/usr/local/bin/perl -w

#
# Figure 16.8, p166
#


use Tk;

my $mw = MainWindow->new;
my $times = $mw->Radiobutton(-text     => 'Times',
			     -variable => \$font,
			     -value    => 'times',
			     -anchor   => 'w',
			    );
my $helvetica = $mw->Radiobutton(-text     => 'Helvetica',
				 -variable => \$font,
				 -value    => 'helvetica',
				 -anchor   => 'w',
				);
my $courier = $mw->Radiobutton(-text     => 'Courier',
			       -variable => \$font,
			       -value    => 'courier',
			       -anchor   => 'w',
			      );
my $symbol = $mw->Radiobutton(-text     => 'Symbol',
			      -variable => \$font,
			      -value    => 'symbol',
			      -anchor   => 'w',
			     );
$times->pack(-side => 'top', -fill => 'x');
$helvetica->pack(-side => 'top', -fill => 'x');
$courier->pack(-side => 'top', -fill => 'x');
$symbol->pack(-side => 'top', -fill => 'x');

MainLoop;
