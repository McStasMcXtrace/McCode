#!/usr/local/bin/perl -w

#
# Figure 16.7, p166
#

# not -w clean (how to do ?)

use Tk;

my $mw = MainWindow->new;
my $bold = $mw->Checkbutton(-text     => 'Bold',
			    -variable => $bold,
			    -anchor   => 'w',
			   );
my $italic = $mw->Checkbutton(-text     => 'Italic',
			      -variable => $italic,
			      -anchor   => 'w',
			     );
my $underline = $mw->Checkbutton(-text     => 'Underline',
				 -variable => $underline,
				 -anchor   => 'w',
				);
$bold->pack(-side => 'top', -fill => 'x');
$italic->pack(-side => 'top', -fill => 'x');
$underline->pack(-side => 'top', -fill => 'x');

MainLoop;
