# This demonstration script prompts the user to select a directory.

use vars qw/$TOP/;

sub choosedir {
    my $demo = shift;

    $TOP = $MW->WidgetDemo
      (
       -name     => $demo,
       -text     => "Enter a directory name in the entry box or click on the \"Browse\" buttons to select a directory name using the directory selection dialog.",
       -title    => 'Choose Directory Demonstration',
       -iconname => 'choosedir',
      );
    {
	my $f = $TOP->Frame;
	my $lab = $f->Label(-text => "Select a directory to open: ",
			    -anchor => 'e');
	my $ent = $f->Entry(-width => 20);
	my $but = $f->Button(-text => "Browse ...",
			     -command => sub { dirDialog($TOP, $ent)});
	$lab->pack(-side => 'left');
	$ent->pack(-side => 'left',-expand => 'yes', -fill => 'x');
	$but->pack(-side => 'left');
	$f->pack(-fill => 'x', -padx => '1c', -pady => 3);
    }
}

sub dirDialog {
    my $w = shift;
    my $ent = shift;
    my $dir;
    $dir = $w->chooseDirectory;
    if (defined $dir and $dir ne '') {
	$ent->delete(0, 'end');
	$ent->insert(0, $dir);
	$ent->xview('end');
    }
}
