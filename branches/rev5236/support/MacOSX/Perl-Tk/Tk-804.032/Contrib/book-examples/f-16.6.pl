#!/usr/local/bin/perl -w

#
# Figure 16.6, p165
#


use Tk;

my $mw = MainWindow->new;
$mw->Button(-text => 'OK', -command => \&ok)->pack(-side => 'left');
$mw->Button(-text => 'Apply', -command => \&apply)->pack(-side => 'left');
$mw->Button(-text => 'Cancel', -command => \&cancel)->pack(-side => 'left');
$mw->Button(-text => 'Help', -command => \&help)->pack(-side => 'left');

sub ok {
  print STDOUT "in OK\n";
}
sub apply {
  print STDOUT "in apply\n";
}
sub cancel {
  print STDOUT "in cancel\n";
}
sub help {
  print STDOUT "in help\n";
}

MainLoop;
