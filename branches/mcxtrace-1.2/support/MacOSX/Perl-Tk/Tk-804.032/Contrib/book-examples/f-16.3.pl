#!/usr/local/bin/perl -w

#
# Figure 16.3, p162
#


# how to do this ?

use Tk;

$mw = MainWindow->new;
$country = "Japan";
&watch($country);
$country = "Great Britain";

sub watch {
  my ($name) = @_;
  my $watch = $mw->Toplevel;
  $watch->Label(-text => 'Value of $name: ')->pack (-side => 'left');
  $watch->Label(-textvariable => $name)->pack(-side => 'left');
#  $watch->update;
}

MainLoop;
