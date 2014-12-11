package Tk::ColorDialog;
require Tk::Toplevel;
use base  qw(Tk::Toplevel);

use vars qw($VERSION);
$VERSION = '4.014';

Construct Tk::Widget 'ColorDialog';

sub Accept
{
 my $cw  = shift;
 $cw->withdraw;
 $cw->{'done'} = 1;
}

sub Cancel
{
 my $cw  = shift;
# $cw->configure(-color => undef);
 $cw->configure(-color => 'cancel');
 $cw->Accept;
}

sub Populate
{
 my ($cw,$args) = @_;
 $cw->SUPER::Populate($args);
 $cw->protocol('WM_DELETE_WINDOW' => [ 'Cancel' => $cw ]);
 $cw->transient($cw->Parent->toplevel);
 $cw->withdraw;
 my $sel = $cw->ColorSelect;
 my $accept = $cw->Button(-text => 'Accept', -command => ['Accept', $cw]);
 my $cancel = $cw->Button(-text => 'Cancel', -command => ['Cancel', $cw]);
 Tk::grid($sel);
 Tk::grid($accept,$cancel);
 $cw->ConfigSpecs(DEFAULT => [$sel]);
}

sub Show
{
 my $cw = shift;
 $cw->configure(@_) if @_;
 $cw->Popup();
 $cw->OnDestroy(sub { $cw->{'done'} = 0 }); # auto-cancel
 $cw->waitVariable(\$cw->{'done'});
 if (Tk::Exists($cw))
  {
   $cw->withdraw;
   $cw->cget('-color');
  }
 else
  {
   undef;
  }
}

1;
