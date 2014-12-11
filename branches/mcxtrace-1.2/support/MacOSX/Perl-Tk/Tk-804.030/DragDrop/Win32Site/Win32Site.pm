package Tk::DragDrop::Win32Site;

use vars qw($VERSION);
$VERSION = '4.009'; # sprintf '4.%03d', q$Revision: #8 $ =~ /\D(\d+)\s*$/;

use Tk qw($XS_VERSION);
require DynaLoader;
require Tk::DropSite;

use base qw(Tk::DropSite DynaLoader);

bootstrap Tk::DragDrop::Win32Site;

use strict;

Tk::DropSite->Type('Win32');

sub WM_DROPFILES () {563}

sub InitSite
{
 my ($class,$site) = @_;
 my $w = $site->widget;
 $w->BindClientMessage(WM_DROPFILES,[\&Win32Drop,$site]);
 DragAcceptFiles($w,1);
}

sub Win32Drop
{
 # print join(',',@_),"\n";
 my ($w,$site,$msg,$wParam,$lParam) = @_;
 my ($x,$y,@files) = DropInfo($wParam);
 my $cb = $site->{'-dropcommand'};
 $site->Apply(-entercommand => $x, $y, 1);
 if ($cb)
  {
   foreach my $file (@files)
    {
     # print "$file @ $x,$y\n";
     $w->clipboardClear;
     $w->clipboardAppend('--',$file);
     $cb->Call('CLIPBOARD',Win32Drop => ['STRING'],$x,$y);
    }
  }
 $site->Apply(-entercommand => $x, $y, 0);
 return 0;
}

1;
__END__
