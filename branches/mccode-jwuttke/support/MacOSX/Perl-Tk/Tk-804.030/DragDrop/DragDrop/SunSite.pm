package Tk::DragDrop::SunSite;
require Tk::DropSite;

use vars qw($VERSION);
$VERSION = '4.007'; # sprintf '4.%03d', q$Revision: #6 $ =~ /\D(\d+)\s*$/;

use Tk::DragDrop::SunConst;
use base  qw(Tk::DropSite);
use strict;

Tk::DropSite->Type('Sun');

sub SunDrop
{
 my ($w,$site) = @_;
 my $e = $w->XEvent;
 my ($seln,$t,$x,$y,$id,$flags) = unpack('LLSSLL',$e->A);
 $w->MakeAtom($seln);
 if ($flags & &ACK_FLAG)
  {
   Tk::catch { $w->SelectionGet('-selection'=>$seln,'_SUN_DRAGDROP_ACK') };
  }
 my @targ = $w->SelectionGet(-selection => $seln,'TARGETS');
 $site->Apply(-dropcommand => $x, $y, $seln, SunDrop => \@targ);
 if ($flags & &TRANSIENT_FLAG)
  {
   Tk::catch { $w->SelectionGet('-selection'=>$seln,'_SUN_DRAGDROP_DONE') };
  }
 $w->configure('-relief' => $w->{'_DND_RELIEF_'}) if (defined $w->{'_DND_RELIEF_'});
 $site->Apply(-entercommand => $x, $y, 0);
}

sub SunPreview
{
 my ($w,$site) = @_;
 my $event = $w->XEvent;
 my ($kind,$t,$x,$y,$id,$flags) = unpack('LLSSLL',$event->A);
 $x -= $site->X;
 $y -= $site->Y;
 if ($kind == _enter)
  {
   $site->Callback(-entercommand => 1, $x, $y);
  }
 elsif ($kind == _leave)
  {
   $site->Callback(-entercommand => 0, $x, $y);
  }
 elsif ($kind == _motion)
  {
   $site->Callback(-motioncommand => $x, $y);
  }
}

sub InitSite
{
 my ($class,$site) = @_;
 my $w = $site->widget;
 $w->BindClientMessage('_SUN_DRAGDROP_TRIGGER',[\&SunDrop,$site]);
 $w->BindClientMessage('_SUN_DRAGDROP_PREVIEW',[\&SunPreview,$site]);
}

sub NoteSites
{
 my ($class,$t,$sites) = @_;
 my $count = @$sites;
 my @data  = (0,0);
 my ($wrapper,$offset) = $t->wrapper;
 if ($t->viewable)
  {
   my $s;
   my $i = 0;
   my @win;
   my $bx = $t->rootx;
   my $by = $t->rooty - $offset;
   $t->MakeWindowExist;
   foreach $s (@$sites)
    {
     my $w = $s->widget;
     if ($w->viewable)
      {
       $w->MakeWindowExist;
       $data[1]++;
       push(@data,${$w->WindowId});                   # XID
       push(@data,$i++);                              # Our 'tag'
       push(@data,ENTERLEAVE|MOTION);                 # Flags
       push(@data,0);                                 # Kind is 'rect'
       push(@data,1);                                 # Number of rects
       push(@data,$s->X-$bx,$s->Y-$by,$s->width,$s->height);  # The rect
      }
    }
  }
 if ($data[1])
  {
   $t->property('set',
                '_SUN_DRAGDROP_INTEREST',           # name
                '_SUN_DRAGDROP_INTEREST',           # type
                32,                                 # format
                \@data,$wrapper);                   # the data
  }
 else
  {
   $t->property('delete','_SUN_DRAGDROP_INTEREST',$wrapper);
  }
}


1;
