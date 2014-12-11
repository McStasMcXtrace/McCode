package Tk::DragDrop::XDNDSite;
use strict;
use vars qw($VERSION);
$VERSION = '4.007'; # sprintf '4.%03d', q$Revision: #6 $ =~ /\D(\d+)\s*$/;
use base qw(Tk::DropSite);

sub XDND_PROTOCOL_VERSION () { 4 }

Tk::DropSite->Type('XDND');

sub InitSite
{my ($class,$site) = @_;
 my $w = $site->widget;
}

sub XdndEnter
{
 my ($t,$sites) = @_;
 my $event = $t->XEvent;
 my ($src,$flags,@types) = unpack('LLLLL',$event->A);
 my $ver = ($flags >> 24) & 0xFF;
 if ($flags & 1)
  {
   my @prop;
   Tk::catch { @prop = $t->property('get','XdndTypeList',$src) };
   @types = @prop if (!$@ && shift(@prop) eq 'ATOM');
  }
 else
  {
   $t->MakeAtom(@types);
  }
 # print "XdndEnter $src $ver @types\n";
 $t->{"XDND$src"} = { ver => $ver, types => \@types };
}

sub XdndLeave
{
 my ($t,$sites) = @_;
 my $event = $t->XEvent;
 my ($src,$flags,@types) = unpack('LLLLL',$event->A);
 # print "XdndLeave $src\n";
 my $info = $t->{"XDND$src"};
 if ($info)
  {
   my $over = $info->{site};
   if ($over)
    {
     my $X = $info->{X};
     my $Y = $info->{Y};
     $over->Apply(-entercommand => $X, $Y, 0)
    }
  }
 delete $t->{"XDND$src"};
}

sub XdndPosition
{
 my ($t,$sites) = @_;
 my $event = $t->XEvent;
 my ($src,$flags,$xy,$time,$action) = unpack('LLLLL',$event->A);
 $t->MakeAtom($action);
 my $X = $xy >> 16;
 my $Y = $xy & 0xFFFF;
 my $info = $t->{"XDND$src"};
 $info->{X}      = $X;
 $info->{Y}      = $Y;
 $info->{action} = $action;
 $info->{t}      = $time;
 my ($id)  = $t->wrapper;
 my $sxy   = 0;
 my $swh   = 0;
 my $sflags = 0;
 my $saction = 0;
 my $over = $info->{site};
 foreach my $site (@$sites)
  {
   if ($site->Over($X,$Y))
    {
     $sxy = ($site->X << 16)     | $site->Y;
     $swh = ($site->width << 16) | $site->height;
     $saction = $action;
     $sflags |= 1;
     if ($over)
      {
       if ($over == $site)
        {
         $site->Apply(-motioncommand => $X, $Y);
        }
       else
        {
         $over->Apply(-entercommand => $X, $Y, 0);
         $site->Apply(-entercommand => $X, $Y, 1);
        }
      }
     else
      {
       $site->Apply(-entercommand => $X, $Y, 1);
      }
     $info->{site} = $site;
     last;
    }
  }
 unless ($sflags & 1)
  {
   if ($over)
    {
     $over->Apply(-entercommand => $X, $Y, 0)
    }
   delete $info->{site};
  }
 my $data = pack('LLLLL',$id,$sflags,$sxy,$swh,$action);
 $t->SendClientMessage('XdndStatus',$src,32,$data);
}

sub XdndDrop
{
 my ($t,$sites) = @_;
 my $event = $t->XEvent;
 my ($src,$flags,$time,$res1,$res2) = unpack('LLLLL',$event->A);
 my $info   = $t->{"XDND$src"};
 my $sflags = 0;
 my $action = 0;
 if ($info)
  {
   $info->{t} = $time;
   my $site = $info->{'site'};
   if ($site)
    {
     my $X = $info->{'X'};
     my $Y = $info->{'Y'};
     $action = $info->{action};
     $site->Apply(-dropcommand => $X, $Y, 'XdndSelection',$action,$info->{types});
     $site->Apply(-entercommand => $X, $Y, 0);
    }
  }
 my ($id) = $t->wrapper;
 my $data  = pack('LLLLL',$id,$sflags,$action,0,0);
 $t->SendClientMessage('XdndFinished',$src,32,$data);
}

sub NoteSites
{my ($class,$t,$sites) = @_;
 my ($wrapper) = $t->wrapper;
 if (@$sites)
  {
   $t->BindClientMessage('XdndLeave',[\&XdndLeave,$sites]);
   $t->BindClientMessage('XdndEnter',[\&XdndEnter,$sites]);
   $t->BindClientMessage('XdndPosition',[\&XdndPosition,$sites]);
   $t->BindClientMessage('XdndDrop',[\&XdndDrop,$sites]);
   $t->property('set','XdndAware','ATOM',32,[XDND_PROTOCOL_VERSION],$wrapper);
  }
 else
  {
   $t->property('delete','XdndAware',$wrapper);
  }
}

1;
__END__
