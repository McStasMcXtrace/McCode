package Tk::DragDrop::SunDrop;
require  Tk::DragDrop::Rect;

use vars qw($VERSION);
$VERSION = '4.006'; # sprintf '4.%03d', q$Revision: #5 $ =~ /\D(\d+)\s*$/;

use base  qw(Tk::DragDrop::Rect);
use strict;
use Tk::DragDrop::SunConst;

Tk::DragDrop->Type('Sun');

BEGIN
 {
  # Define the Rect API as members of the array
  my @fields = qw(name win X Y width height flags ancestor widget);
  my $i = 0;
  no strict 'refs';
  for ($i=0; $i < @fields; $i++)
   {
    my $j    = $i;
    *{"$fields[$i]"} = sub { shift->[$j] };
   }
 }


sub Preview
{
 my ($site,$token,$e,$kind,$flags) = (@_);
 $token->BackTrace('No flags') unless defined $flags;
 my $sflags = $site->flags;
 return if ($kind == _motion && !($sflags & MOTION));
 return if ($kind != _motion && !($sflags & ENTERLEAVE));
 my $data = pack('LLSSLL',$kind,$e->t,$e->X,$e->Y,$site->name,$flags);
 $token->SendClientMessage('_SUN_DRAGDROP_PREVIEW',$site->win,32,$data);
}

sub Enter
{
 my ($site,$token,$e) = @_;
 $token->AcceptDrop;
 $site->Preview($token,$e,_enter,0);
}

sub Leave
{
 my ($site,$token,$e) = @_;
 $token->RejectDrop;
 $site->Preview($token,$e,_leave,0);
}

sub Motion
{
 my ($site,$token,$e) = @_;
 $site->Preview($token,$e,_motion,0);
}

sub HandleDone
{
 my ($token,$seln,$offset,$max) = @_;
 $token->Done;
 return '';
}

sub HandleAck
{
 my ($w,$seln,$offset,$max) = @_;
 return '';
}

sub HandleItem
{
 my ($w,$seln,$offset,$max) = @_;
 return undef;
}

sub HandleCount
{
 my ($w,$seln,$offset,$max) = @_;
 return 1;
}

sub Drop
{
 my ($site,$token,$seln,$e) = @_;
 my $w  = $token->parent;
 $w->SelectionHandle('-selection'=>$seln,'-type'=>'_SUN_DRAGDROP_ACK',[\&HandleAck,$token,$seln]);
 $w->SelectionHandle('-selection'=>$seln,'-type'=>'_SUN_DRAGDROP_DONE',[\&HandleDone,$token,$seln]);
 my $atom  = $w->InternAtom($seln);
 my $flags = ACK_FLAG | TRANSIENT_FLAG;
 my $data  = pack('LLSSLL',$atom,$e->t,$e->X,$e->Y,$site->name,$flags);
 $w->SendClientMessage('_SUN_DRAGDROP_TRIGGER',$site->win,32,$data);
}

sub FindSite
{
 my ($class,$token,$X,$Y) = @_;
 $token->{'SunDD'} = [] unless exists $token->{'SunDD'};
 my $site = $class->SUPER::FindSite($token,$X,$Y);
 if (!defined $site)
  {
   my $id = $token->PointToWindow($X,$Y);
   while ($id)
    {
     my @prop;
     Tk::catch { @prop = $token->property('get','_SUN_DRAGDROP_INTEREST', $id) };
     if (!$@ && shift(@prop) eq '_SUN_DRAGDROP_INTEREST' && shift(@prop) == 0)
      {
       # This is a "toplevel" which has some sites associated with it.
       my ($bx,$by) = $token->WindowXY($id);
       $token->{'SunDDSeen'} = {} unless exists $token->{'SunDDSeen'};
       return $site if $token->{'SunDDSeen'}{$id};
       $token->{'SunDDSeen'}{$id} = 1;
       my $sites = $token->{'SunDD'};
       my $count = shift(@prop);
       while (@prop && $count-- > 0)
        {
         my ($xid,$sn,$flags,$kind,$n) = splice(@prop,0,5);
         if ($kind != 0)
          {
           warn "Don't understand site type $kind";
           last;
          }
         while (@prop >= 4 && $n-- > 0)
          {
           my ($x,$y,$w,$h) = splice(@prop,0,4);
           push(@$sites,bless [$sn,$xid,$x+$bx,$y+$by,$w,$h,$flags,$id,$token],$class);
          }
        }
       return $class->SUPER::FindSite($token,$X,$Y);
      }
     $id = $token->PointToWindow($X,$Y,$id)
    }
  }
 return $site;
}

my $busy = 0;

sub NewDrag
{
 my ($class,$token) = @_;
 delete $token->{'SunDD'} unless $busy;
 delete $token->{'SunDDSeen'};
}

sub SiteList
{
 my ($class,$token) = @_;
 return @{$token->{'SunDD'}};
}

1;
__END__

# this code is obsolete now that we look at properties ourselves
# which means we don't need dropsite manager running
# On Sun's running OpenLook the window manager or dropsite mananger
# watches for and caches site info in a special selection
# This code got sites from that
#

sub SiteList
{
 my ($class,$token) = @_;
 unless (1 || $busy || exists $token->{'SunDD'})
  {
   Carp::confess('Already doing it!') if ($busy++);
   my @data  = ();
   my @sites = ();
   my $mw = $token->MainWindow;
   $token->{'SunDD'} = \@sites;
   Tk::catch {
      @data = $mw->SelectionGet( '-selection'=>'_SUN_DRAGDROP_DSDM',  '_SUN_DRAGDROP_SITE_RECTS');
   };
   if ($@)
    {
     $token->configure('-cursor'=>'hand2');
     $token->grab(-global);
    }
   else
    {
     while (@data)
      {
       my $version = shift(@data);
       if ($version != 0)
        {
         warn "Unexpected site version $version";
         last;
        }
       push(@sites,bless [splice(@data,0,7)],$class);
      }
    }
   $busy--;
  }
 return @{$token->{'SunDD'}};
}

1;

