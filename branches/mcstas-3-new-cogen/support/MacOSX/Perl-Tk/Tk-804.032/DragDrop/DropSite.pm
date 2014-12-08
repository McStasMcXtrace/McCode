package Tk::DropSite;
require Tk::DragDrop::Common;
require Tk::DragDrop::Rect;

use vars qw($VERSION);
$VERSION = '4.008'; # sprintf '4.%03d', q$Revision: #7 $ =~ /\D(\d+)\s*$/;

use base  qw(Tk::DragDrop::Common Tk::DragDrop::Rect);

Construct Tk::Widget 'DropSite';

use strict;
use vars qw(%type @types);

Tk::DragDrop->Tk::DragDrop::Common::Type('Local');

my @toplevels;

BEGIN
{
 # Are these really methods of Tk::DragDrop::Rect ?
 no strict 'refs';
 foreach my $name (qw(x y X Y width height widget))
  {
   my $key = $name;
   *{"$key"} = sub { shift->{$key} };
  }
}

# Dropping side API - really only here for Local drops
# inheritance is a mess right now.

sub NewDrag
{
 my ($class,$token) = @_;
 # No need to clear cached sites we see live data
}

sub SiteList
{
 # this should be inheritable - so that receive side of XDND can re-use it.
 my ($class,$widget) = @_;
 my $t;
 my @list;
 foreach $t (@toplevels)
  {
   my $sites = $t->{'DropSites'};
   if ($sites)
    {
     $sites = $sites->{'Local'};
     push(@list,@{$sites}) if ($sites);
    }
  }
 return @list;
}

sub Apply
{
 my $site = shift;
 my $name = shift;
 my $cb   = $site->{$name};
 if ($cb)
  {
   my $X = shift;
   my $Y = shift;
   $cb->Call(@_,$X - $site->X, $Y - $site->Y);
  }
}

sub Drop
{
 my ($site,$token,$seln,$event) = @_;
 my $X = $event->X;
 my $Y = $event->Y;
 my @targ = $token->SelectionGet(-selection => $seln,'TARGETS');
 $site->Apply(-dropcommand => $X, $Y, $seln,'LocalDrop',\@targ);
 $site->Apply(-entercommand => $X, $Y, 0);
 $token->Done;
}

sub Enter
{
 my ($site,$token,$event) = @_;
 $token->AcceptDrop;
 $site->Apply(-entercommand => $event->X, $event->Y, 1);
}

sub Leave
{
 my ($site,$token,$event) = @_;
 $token->RejectDrop;
 $site->Apply(-entercommand => $event->X, $event->Y, 0);
}

sub Motion
{
 my ($site,$token,$event) = @_;
 $site->Apply(-motioncommand => $event->X, $event->Y);
}

# This is receive side API.

sub NoteSites
{
 my ($class,$t,$sites) = @_;
 unless (grep($_ == $t,@toplevels))
  {
   $Tk::DragDrop::types{'Local'} = $class if (@$sites);
   push(@toplevels,$t);
   $t->OnDestroy(sub { @toplevels = grep($_ != $t,@toplevels) });
  }
}

sub UpdateDropSites
{
 my ($t) = @_;
 $t->{'DropUpdate'} = 0;
 foreach my $type (@types)
  {
   my $sites = $t->{'DropSites'}->{$type};
   if ($sites && @$sites)
    {
     my $class = $type{$type};
     $class->NoteSites($t,$sites);
    }
  }
}

sub QueueDropSiteUpdate
{
 my $obj = shift;
 my $class = ref($obj);
 my $t   = $obj->widget->toplevel;
 unless ($t->{'DropUpdate'})
  {
   $t->{'DropUpdate'} = 1;
   $t->afterIdle(sub { UpdateDropSites($t) });
  }
}

sub delete
{
 my ($obj) = @_;
 my $w = $obj->widget;
 $w->bindtags([grep($_ ne $obj,$w->bindtags)]);
 my $t = $w->toplevel;
 foreach my $type (@{$obj->{'-droptypes'}})
  {
   my $a = $t->{'DropSites'}->{$type};
   @$a   = grep($_ ne $obj,@$a);
  }
 $obj->QueueDropSiteUpdate;
}

sub DropSiteUpdate
{
 # Note size of widget and arrange to update properties etc.
 my $obj = shift;
 my $w   = $obj->widget;
 $obj->{'x'}      = $w->X;
 $obj->{'y'}      = $w->Y;
 $obj->{'X'}      = $w->rootx;
 $obj->{'Y'}      = $w->rooty;
 $obj->{'width'}  = $w->Width;
 $obj->{'height'} = $w->Height;
 $obj->QueueDropSiteUpdate;
}

sub TopSiteUpdate
{
 my ($t) = @_;
 foreach my $type (@types)
  {
   my $sites = $t->{'DropSites'}->{$type};
   if ($sites && @$sites)
    {
     my $site;
     foreach $site (@$sites)
      {
       $site->DropSiteUpdate;
      }
    }
  }
}

sub Callback
{
 my $obj = shift;
 my $key = shift;
 my $cb  = $obj->{$key};
 $cb->Call(@_) if (defined $cb);
}

sub InitSite
{
 my ($class,$site) = @_;
 # Tk::DragDrop->Type('Local');
}

sub new
{
 my ($class,$w,%args) = @_;
 my $t = $w->toplevel;
 $args{'widget'} = $w;
 if (exists $args{'-droptypes'})
  {
   # Convert single type to array-of-one
   $args{'-droptypes'} = [$args{'-droptypes'}] unless (ref $args{'-droptypes'});
  }
 else
  {
   # Default to all known types.
   $args{'-droptypes'} = \@types;
  }
 my ($key,$val);
 while (($key,$val) = each %args)
  {
   if ($key =~ /command$/)
    {
     $val = Tk::Callback->new($val);
     $args{$key} = $val;
    }
  }
 my $obj = bless \%args,$class;
 unless (exists $t->{'DropSites'})
  {
   $t->{'DropSites'} = {};
   $t->{'DropUpdate'} = 0;
  }
 my $type;
 foreach $type (@{$args{'-droptypes'}})
  {
   Tk::DropSite->import($type) unless (exists $type{$type});
   my $class = $type{$type};
   $class->InitSite($obj);
   # Should this be indexed by type or class ?
   unless (exists $t->{'DropSites'}->{$type})
    {
     $t->{'DropSites'}->{$type}  = [];
    }
   push(@{$t->{'DropSites'}->{$type}},$obj);
  }
 $w->OnDestroy([$obj,'delete']);
 $obj->DropSiteUpdate;
 $w->bindtags([$w->bindtags,$obj]);
 $w->Tk::bind($obj,'<Map>',[$obj,'DropSiteUpdate']);
 $w->Tk::bind($obj,'<Unmap>',[$obj,'DropSiteUpdate']);
 $w->Tk::bind($obj,'<Configure>',[$obj,'DropSiteUpdate']);
 $t->Tk::bind($class,'<Configure>',[\&TopSiteUpdate,$t]);
 unless (grep($_ eq $class,$t->bindtags))
  {
   $t->bindtags([$t->bindtags,$class]);
  }
 return $obj;
}

1;
