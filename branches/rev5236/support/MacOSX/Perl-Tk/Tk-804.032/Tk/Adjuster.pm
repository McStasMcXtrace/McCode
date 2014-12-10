package Tk::Adjuster;

use vars qw($VERSION);
$VERSION = '4.008'; # $Id: //depot/Tkutf8/Tk/Adjuster.pm#7 $

use base  qw(Tk::Frame);

# We cannot do this :

# Construct Tk::Widget 'packAdjust';

# because if managed object is Derived (e.g. a Scrolled) then our 'new'
# will be delegated and hierachy gets turned inside-out
# So packAdjust is autoloaded in Widget.pm


Construct Tk::Widget qw(Adjuster);

{package Tk::Adjuster::Item;

use strict;
use base  qw(Tk::Frame);

sub ClassInit
{
 my ($class,$mw) = @_;
 $mw->bind($class,'<1>',['BDown', 1]);
 $mw->bind($class,'<Shift-1>',['BDown', 0]);
 $mw->bind($class,'<B1-Motion>',['Motion',1]);
 $mw->bind($class,'<Shift-B1-Motion>',['Motion',0]);
 $mw->bind($class,'<ButtonRelease-1>',['Motion',0]);
 return $class;
}

sub BDown
{
 my($w, $delay_mask) = @_;
 $w->{'start_x'} = $w->XEvent->x;
 $w->{'start_y'} = $w->XEvent->y;
 my $adj  = $w->Parent;
 delete $adj->{'lin_info'};
 my $delay = $delay_mask && $adj->cget('-delay');
 if ($delay)
  {
    $adj->vert ? $adj->delta_width_bar(0) : $adj->delta_height_bar(0);
  }
}

sub Motion
{
 my($w, $delay_mask) = @_;
 my $ev = $w->XEvent;
 my $adj  = $w->Parent;

 my $delay = $delay_mask && $adj->cget('-delay');
 if ($adj->vert)
  {
    my $dx = $ev->x - $w->{'start_x'};
    $delay ?  $adj->delta_width_bar($dx) : $adj->delta_width($dx);
  }
 else
  {
    my $dy = $ev->y - $w->{'start_y'};
    $delay ? $adj->delta_height_bar($dy) : $adj->delta_height($dy);
  }
}

}



sub packAfter
{
 my ($w,$s,%args) = @_;
 my $side = $args{'-side'} ? $args{'-side'} : 'top';
 $w->configure(-side   => $side, -widget => $s);
 $w->packed($s, %args);
}

sub packForget
{
 my ($w,$forget_slave) = @_;
 $w->Tk::Widget::packForget;
 $w->slave->packForget if $forget_slave;
}

# Called by Tk::Widget::packAdjust. It was here before packAfter was added
sub packed
{
 my ($w,$s,%args) = @_;
 delete $args{'-before'};
 delete $args{'-in'};
 $args{'-expand'} = 0;
 $args{'-after'} = $s;
 $args{'-fill'} = (($w->vert) ? 'y' : 'x');
 $w->pack(%args);
}

sub gridded
{
 my ($w,$s,%args) = @_;
 # delete $args{'-before'};
 # $args{'-expand'} = 0;
 # $args{'-after'} = $s;
 # $args{'-fill'} = (($w->vert) ? 'y' : 'x');
 $w->grid(%args);
}

sub ClassInit
{
 my ($class,$mw) = @_;
 $mw->bind($class,'<Configure>','SizeChange');
 $mw->bind($class,'<Unmap>','Restore');
 $mw->bind($class,'<Map>','Mapped');
 return $class;
}

sub SizeChange
{
 my $w = shift;
 # reqwidth/height of Adjuster is stored here. If it is partially pushed out
 # of the window, then $w->width/height returns that of the visible part.
 if ($w->vert)
  {
   my $sx = ($w->Width - $w->{'sep'}->Width)/2;
   $w->{'but'}->place('-x' => 0, '-y' => $w->Height-18);
   $w->{'sep'}->place('-x' => $sx, '-y' => 0,  -relheight => 1);
   $w->configure(-width => $w->{'but'}->ReqWidth);
   $w->{'reqwidth'} = $w->reqwidth;
  }
 else
  {
   my $sy = ($w->Height - $w->{'sep'}->Height)/2;
   $w->{'but'}->place('-x' => $w->Width-18, '-y' => 0);
   $w->{'sep'}->place('-x' => 0, '-y' => $sy,  -relwidth => 1);
   $w->configure(-height => $w->{'but'}->ReqHeight);
   $w->{'reqheight'} = $w->reqheight;
  }
 # Turn off geometry propagation in the slave. Do only if necessary, as this
 # causes repacking.
 my $s = $w->slave;
 $s->packPropagate('0') if $s->packSlaves && $s->packPropagate();
 $s->gridPropagate('0') if $s->gridSlaves && $s->gridPropagate();
}

sub Mapped
{
 my $w = shift;
 $w->idletasks;
 my $m = $w->manager;
 if ($m =~ /^(?:pack|grid)$/)
  {
   my %info = $w->$m('info');
   my $master = $info{'-in'};
   $master->$m('propagate',0);
   $w->{'master'} = $master;
  }
 $w->slave_expand_off;
}

sub Populate
{
 my ($w,$args) = @_;
 $w->SUPER::Populate($args);
 $w->{'sep'} = Tk::Adjuster::Item->new($w,-bd => 1, -relief => 'sunken');
 $w->{'but'} = Tk::Adjuster::Item->new($w,-bd => 1, -width => 8, -height => 8, -relief => 'raised');

 # Force creation of Frame in widget's Toplevel. This makes a difference
 # where the widget's Toplevel has Delegates('Construct') set.
 # Need to explicitly set frame width to 0 for Win32
 my $l = $w->{'lin'} = Tk::Frame->new($w->toplevel, -bd => 0);
 # my $l = $w->{'lin'} = $w->toplevel->Frame(-bd => 0);

 my $cs = $w->ConfigSpecs(-widget => ['PASSIVE','widget','Widget',$w->Parent],
                 -side       => ['METHOD','side','Side','top'],
                 -delay      => ['PASSIVE','delay','Delay', 1],
                 -background => [['SELF',$w->{'sep'},$w->{'but'}],'background','Background',undef],
                 -foreground => [Tk::Configure->new($w->{'lin'},'-background'),'foreground','Foreground','black'],
		 -restore    => ['PASSIVE','restore', 'Restore', 1],
                );
 $w->_OnDestroy(qw(sep but lin master));
}

sub side
{
 my ($w,$val) = @_;
 if (@_ > 1)
  {
   $w->{'side'} = $val;
   my $cursor;
   if ($w->vert)
    {
     $cursor = 'sb_h_double_arrow';
     $w->{'sep'}->configure(-width => 2, -height => 10000);
    }
   else
    {
     $cursor = 'sb_v_double_arrow';
     $w->{'sep'}->configure(-height => 2, -width => 10000);
    }
   my $x;
   foreach $x ($w->{'sep'},$w->{'but'})
    {
     $x->configure(-cursor => $cursor);
    }
  }
 return $w->{'side'};
}

sub slave
{
 my $w = shift;
 my $s = $w->cget('-widget');
 return $s;
}

sub vert
{
 my $w = shift;
 my $side = $w->cget('-side');
 return  1 if $side eq 'left';
 return -1 if $side eq 'right';
 return  0;
}

# If the Adjuster gets unmapped, it attempts to restore itself. If its
# slave is mapped, then it reduces the size of the slave so that there is
# then room in the master for the Adjuster widget.
sub Restore
{
 my $w = shift;
 return if ! $w->toplevel->IsMapped ||
 	   ! $w->slave->IsMapped ||
	   ! $w->cget('-restore');
 $w->vert ? $w->delta_width(0) : $w->delta_height(0);
}

sub delta_width_bar
{
 my ($w,$dx) = @_;
 my $l = $w->{'lin'};
 my $r = $w->{'sep'};
 my $t = $w->toplevel;
 my $m = $w->{'master'};
 my $s = $w->slave;
 my ($min_rootx, $max_rootx, $t_border);
 if (! $w->{'lin_info'})
  {
   my $m_border = $m->cget('-bd') + $m->cget('-highlightthickness');
   $t_border    = $t->cget('-bd') + $t->cget('-highlightthickness');
   if ($w->cget('-side') eq 'right')
    {
     $min_rootx = $m->rootx + $m_border;
     $max_rootx = $s->rootx + $s->width - 1;
    }
   else
    {
     $min_rootx = $s->rootx;
     $max_rootx = $m->rootx + $m->width - $m_border - 1;
    }
   $w->{'lin_info'} = [$min_rootx, $max_rootx, $t_border];
  }
  else
   {
    ($min_rootx, $max_rootx, $t_border) = @{$w->{'lin_info'}};
   }
 $l->configure(-width => 1, -height => $w->height) unless $l->IsMapped;

 my $new_rootx = $w->rootx + $w->{'reqwidth'}/2 + $dx;
 $new_rootx = $min_rootx if $new_rootx < $min_rootx;
 $new_rootx = $max_rootx if $new_rootx > $max_rootx;
 my $placex = $new_rootx - $t->rootx - $t_border;
 my $placey = $w->rooty  - $t->rooty - $t_border;
 $l->place(-in => $t, -anchor => 'n', '-x' => $placex, '-y' => $placey);
 my $this = $w->containing($new_rootx, $w->rooty + 1);
 $l->raise($this) if $this && $this ne $t;
}

sub delta_width
{
 my ($w,$dx) = @_;
 my $l = $w->{'lin'};
 $l->placeForget;
 my $s = $w->slave;
 if ($s)
  {
   my $m = $w->{'master'};
   my $m_border = $m->cget('-bd') + $m->cget('-highlightthickness');
   my $w_width = $w->{'reqwidth'};
   my $m_width = $m->width;
   my $s_width = $s->width;
   my $max_width = $m_width - $w_width;
   my $max_s_width;
   if ($w->cget('-side') eq 'right')
    {
     $dx = -$dx;
     $max_s_width = $max_width -
		      ($m->rootx + $m_width - ($s->rootx+$s_width)) - $m_border;
    }
   else
    {
     $max_s_width = $max_width - ($s->rootx - $m->rootx) - $m_border;
    }
   my $new_width = $s_width+$dx;
   $new_width = $max_s_width if $new_width > $max_s_width;
   $new_width = 0 if $new_width < 0;
   $s->GeometryRequest($new_width, $s->height);
  }
}

sub delta_height_bar
{
 my ($w,$dy) = @_;
 my $l = $w->{'lin'};
 my $r = $w->{'sep'};
 my $t = $w->toplevel;
 my $m = $w->{'master'};
 my $s = $w->slave;
 my ($min_rooty, $max_rooty, $t_border);
 if (! $w->{'lin_info'})
  {
   my $m_border = $m->cget('-bd') + $m->cget('-highlightthickness');
   $t_border    = $t->cget('-bd') + $t->cget('-highlightthickness');
   if ($w->cget('-side') eq 'bottom')
    {
     $min_rooty = $m->rooty + $m_border;
     $max_rooty = $s->rooty + $s->height - 1;
    }
   else
    {
     $min_rooty = $s->rooty;
     $max_rooty = $m->rooty + $m->height - $m_border - 1;
    }
   $w->{'lin_info'} = [$min_rooty, $max_rooty, $t_border];
  }
 else
  {
   ($min_rooty, $max_rooty, $t_border) = @{$w->{'lin_info'}};
  }
 $l->configure(-height => 1, -width => $w->width) unless $l->IsMapped;

 my $new_rooty = $w->rooty + $w->{'reqheight'}/2 + $dy;
 $new_rooty = $min_rooty if $new_rooty < $min_rooty;
 $new_rooty = $max_rooty if $new_rooty > $max_rooty;
 my $placey = $new_rooty - $t->rooty - $t_border;
 my $placex = $w->rootx  - $t->rootx - $t_border;
 $l->place(-in => $t, -anchor => 'w', '-x' => $placex, '-y' => $placey);
 my $this = $w->containing($w->rootx + 1, $new_rooty);
 $l->raise($this) if $this && $this ne $t;
}

sub delta_height
{
 my ($w,$dy) = @_;
 my $l = $w->{'lin'};
 $l->placeForget;
 my $s = $w->slave;
 if ($s)
  {
   my $m = $w->{'master'};
   my $m_border = $m->cget('-bd') + $m->cget('-highlightthickness');
   my $w_height = $w->{'reqheight'};
   my $m_height = $m->height;
   my $s_height = $s->height;
   my $max_height = $m_height - $w_height;
   my $max_s_height;
   if ($w->cget('-side') eq 'bottom')
    {
     $dy = -$dy;
     $max_s_height = $max_height -
		    ($m->rooty + $m_height - ($s->rooty+$s_height)) - $m_border;
    }
   else
    {
     $max_s_height = $max_height - ($s->rooty - $m->rooty) - $m_border;
    }
   my $new_height = $s_height+$dy;

   $new_height = $max_s_height if $new_height > $max_s_height;
   $new_height = 0 if $new_height < 0;
   $s->GeometryRequest($s->width, $new_height);
  }
}

# Turn off expansion in the slave.
# This is done only if necessary, as calls to pack/gridConfigure cause
# repacking.
# Before call to pack/gridConfigure, the reqwidth/reqheight is set to the
# current width/height. This is because the geometry managers use
# the requested values, not the actual, to calculate the new geometry.
sub slave_expand_off
{
 my $w = shift;
 my $s = $w->slave;
 return if ! $s;

 my $manager = $s->manager;
 if ($manager eq 'pack')
  {
   my %info = $s->packInfo;
   my $expand = $info{'-expand'};
   if ($expand)
    {
     $s->GeometryRequest($s->width, $s->height);
     $s->packConfigure(-expand => 0);
    }
  }
 elsif ($manager eq 'grid')
  {
   my %info = $s->gridInfo;
   my $master = $info{'-in'};
   if ($w->vert)
    {
     my $col = $info{'-column'};
     my $expand = $master->gridColumnconfigure($col, '-weight');
     if ($expand)
      {
       $s->GeometryRequest($s->width, $s->height);
       $master->gridColumnconfigure($col, -weight => 0);
      }
    }
   else
    {
     my $row = $info{'-row'};
     my $expand = $master->gridRowconfigure($row, '-weight');
     if ($expand)
      {
       $s->GeometryRequest($s->width, $s->height);
       $master->gridRowconfigure($row, -weight => 0);
      }
    }
  }
}

1;

__END__

