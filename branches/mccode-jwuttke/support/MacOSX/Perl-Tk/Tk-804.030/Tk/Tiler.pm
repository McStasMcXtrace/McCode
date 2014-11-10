# Copyright (c) 1995-2004 Nick Ing-Simmons. All rights reserved.
# This program is free software; you can redistribute it and/or
# modify it under the same terms as Perl itself.
# An example of a geometry manager "widget" in perl
package Tk::Tiler;
require Tk;
require Tk::Frame;

use vars qw($VERSION);
#$VERSION = sprintf '4.%03d', q$Revision: #12 $ =~ /\D(\d+)\s*$/;
$VERSION = '4.013';

use base  qw(Tk::Frame);

Construct Tk::Widget 'Tiler';
sub Tk::Widget::ScrlTiler { shift->Scrolled('Tiler' => @_) }

sub FocusChildren
{
 return (wantarray) ? () : 0;
}

sub Populate
{
 my ($obj,$args) = @_;
 $obj->SUPER::Populate($args);
 $obj->{Slaves} = [];
 $obj->{LayoutPending} = 0;
 $obj->{Start} = 0;
 $obj->{Sw}    = 0;
 $obj->{Sh}    = 0;
 $obj->ConfigSpecs('-takefocus'      => ['SELF', 'takeFocus','TakeFocus',1],
                   '-highlightthickness' => ['SELF', 'highlightThickness','HighlightThickness',2],
                   '-yscrollcommand' => ['CALLBACK',undef,undef,undef],
                   '-columns'        => ['PASSIVE','columns','Columns',5],
                   '-rows'           => ['PASSIVE','rows','Rows',10]
                  );
 return $obj;
}

sub change_size
{
 my ($w) = shift;
 my $r  = $w->cget('-rows');
 my $c  = $w->cget('-columns');
 my $bw = $w->cget(-highlightthickness);
 if (defined $r && defined $c)
  {
   $w->GeometryRequest($c*$w->{Sw}+2*$bw,$r*$w->{Sh}+2*$bw);
  }
}

sub Layout
{
 my $m = shift;
 my $bw = $m->cget(-highlightthickness);
 my $why = $m->{LayoutPending};
 $m->{LayoutPending} = 0;
 my $W = $m->Width;
 my $H = $m->Height;
 my $w = $m->{Sw} || 1;  # max width of slave
 my $h = $m->{Sh} || 1;  # max height of slave
 my $x = $bw;
 my $y = $bw;
 my $start = 0;
 # Set size and position of slaves
 my $rows = $m->{Rows} = int(($H-2*$bw)/$h) || 1;
 my $cols = $m->{Cols} = int(($W-2*$bw)/$w) || 1;
 my $need = $m->{Need} = int( (@{$m->{Slaves}}+$cols-1)/$cols );
 $m->{Start} = ($need - $rows) if ($m->{Start} + $rows > $need);

 $m->{Start} = 0               if ($m->{Start} < 0);
 my $row = 0;
 my @posn  = ();
 my $s;
 foreach $s (@{$m->{Slaves}})
  {
   if ($row < $m->{Start})
    {
     $s->UnmapWindow;
     $x += $w;
     if ($x+$w+$bw > $W)
      {
       $x = $bw;
       $row++;
      }
    }
   elsif ($y+$h+$bw > $H)
    {
     $s->UnmapWindow;
     $s->ResizeWindow($w,$h) if ($why & 1);
    }
   else
    {
     push(@posn,[$s,$x,$y]);
     $x += $w;
     if ($x+$w+$bw > $W)
      {
       $x = $bw;
       $y += $h;
       $row++;
      }
    }
   $s->ResizeWindow($w,$h) if ($why & 1);
  }
 $row++ if ($x > $bw);
 if (defined $m->{Prev} && $m->{Prev} > $m->{Start})
  {
   @posn = reverse(@posn);
  }
 while (@posn)
  {
   my $posn = shift(@posn);
   my ($s,$x,$y) = (@$posn);
   $s->MoveWindow($x,$y);
   $s->MapWindow;
  }
 $m->{Prev} = $m->{Start};
 $m->Callback(-yscrollcommand => $m->{Start}/$need,$row/$need) if $need;
}

sub QueueLayout
{
 my ($m,$why) = @_;
 $m->afterIdle(['Layout',$m]) unless ($m->{LayoutPending});
 $m->{LayoutPending} |= $why;
}

sub SlaveGeometryRequest
{
 my ($m,$s) = @_;
 my $sw = $s->ReqWidth;
 my $sh = $s->ReqHeight;
 my $sz = 0;
 if ($sw > $m->{Sw})
  {
   $m->{Sw} = $sw;
   $m->QueueLayout(1);
   $sz++;
  }
 if ($sh > $m->{Sh})
  {
   $m->{Sh} = $sh;
   $m->QueueLayout(1);
   $sz++;
  }
 $m->change_size if ($sz);
}

sub LostSlave
{
 my ($m,$s) = @_;
 @{$m->{Slaves}} = grep($_ != $s,@{$m->{Slaves}});
 $m->QueueLayout(2);
}

sub Manage
{
 my $m = shift;
 my $s;
 foreach $s (@_)
  {
   $m->ManageGeometry($s);
   push(@{$m->{Slaves}},$s);
   $m->SlaveGeometryRequest($s);
  }
 $m->QueueLayout(2 | 1);
}

sub moveto
 {
  my ($m,$frac) = (@_);
  $m->{Start} = int($m->{Need} * $frac);
  $m->QueueLayout(4);
 }

sub scroll
 {
  my ($m,$delta,$type) = @_;
  $delta *= $m->{Rows}/2 if ($type eq 'pages');
  $m->{Start} += $delta;
  $m->QueueLayout(4);
 }

sub yview { my $w = shift; my $c = shift; $w->$c(@_) }

sub FocusIn
{
 my ($w) = @_;
# print 'Focus ',$w->PathName,"\n";
}

sub ClassInit
{
 my ($class,$mw) = @_;
 $mw->bind($class,'<Configure>',['QueueLayout',8]);
 $mw->bind($class,'<FocusIn>',  'NoOp');
 $mw->YscrollBind($class);
 return $class;
}

1;
