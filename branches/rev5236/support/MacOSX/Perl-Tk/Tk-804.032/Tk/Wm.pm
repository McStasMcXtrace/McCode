# Copyright (c) 1995-2003 Nick Ing-Simmons. All rights reserved.
# This program is free software; you can redistribute it and/or
# modify it under the same terms as Perl itself.
package Tk::Wm;
use AutoLoader;

require Tk::Widget;
*AUTOLOAD = \&Tk::Widget::AUTOLOAD;

use strict qw(vars);

# There are issues with this stuff now we have Tix's wm release/capture
# as toplevel-ness is now dynamic.


use vars qw($VERSION);
$VERSION = '4.015'; # was: sprintf '4.%03d', q$Revision: #14 $ =~ /\D(\d+)\s*$/;

use Tk::Submethods;

*{Tk::Wm::wmGrid}    = sub { shift->wm("grid", @_) };
*{Tk::Wm::wmTracing} = sub { shift->wm("tracing", @_) };

Direct Tk::Submethods ('wm' => [qw(aspect attributes client colormapwindows command
                       deiconify focusmodel frame geometry group
                       iconbitmap iconify iconimage iconmask iconname iconphoto
                       iconwindow maxsize minsize overrideredirect positionfrom
                       protocol resizable sizefrom stackorder state title transient
                       withdraw wrapper)]);
		
sub SetBindtags
{
 my ($obj) = @_;
 $obj->bindtags([ref($obj),$obj,'all']);
}

sub Populate
{
 my ($cw,$args) = @_;
 $cw->ConfigSpecs('-overanchor' => ['PASSIVE',undef,undef,undef],
                  '-popanchor'  => ['PASSIVE',undef,undef,undef],
                  '-popover'    => ['PASSIVE',undef,undef,undef]
                 );
}

sub MoveResizeWindow
{
 my ($w,$x,$y,$width,$height) = @_;
 $w->withdraw;
 $w->geometry($width.'x'.$height);
 $w->MoveToplevelWindow($x,$y);
 $w->deiconify;
}

sub WmDeleteWindow
{
 my ($w) = @_;
 my $cb  = $w->protocol('WM_DELETE_WINDOW');
 if (defined $cb)
  {
   $cb->Call;
  }
 else
  {
   $w->destroy;
  }
}


1;

__END__


sub Post
{
 my ($w,$X,$Y) = @_;
 $X = int($X);
 $Y = int($Y);
 $w->positionfrom('user');
 $w->geometry("+$X+$Y");
 # $w->MoveToplevelWindow($X,$Y);
 $w->deiconify;
 $w->raise;
}

sub AnchorAdjust
{
 my ($anchor,$X,$Y,$w,$h) = @_;
 $anchor = 'c' unless (defined $anchor);
 $Y += ($anchor =~ /s/) ? $h : ($anchor =~ /n/) ? 0 : $h/2;
 $X += ($anchor =~ /e/) ? $w : ($anchor =~ /w/) ? 0 : $w/2;
 return ($X,$Y);
}

sub Popup
{
 my $w = shift;
 $w->configure(@_) if @_;
 $w->idletasks;
 my ($mw,$mh) = ($w->reqwidth,$w->reqheight);
 my ($rx,$ry,$rw,$rh) = (0,0,0,0);
 my $base    = $w->cget('-popover');
 my $outside = 0;
 if (defined $base)
  {
   if ($base eq 'cursor')
    {
     ($rx,$ry) = $w->pointerxy;
    }
   else
    {
     $rx = $base->rootx;
     $ry = $base->rooty;
     $rw = $base->Width;
     $rh = $base->Height;
    }
  }
 else
  {
   my $sc = ($w->parent) ? $w->parent->toplevel : $w;
   $rx = -$sc->vrootx;
   $ry = -$sc->vrooty;
   $rw = $w->screenwidth;
   $rh = $w->screenheight;
  }
 my ($X,$Y) = AnchorAdjust($w->cget('-overanchor'),$rx,$ry,$rw,$rh);
 ($X,$Y)    = AnchorAdjust($w->cget('-popanchor'),$X,$Y,-$mw,-$mh);
 # adjust to not cross screen borders
 if ($X < 0) { $X = 0 }
 if ($Y < 0) { $Y = 0 }
 if ($mw > $w->screenwidth)  { $X = 0 }
 if ($mh > $w->screenheight) { $Y = 0 }
 $w->Post($X,$Y);
 $w->waitVisibility;
}

sub FullScreen
{
 my $w = shift;
 my $over = (@_) ? shift : 0;
 my $width  = $w->screenwidth;
 my $height = $w->screenheight;
 $w->GeometryRequest($width,$height);
 $w->overrideredirect($over & 1);
 $w->Post(0,0);
 $w->update;
 if ($over & 2)
  {
   my $x = $w->rootx;
   my $y = $w->rooty;
   $width -= 2*$x;
   $height -= $x + $y;
   $w->GeometryRequest($width,$height);
   $w->update;
  }
}

sub iconposition
{
 my $w = shift;
 if (@_ == 1)
  {
   return $w->wm('iconposition',$1,$2) if $_[0] =~ /^(\d+),(\d+)$/;
   if ($_[0] =~ /^([+-])(\d+)([+-])(\d+)$/)
    {
     my $x = ($1 eq '-') ? $w->screenwidth-$2 : $2;
     my $y = ($3 eq '-') ? $w->screenheight-$4 : $4;
     return $w->wm('iconposition',$x,$y);
    }
  }
 $w->wm('iconposition',@_);
}

