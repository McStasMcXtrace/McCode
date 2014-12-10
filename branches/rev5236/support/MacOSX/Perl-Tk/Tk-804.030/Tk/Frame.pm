# Copyright (c) 1995-2003 Nick Ing-Simmons. All rights reserved.
# This program is free software; you can redistribute it and/or
# modify it under the same terms as Perl itself.
package Tk::Frame;
require Tk::Widget;
require Tk::Derived;
use AutoLoader;
use strict qw(vars);
use Carp;

use base qw(Tk::Derived Tk::Widget);

Construct Tk::Widget 'Frame';

use vars qw($VERSION);
$VERSION = '4.010'; # $Id: //depot/Tkutf8/Tk/Frame.pm#10 $

sub Tk_cmd { \&Tk::frame }

sub CreateOptions
{
 return (shift->SUPER::CreateOptions,'-colormap','-visual','-container')
}

sub Default
{
 my ($cw,$name,$widget)  = @_;
 confess 'No name' unless (defined $name);
 croak 'No widget' unless (defined $widget);
 $cw->Delegates(DEFAULT => $widget);
 $cw->ConfigSpecs(DEFAULT => [$widget]);
 $widget->pack('-expand' => 1, -fill => 'both') unless ($widget->manager);  # Suspect
 $cw->Advertise($name,$widget);
}

sub ConfigDelegate
{
 my ($cw,$name,@skip) = @_;
 my $sw = $cw->Subwidget($name);
 my $sc;
 my %skip = ();
 foreach $sc (@skip)
  {
   $skip{$sc} = 1;
  }
 foreach $sc ($sw->configure)
  {
   my (@info) = @$sc;
   next if (@info == 2);
   my $option = $info[0];
   unless ($skip{$option})
    {
     $option =~ s/^-(.*)/-$name\u$1/;
     $info[0] = Tk::Configure->new($sw,$info[0]);
     pop(@info);
     $cw->ConfigSpecs($option => \@info);
    }
  }
}

sub bind
{my ($cw,@args) = @_;
 $cw->Delegate('bind',@args);
}

sub menu
{my ($cw,@args) = @_;
 $cw->Delegate('menu',@args);
}

sub focus
{my ($cw,@args) = @_;
 $cw->Delegate('focus',@args);
}

#sub bindtags
#{my ($cw,@args) = @_;
# $cw->Delegate('bindtags',@args);
#}

sub selection
{my ($cw,@args) = @_;
 $cw->Delegate('selection',@args);
}

sub autoLabel { 1 }

sub Populate
{
 my ($cw,$args) = @_;
 if ($cw->autoLabel)
  {
   $cw->ConfigSpecs('-labelPack'     => [ 'METHOD', undef, undef, undef]);
   $cw->ConfigSpecs('-labelVariable' => [ 'METHOD', undef, undef, undef]);
   $cw->ConfigSpecs('-label'         => [ 'METHOD', undef, undef, undef]);
   $cw->labelPack([]) if grep /^-label\w+/, keys %$args;
  }
}

sub Menubar
{
 my $frame = shift;
 my $menu = $frame->cget('-menu');
 if (defined $menu)
  {
   $menu->configure(@_) if @_;
  }
 else
  {
   $menu = $frame->Menu(-type => 'menubar',@_);
   $frame->configure('-menu' => $menu);
  }
 $frame->Advertise('menubar' => $menu);
 return $menu;
}

1;

__END__

sub labelPack
{
 my ($cw,$val) = @_;
 my $w = $cw->Subwidget('label');
 my @result = ();
 if (@_ > 1)
  {
   if (defined($w) && !defined($val))
    {
     $w->packForget;
    }
   elsif (defined($val) && !defined ($w))
    {
     require Tk::Label;
     $w = Tk::Label->new($cw,-textvariable => $cw->labelVariable);
     $cw->Advertise('label' => $w);
     $cw->ConfigDelegate('label',qw(-text -textvariable));
    }
   if (defined($val) && defined($w))
    {
     my %pack = @$val;
     unless (exists $pack{-side})
      {
       $pack{-side} = 'top' unless (exists $pack{-side});
      }
     unless (exists $pack{-fill})
      {
       $pack{-fill} = 'x' if ($pack{-side} =~ /(top|bottom)/);
       $pack{-fill} = 'y' if ($pack{-side} =~ /(left|right)/);
      }
     unless (exists($pack{'-before'}) || exists($pack{'-after'}))
      {
       my $before = ($cw->packSlaves)[0];
       $pack{'-before'} = $before if (defined $before);
      }
     $w->pack(%pack);
    }
  }
 @result = $w->packInfo if (defined $w);
 return (wantarray) ? @result : \@result;
}

sub labelVariable
{
 my ($cw,$val) = @_;
 my $var = \$cw->{Configure}{'-labelVariable'};
 if (@_ > 1 && defined $val)
  {
   $$var = $val;
   $$val = '' unless (defined $$val);
   my $w = $cw->Subwidget('label');
   unless (defined $w)
    {
     $cw->labelPack([]);
     $w = $cw->Subwidget('label');
    }
   $w->configure(-textvariable => $val);
  }
 return $$var;
}

sub label
{
 my ($cw,$val) = @_;
 my $var = $cw->cget('-labelVariable');
 if (@_ > 1 && defined $val)
  {
   if (!defined $var)
    {
     $var = \$cw->{Configure}{'-label'};
     $cw->labelVariable($var);
    }
   $$var = $val;
  }
 return (defined $var) ? $$var : undef;;
}

sub queuePack
{
 my ($cw) = @_;
 unless ($cw->{'pack_pending'})
  {
   $cw->{'pack_pending'} = 1;
   $cw->afterIdle([$cw,'packscrollbars']);
  }
}

sub sbset
{
 my ($cw,$sb,$ref,@args) = @_;
 $sb->set(@args);
 $cw->queuePack if (@args == 2 && $sb->Needed != $$ref);
}

sub freeze_on_map
{
 my ($w) = @_;
 unless ($w->Tk::bind('Freeze','<Map>'))
  {
   $w->Tk::bind('Freeze','<Map>',['packPropagate' => 0])
  }
 $w->AddBindTag('Freeze');
}

sub AddScrollbars
{
 require Tk::Scrollbar;
 my ($cw,$w) = @_;
 my $def = '';
 my ($x,$y) = ('','');
 my $s = 0;
 my $c;
 $cw->freeze_on_map;
 foreach $c ($w->configure)
  {
   my $opt = $c->[0];
   if ($opt eq '-yscrollcommand')
    {
     my $slice  = Tk::Frame->new($cw,Name => 'ysbslice');
     my $ysb    = Tk::Scrollbar->new($slice,-orient => 'vertical', -command => [ 'yview', $w ]);
     my $size   = $ysb->cget('-width');
     my $corner = Tk::Frame->new($slice,Name=>'corner','-relief' => 'raised',
                  '-width' => $size, '-height' => $size);
     $ysb->pack(-side => 'left', -fill => 'y');
     $cw->Advertise('yscrollbar' => $ysb);
     $cw->Advertise('corner' => $corner);
     $cw->Advertise('ysbslice' => $slice);
     $corner->{'before'} = $ysb->PathName;
     $slice->{'before'} = $w->PathName;
     $y = 'w';
     $s = 1;
    }
   elsif ($opt eq '-xscrollcommand')
    {
     my $xsb = Tk::Scrollbar->new($cw,-orient => 'horizontal', -command => [ 'xview', $w ]);
     $cw->Advertise('xscrollbar' => $xsb);
     $xsb->{'before'} = $w->PathName;
     $x = 's';
     $s = 1;
    }
  }
 if ($s)
  {
   $cw->Advertise('scrolled' => $w);
   $cw->ConfigSpecs('-scrollbars' => ['METHOD','scrollbars','Scrollbars',$x.$y]);
  }
}

sub packscrollbars
{
 my ($cw) = @_;
 my $opt    = $cw->cget('-scrollbars');
 my $slice  = $cw->Subwidget('ysbslice');
 my $xsb    = $cw->Subwidget('xscrollbar');
 my $corner = $cw->Subwidget('corner');
 my $w      = $cw->Subwidget('scrolled');
 my $xside  = (($opt =~ /n/) ? 'top' : 'bottom');
 my $havex  = 0;
 my $havey  = 0;
 $opt =~ s/r//;
 $cw->{'pack_pending'} = 0;
 if (defined $slice)
  {
   my $reqy;
   my $ysb    = $cw->Subwidget('yscrollbar');
   if ($opt =~ /(o)?[we]/ && (($reqy = !defined($1)) || $ysb->Needed))
    {
     my $yside = (($opt =~ /w/) ? 'left' : 'right');
     $slice->pack(-side => $yside, -fill => 'y',-before => $slice->{'before'});
     $havey = 1;
     if ($reqy)
      {
       $w->configure(-yscrollcommand => ['set', $ysb]);
      }
     else
      {
       $w->configure(-yscrollcommand => ['sbset', $cw, $ysb, \$cw->{'packysb'}]);
      }
    }
   else
    {
     $w->configure(-yscrollcommand => undef) unless $opt =~ s/[we]//;
     $slice->packForget;
    }
   $cw->{'packysb'} = $havey;
  }
 if (defined $xsb)
  {
   my $reqx;
   if ($opt =~ /(o)?[ns]/ && (($reqx = !defined($1)) || $xsb->Needed))
    {
     $xsb->pack(-side => $xside, -fill => 'x',-before => $xsb->{'before'});
     $havex = 1;
     if ($reqx)
      {
       $w->configure(-xscrollcommand => ['set', $xsb]);
      }
     else
      {
       $w->configure(-xscrollcommand => ['sbset', $cw, $xsb, \$cw->{'packxsb'}]);
      }
    }
   else
    {
     $w->configure(-xscrollcommand => undef) unless $opt =~ s/[ns]//;
     $xsb->packForget;
    }
   $cw->{'packxsb'} = $havex;
  }
 if (defined $corner)
  {
   if ($havex && $havey && defined $corner->{'before'})
    {
     my $anchor = $opt;
     $anchor =~ s/o//g;
     $corner->configure(-height => $xsb->ReqHeight);
     $corner->pack(-before => $corner->{'before'}, -side => $xside,
                   -anchor => $anchor, -fill => 'x');
    }
   else
    {
     $corner->packForget;
    }
  }
}

sub scrollbars
{
 my ($cw,$opt) = @_;
 my $var = \$cw->{'-scrollbars'};
 if (@_ > 1)
  {
   my $old = $$var;
   if (!defined $old || $old ne $opt)
    {
     $$var = $opt;
     $cw->queuePack;
    }
  }
 return $$var;
}

sub FindMenu
{
 my ($w,$char) = @_;
 my $child;
 my $match;
 foreach $child ($w->children)
  {
   next unless (ref $child);
   $match = $child->FindMenu($char);
   return $match if (defined $match);
  }
 return undef;
}



