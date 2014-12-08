package Tk::Font;
use vars qw($VERSION);
$VERSION = '4.004'; # $Id: //depot/Tkutf8/Tk/Font.pm#4 $
require Tk::Widget;
use strict;
use Carp;
use overload '""' => 'as_string';
sub as_string { return ${$_[0]} }

*MainWindow = \&Tk::Widget::MainWindow;

foreach my $key (qw(actual metrics measure configure))
 {
  no strict 'refs';
  *{$key} = sub { shift->Tk::font($key,@_) };
 }

Construct Tk::Widget 'Font';

my @xfield  = qw(foundry family weight slant swidth adstyle pixel
               point xres yres space avgwidth registry encoding);
my @tkfield = qw(family size weight slant underline overstrike);
my %tkfield = map { $_ => "-$_" } @tkfield;

sub _xonly { my $old = '*'; return $old }

sub Pixel
{
 my $me  = shift;
 my $old = $me->configure('-size');
 $old = '*' if ($old > 0);
 if (@_)
  {
   $me->configure(-size => -$_[0]);
  }
 return $old;
}

sub Point
{
 my $me  = shift;
 my $old = 10*$me->configure('-size');
 $old = '*' if ($old < 0);
 if (@_)
  {
   $me->configure(-size => int($_[0]/10));
  }
 return $old;
}

foreach my $f (@tkfield,@xfield)
 {
  no strict 'refs';
  my $sub = "\u$f";
  unless (defined &{$sub})
   {
    my $key = $tkfield{$f};
    if (defined $key)
     {
      *{$sub} = sub { shift->configure($key,@_) };
     }
    else
     {
      *{$sub} = \&_xonly;
     }
   }
 }

sub new
{
 my $pkg  = shift;
 my $w    = shift;
 my $me;
 if (scalar(@_) == 1)
  {
   $me = $w->Tk::font('create',@_);
  }
 else
  {
   croak 'Odd number of args' if @_ & 1;
   my %attr;
   while (@_)
    {
     my $k = shift;
     my $v = shift;
     my $t = (substr($k,0,1) eq '-') ? $k : $tkfield{$k};
     if (defined $t)
      {
       $attr{$t} = $v;
      }
     elsif ($k eq 'point')
      {
       $attr{'-size'} = -int($v/10+0.5);
      }
     elsif ($k eq 'pixel')
      {
       $attr{'-size'} = -$v;
      }
     else
      {
       carp "$k ignored" if $^W;
      }
    }
   $me = $w->Tk::font('create',%attr);
  }
 return bless $me,$pkg;
}

sub Pattern
{
 my $me  = shift;
 my @str;
 foreach my $f (@xfield)
  {
   my $meth = "\u$f";
   my $str  = $me->$meth();
   if ($f eq 'family')
    {
     $str =~ s/(?:Times\s+New\s+Roman|New York)/Times/i;
     $str =~ s/(?:Courier\s+New|Monaco)/Courier/i;
     $str =~ s/(?:Arial|Geneva)/Helvetica/i;
    }
   elsif ($f eq 'slant')
    {
     $str = substr($str,0,1);
    }
   elsif ($f eq 'weight')
    {
     $str = 'medium' if ($str eq 'normal');
    }
   push(@str,$str);
  }
 return join('-', '', @str);
}

sub Name
{
 my $me  = shift;
 return $$me if (!wantarray || ($^O eq 'MSWin32'));
 my $max = shift || 128;
 my $w = $me->MainWindow;
 my $d = $w->Display;
 return $d->XListFonts($me->Pattern,$max);
}

sub Clone
{
 my $me = shift;
 return ref($me)->new($me,$me->actual,@_);
}

sub ascent
{
 return shift->metrics('-ascent');
}

sub descent
{
 return shift->metrics('-descent');
}

1;

