package Tk::X11Font;
use vars qw($VERSION);
$VERSION = '4.007'; # $Id: //depot/Tkutf8/Tk/X11Font.pm#7 $

require Tk::Widget;
require Tk::Xlib;
use strict;

Construct Tk::Widget 'X11Font';

my @field = qw(foundry family weight slant swidth adstyle pixel
               point xres yres space avgwidth registry encoding);

map { eval "sub \u$_ { shift->elem('$_', \@_) }" } @field;

use overload '""' => 'as_string';

sub new
{
 my $pkg = shift;
 my $w   = shift;

 my %me = ();
 my $d  = $w->Display;

 local $_;

 if(scalar(@_) == 1)
  {
   my $pattern = shift;

   if($pattern =~ /\A(-[^-]*){14}\Z/)
    {
     @me{@field} = split(/-/, substr($pattern,1));
    }
   else
    {
     $me{Name} = $pattern;

     if($pattern =~ /^[^-]?-([^-]*-){2,}/)
      {
       my $f = $d->XListFonts($pattern,1);

       if($f && $f =~ /\A(-[^-]*){14}/)
        {
         my @f = split(/-/, substr($f,1));
         my @n = split(/-/, $pattern);
         my %f = ();
         my $i = 0;

         shift @n if($pattern =~ /\A-/);

         while(@n && @f)
          {
           if($n[0] eq '*')
            {
             shift @n;
            }
           elsif($n[0] eq $f[0])
            {
             $f{$field[$i]} = shift @n;
            }
           $i++;
           shift @f;
          }

         %me = %f
           unless(@n);
        }
      }
    }
  }
 else
  {
   %me = @_;
  }

 map { $me{$_} ||= '*' } @field;

 $me{Display} = $d;
 $me{MainWin} = $w->MainWindow;

 bless \%me, $pkg;
}

sub Pattern
{
 my $me  = shift;
 return join('-', '',@{$me}{@field});
}

sub Name
{
 my $me  = shift;
 my $max = wantarray ? shift || 128 : 1;

 if ($^O eq 'MSWin32' or ($^O eq 'cygwin' and $Tk::platform eq 'MSWin32'))
  {
   my $name = $me->{Name};
   if (!defined $name)
    {
     my $fm  = $me->{'family'} || 'system';
     my $sz  = -int($me->{'point'}/10) || -($me->{'pixel'}) || 12;
     my @opt = (-family => $fm, -size => $sz );
     my $wt  = $me->{'weight'};
     if (defined $wt)
      {
       $wt = 'normal' unless $wt =~ /bold/i;
       push(@opt,-weight => lc($wt));
      }
     my $sl  = $me->{'slant'};
     if (defined $sl)
      {
       $sl = ($sl =~ /^[io]/) ? 'italic' : 'roman';
       push(@opt,-slant => $sl);
      }
     $name = join(' ',@opt);
    }
   return $name;
  }
 else
  {
   my $name = $me->{Name} ||
              join('-', '',@{$me}{@field});
   return $me->{Display}->XListFonts($name,$max);
  }
}

sub as_string
{
 return shift->Name;
}

sub elem
{
 my $me   = shift;
 my $elem = shift;

 return undef
   if(exists $me->{'Name'});

 my $old  = $me->{$elem};

 $me->{$elem} = shift
   if(@_);

 $old;
}

sub Clone
{
 my $me = shift;

 $me = bless { %$me }, ref($me);

 unless(exists $me->{'Name'})
  {
   while(@_)
    {
     my $k = shift;
     my $v = shift || $me->{MainWin}->BackTrace('Tk::Font->Clone( key => value, ... )');
     $me->{$k} = $v;
    }
  }

 $me;
}

sub ascent
{
 my $me = shift;
 my $name = $me->Name;
 $me->{MainWin}->fontMetrics($name, '-ascent');
}

sub descent
{
 my $me = shift;
 my $name = $me->Name;
 $me->{MainWin}->fontMetrics($name, '-descent');
}

1;

