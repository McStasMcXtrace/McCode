package Tk::Region;

# Ideas in progress do not document ...

use strict;

use vars qw($VERSION);
$VERSION = '4.006'; # $Id: //depot/Tkutf8/Tk/Region.pm#6 $

use Tk::Widget ();

Construct Tk::Widget 'Region';

my %index = (-widget => 1, '-x' => 2, '-y' => 3, -width => 4, -height => 5);

sub _attr
{
 my ($obj,$key,$val) = @_;
 if (@_ > 2)
  {
   $obj->{$key} = $val;
  }
 return $obj->{$key}
}

foreach my $name (qw(widget x y width height))
 {
  my $key = "-$name";
  no strict 'refs';
  *{$name} = sub { shift->_attr($key,@_) };
 }

sub new
{
 my $class  = shift;
 my $widget = shift;
 my $obj = bless [\%index,$widget,0,0,0,0],$class;
 $obj->configure(@_);
}

sub cfgDefault
{
 my ($class,$key) = @_;
 return undef;
}

sub cfgName
{
 my ($class,$key) = @_;
 $key =~ s/^-//;
 return lcfirst($key);
}

sub cfgClass
{
 return ucfirst(shift->cfgName(@_));
}

sub configure
{
 my $obj = shift;
 my @results;
 if (@_ > 1)
  {
   while (@_)
    {
     my $key = shift;
     my $val = shift;
     if (exists $obj->{$key})
      {
       $obj->{$key} = $val;
      }
     else
      {
       my ($meth) = $key =~ /^-(\w+)$/;
       croak("Invalid option $key") unless $obj->can($meth);
       $obj->$meth($val);
      }
    }
  }
 elsif (@_ == 1)
  {
   my $key     = shift;
   my $value   = $obj->cget($key);
   push(@results,$key,$obj->cfgName($key),$obj->cfgClass($key),$obj->cfgDefault($key),$value);
  }
 else
  {
   foreach my $key (sort keys %$obj)
    {
     push(@results,scalar($obj->configure($key)))
    }
  }
 return wantarray ? @results : \@results;
}

sub cget
{
 my $obj = shift;
 my $key = shift;
 return $obj->{$key} if exists $obj->{$key};
 my ($meth) = $key =~ /^-(\w+)$/;
 croak("Invalid option $key") unless $obj->can($meth);
 return $obj->$meth();
}

sub bbox
{
 my $obj = shift;
 my @results;
 if (@_)
  {
   my $ref = (@_ == 1) ? shift : \@_;
   my ($x1,$y1,$x2,$y2) = (ref $ref) ? @$ref : split(/\s+/,$ref);
   ($x2,$x1) = ($x1,$x2) if ($x2 < $x1);
   ($y2,$y1) = ($y1,$y2) if ($y2 < $y1);
   $obj->width($x2-$x1);
   $obj->height($y2-$y1);
   $obj->x($x1);
   $obj->y($y1);
  }
 else
  {
   my $x = $obj->x;
   my $y = $obj->x;
   push(@results,$x,$y,$x+$obj->width,$y+$obj->height);
  }
 return wantarray ? @results : \@results;
}

sub rootx
{
 my $obj = shift;
 if (@_)
  {
   my $x = shift;
   $obj->x($x-$obj->widget->rootx);
  }
 return $obj->widget->rootx + $obj->{'-x'}
}

sub rooty
{
 my $obj = shift;
 if (@_)
  {
   my $y = shift;
   $obj->y($y-$obj->widget->rootx);
  }
 return $obj->widget->rooty + $obj->{'-y'}
}

sub rootxy
{
 my $obj = shift;
 if (@_)
  {
   $obj->rootx(shift);
   $obj->rooty(shift);
  }
 my @results = ($obj->rootx,$obj->rooty);
 return wantarray ? @results : \@results;
}

sub rootbbox
{
 my $obj = shift;
 my ($x1,$y1) = $obj->rootxy;
 my $x2 = $x1+$obj->width;
 my $y2 = $y1+$obj->height;
 my @results = ($x1,$y1,$x2,$y2);
 return wantarray ? @results : \@results;
}


*Width  = \&width;
*Height = \&height;
*X      = \&rootx;
*Y      = \&rooty;

1;
__END__
