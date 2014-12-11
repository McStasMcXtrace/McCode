package Tk::Compound;
require Tk;
import  Tk qw($XS_VERSION);
require Tk::Image;

use vars qw($VERSION);
$VERSION = '4.004'; # $Id: //depot/Tkutf8/Compound/Compound.pm#4 $

use base  qw(Tk::Image);

Construct Tk::Image 'Compound';

bootstrap Tk::Compound;

sub Tk_image { 'compound' }

Tk::Methods('add');

sub new
{
 my $package = shift;
 my $widget  = shift;
 my $leaf = $package->Tk_image;
 $package->InitClass($widget);
 my $obj = $widget->image(create => $leaf,@_,-window => $widget);
 return bless($obj,$package);
}

BEGIN
 {
  foreach my $type (qw(line text image bitmap space))
   {
    my $meth = ucfirst($type);
    no strict qw 'refs';
    *{$meth} = sub { shift->add($type,@_) };
   }
 }

1;
__END__
