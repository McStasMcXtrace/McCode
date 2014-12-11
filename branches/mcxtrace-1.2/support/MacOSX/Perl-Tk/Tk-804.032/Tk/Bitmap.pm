package Tk::Bitmap;
require Tk;
require Tk::Image;
use vars qw($VERSION);
$VERSION = '4.004'; # $Id: //depot/Tkutf8/Tk/Bitmap.pm#4 $
use base  qw(Tk::Image);
Construct Tk::Image 'Bitmap';
sub Tk_image { 'bitmap' }
1;
__END__
