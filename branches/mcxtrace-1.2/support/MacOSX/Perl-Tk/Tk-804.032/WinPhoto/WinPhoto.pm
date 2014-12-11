package Tk::WinPhoto;
require DynaLoader;

use vars qw($VERSION);
$VERSION = '4.005'; # $Id: //depot/Tkutf8/WinPhoto/WinPhoto.pm#4 $

use Tk qw($XS_VERSION);
require Tk::Image;
require Tk::Photo;

use base  qw(DynaLoader);

bootstrap Tk::WinPhoto;

1;

__END__

=head1 NAME

Tk::WinPhoto - Load a Photo image from a window

=for category Experimental Modules

=head1 SYNOPSIS

  use Tk;
  use Tk::WinPhoto;

  my $image = $mw->Photo(-format => 'Window', -data => oct($mw->id));
  $image->write($path_name, -format => 'BMP|PPM|XPM');


=head1 DESCRIPTION

This is an extension for Tk800.* which will load a Photo image
from a snapshot of an X window specified by the -data option.

The window must exist and be visible. Because the code allows
you to capture windows not owned by Tk it does not attempt to
enforce this. If you are capturing one of Tk's windows then
use C<$w-E<gt>update>.

If window is mapped, but obscured by other windows then what is captured is the
rectangle the window would occupy. This can be considered a feature.
For Tk-owned windows C<$w-E<gt>raise> can used to bring window forward.

Once the Photo is loaded it can be saved using
C<$image-E<gt>write(-format =E<gt> ...)> using any of formats which support
writing.

=head1 AUTHOR

Nick Ing-Simmons E<lt>nick@ni-s.u-net.comE<gt>

=cut

