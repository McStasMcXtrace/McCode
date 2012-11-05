package # hide from CPAN indexer
    Tk::ImageBack;
require DynaLoader;

use vars qw($VERSION);
$VERSION = '4.005'; # $Id: //depot/Tkutf8/Extensions/ImageBack/ImageBack.pm#4 $

use Tk qw($XS_VERSION);
use base  qw(DynaLoader);

bootstrap Tk::ImageBack;

1;
__END__
