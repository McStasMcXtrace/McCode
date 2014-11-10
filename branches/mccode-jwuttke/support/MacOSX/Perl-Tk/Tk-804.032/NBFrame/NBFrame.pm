package Tk::NBFrame;

use vars qw($VERSION);
$VERSION = '4.004'; # $Id: //depot/Tkutf8/NBFrame/NBFrame.pm#4 $

use Tk qw($XS_VERSION);

use base  qw(Tk::Widget);

Construct Tk::Widget 'NBFrame';

bootstrap Tk::NBFrame;

sub Tk_cmd { \&Tk::nbframe }

Tk::Methods qw(activate add delete focus info geometryinfo identify
               move pagecget pageconfigure);

1;

