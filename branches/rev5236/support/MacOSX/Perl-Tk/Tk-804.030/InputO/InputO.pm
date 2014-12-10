package Tk::InputO;

use vars qw($VERSION);
$VERSION = '4.004'; # $Id: //depot/Tkutf8/InputO/InputO.pm#4 $

use Tk qw($XS_VERSION);

use base  qw(Tk::Widget);

Construct Tk::Widget 'InputO';

bootstrap Tk::InputO;

sub Tk_cmd { \&Tk::inputo }

#Tk::Methods qw(add ...);

1;

