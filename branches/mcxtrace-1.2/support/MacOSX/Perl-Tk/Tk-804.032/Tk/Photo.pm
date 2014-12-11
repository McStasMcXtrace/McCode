package Tk::Photo;

use vars qw($VERSION);
$VERSION = '4.006'; # sprintf '4.%03d', 4+q$Revision: #4 $ =~ /\D(\d+)\s*$/;

use Tk qw($XS_VERSION);

use base  qw(Tk::Image);

Construct Tk::Image 'Photo';

sub Tk_image { 'photo' }

Tk::Methods('blank','copy','data','formats','get','put','read',
            'redither','transparency','write');

use Tk::Submethods (
    'transparency'  => [qw/get set/],
);

1;
__END__
