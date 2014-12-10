package Tk::Labelframe;
use strict;

use vars qw($VERSION);
$VERSION = '4.003'; # sprintf '4.%03d', q$Revision: #2 $ =~ /#(\d+)/;

# New widget which is a kind of Frame with a label ...

use base qw(Tk::Frame);

Construct Tk::Widget 'Labelframe';

sub Tk_cmd { \&Tk::labelframe }

1;
__END__
