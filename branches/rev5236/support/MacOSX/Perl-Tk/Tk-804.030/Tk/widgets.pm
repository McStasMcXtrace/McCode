package Tk::widgets;
use Carp;

use vars qw($VERSION);
$VERSION = '4.005'; # $Id: //depot/Tkutf8/Tk/widgets.pm#4 $

sub import
{
 my $class = shift;
 foreach (@_)
  {
   local $SIG{__DIE__} = \&Carp::croak;
   # carp "$_ already loaded" if (exists $INC{"Tk/$_.pm"});
   require "Tk/$_.pm";
  }
}

1;
__END__

