package Tk::Configure;
use vars qw($VERSION);
$VERSION = '4.009'; # $Id: //depot/Tkutf8/Tk/Configure.pm#8 $

use Carp;


# Class that handles cget/configure for options that
# need translating from public form
# e.g. $cw->configure(-label => 'fred')
# into $cw->subwiget('label')->configure(-text => 'fred')
# Should probably do something clever with regexp's here


sub new
{
 my ($class,@args) = @_;
 unshift(@args,'configure','cget') if (@args < 3);
 return bless \@args,$class;
}

sub cget
{
 croak('Wrong number of args to cget') unless (@_ == 2);
 my ($alias,$key) = @_;
 my ($set,$get,$widget,@args) = @$alias;
 $widget->$get(@args);
}

sub configure
{
 my $alias = shift;
 shift if (@_);
 my ($set,$get,$widget,@args) = @$alias;
 if (wantarray)
  {
   my @results;
   eval { @results = $widget->$set(@args,@_) };
   croak($@) if $@;
   return @results;
  }
 else
  {
   my $results;
   eval { $results = $widget->$set(@args,@_) };
   croak($@) if $@;
   return $results;
  }
}

*TIESCALAR = \&new;
*TIEHASH   = \&new;

sub FETCH
{
 my $alias = shift;
 my ($set,$get,$widget,@args) = @$alias;
 return $widget->$get(@args,@_);
}

sub STORE
{
 my $alias = shift;
 my ($set,$get,$widget,@args) = @$alias;
 $widget->$set(@args,@_);
}

1;
