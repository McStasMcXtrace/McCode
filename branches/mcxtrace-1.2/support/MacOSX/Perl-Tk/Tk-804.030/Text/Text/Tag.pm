package Tk::Text::Tag;
require Tk::Text;

use overload '""' => \&name;


use vars qw($VERSION);
$VERSION = '4.004'; # $Id: //depot/Tkutf8/Text/Text/Tag.pm#4 $

sub _apply
{
 my $self = shift;
 my $meth = shift;
 $self->widget->tag($meth => $self->name,@_);
}

sub name
{
 return shift->[0];
}

sub widget
{
 return shift->[1];
}

BEGIN
{
 my $meth;
 foreach $meth (qw(cget configure bind add))
  {
   *{$meth} = sub { shift->_apply($meth,@_) }
  }
}

sub new
{
 my $class  = shift;
 my $widget = shift;
 my $name   = shift;
 my $obj    = bless [$name,$widget],$class;
 $obj->configure(@_) if (@_);
 return $obj;
}

1;
