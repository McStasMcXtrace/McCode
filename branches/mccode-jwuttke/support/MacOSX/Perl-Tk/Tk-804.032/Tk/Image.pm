# Copyright (c) 1995-2003 Nick Ing-Simmons. All rights reserved.
# This program is free software; you can redistribute it and/or
# modify it under the same terms as Perl itself.
package Tk::Image;

# This module does for images what Tk::Widget does for widgets:
# provides a base class for them to inherit from.
require DynaLoader;

use base qw(DynaLoader Tk); # but are they ?

use vars qw($VERSION);
$VERSION = '4.011'; # $Id: //depot/Tkutf8/Tk/Image.pm#11 $

sub new
{
 my $package = shift;
 my $widget  = shift;
 $package->InitClass($widget);
 my $leaf = $package->Tk_image;
 my $obj = $widget->Tk::image('create',$leaf,@_);
 $obj = $widget->_object($obj) unless (ref $obj);
 return bless $obj,$package;
}

sub Install
{
 # Dynamically loaded image types can install standard images here
 my ($class,$mw) = @_;
}

sub ClassInit
{
 # Carry out class bindings (or whatever)
 my ($package,$mw) = @_;
 return $package;
}

require Tk::Submethods;

Direct Tk::Submethods ('image' => [qw(delete width height inuse type)]);

sub Tk::Widget::imageNames
{
 my $w = shift;
 $w->image('names',@_);
}

sub Tk::Widget::imageTypes
{
 my $w = shift;
 map("\u$_",$w->image('types',@_));
}

sub Construct
{
 my ($base,$name) = @_;
 my $class = (caller(0))[0];

 # Hack for broken ->isa in perl5.6.0
 delete ${"$class\::"}{'::ISA::CACHE::'} if $] == 5.006;

 *{"Tk::Widget::$name"}  = sub { $class->new(@_) };
}

# This is here to prevent AUTOLOAD trying to find it.
sub DESTROY
{
 my $i = shift;
 # maybe do image delete ???
}


1;
