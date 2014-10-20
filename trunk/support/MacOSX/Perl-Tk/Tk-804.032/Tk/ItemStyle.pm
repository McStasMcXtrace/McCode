package Tk::ItemStyle;

use vars qw($VERSION);
$VERSION = '4.004'; # $Id: //depot/Tkutf8/Tk/ItemStyle.pm#4 $

require Tk;
use base  qw(Tk);
require Tk::Widget;
Construct Tk::Widget 'ItemStyle';

Tk::Methods ('delete');

sub new
{
 my $package = shift;
 my $widget  = shift;
 my $type    = shift;
 my %args    = @_;
 $args{'-refwindow'} = $widget unless exists $args{'-refwindow'};
 $package->InitClass($widget);
 my $obj = $widget->itemstyle($type, %args);
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

1;
