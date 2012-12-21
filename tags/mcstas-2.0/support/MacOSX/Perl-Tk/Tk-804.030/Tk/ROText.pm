# Copyright (c) 1995-2003 Nick Ing-Simmons. All rights reserved.
# This program is free software; you can redistribute it and/or
# modify it under the same terms as Perl itself.
package Tk::ROText;

use vars qw($VERSION);
#$VERSION = sprintf '4.%03d', q$Revision: #10 $ =~ /\D(\d+)\s*$/;
$VERSION = '4.011';

use Tk::Text;
use base  qw(Tk::Derived Tk::Text);

Construct Tk::Widget 'ROText';

sub clipEvents
{
 return qw[Copy];
}

sub ClassInit
{
 my ($class,$mw) = @_;
 my $val = $class->bindRdOnly($mw);
 my $cb  = $mw->bind($class,'<Next>');
 $mw->bind($class,'<space>',$cb) if (defined $cb);
 $cb  = $mw->bind($class,'<Prior>');
 $mw->bind($class,'<BackSpace>',$cb) if (defined $cb);
 $class->clipboardOperations($mw,'Copy');
 return $val;
}

sub Populate
{
 my($self,$args) = @_;
 $self->SUPER::Populate($args);
 my $m = $self->menu->entrycget($self->menu->index('Search'), '-menu');
 $m->delete($m->index('Replace'));
 $self->ConfigSpecs(-background => ['SELF'],
		    -foreground => ['SELF'],
		   );
}

sub Tk::Widget::ScrlROText { shift->Scrolled('ROText' => @_) }

1;

__END__

