package Tk::DragDrop::SunConst;
require Exporter;

use vars qw($VERSION);
$VERSION = '4.004'; # $Id: //depot/Tkutf8/DragDrop/DragDrop/SunConst.pm#4 $

use base  qw(Exporter);

@EXPORT = qw(_enter _leave _motion
             ENTERLEAVE MOTION DEFAULT_SITE
             MOVE_FLAG ACK_FLAG TRANSIENT_FLAG FORWARDED_FLAG
            );

# Event types
sub _enter  () {7};
sub _leave  () {8};
sub _motion () {6};

# Site flags

sub ENTERLEAVE   ()  {1<<0}
sub MOTION       ()  {1<<1}
sub DEFAULT_SITE ()  {1<<2}

# Trigger flags
sub MOVE_FLAG      () {1<<0}
sub ACK_FLAG       () {1<<1}
sub TRANSIENT_FLAG () {1<<2}
sub FORWARDED_FLAG () {1<<3}

1;

__END__

