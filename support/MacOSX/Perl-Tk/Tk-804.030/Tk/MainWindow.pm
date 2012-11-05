# Copyright (c) 1995-2003 Nick Ing-Simmons. All rights reserved.
# This program is free software; you can redistribute it and/or
# modify it under the same terms as Perl itself.
package Tk::MainWindow;
use base qw(Tk::Toplevel);
BEGIN { @MainWindow::ISA = 'Tk::MainWindow' }

use strict;

use vars qw($VERSION);
$VERSION = '4.015'; # was: sprintf '4.%03d', q$Revision: #12 $ =~ /\D(\d+)\s*$/;

use Tk::CmdLine;
use Tk qw(catch);
require Tk::Toplevel;

use Carp;

$| = 1;

my %Windows = ();

sub CreateArgs
{
 my ($class,$args) = @_;
 my $cmd = Tk::CmdLine->CreateArgs();
 my $key;
 foreach $key (keys %$cmd)
  {
   $args->{$key} = $cmd->{$key} unless exists $args->{$key};
  }
 my %result = $class->SUPER::CreateArgs(undef,$args);
 my $name = delete($args->{'-name'});
 unless (Tk::tainting)
  {
   $ENV{'DISPLAY'} = ':0' unless (exists $ENV{'DISPLAY'});
   $result{'-screen'} = $ENV{'DISPLAY'} unless exists $result{'-screen'};
  }
 return (-name => "\l$name",%result);
}

sub new
{
 my $package = shift;
 if (@_ > 0 && $_[0] =~ /:\d+(\.\d+)?$/)
  {
   carp "Usage $package->new(-screen => '$_[0]' ...)" if $^W;
   unshift(@_,'-screen');
  }
 croak('Odd number of args'."$package->new(" . join(',',@_) .')') if @_ % 2;
 my %args = @_;

 my $top = eval { bless Create($package->CreateArgs(\%args)), $package };
 croak($@ . "$package->new(" . join(',',@_) .')') if ($@);
 $top->apply_command_line;
 $top->InitBindings;
 $top->SetBindtags;
 $top->InitObject(\%args);
 eval { $top->configure(%args) };
 croak "$@" if ($@);
 if (($top->positionfrom||'') ne 'user' and ($top->sizefrom||'') ne 'user') {
     my $geometry = $top->optionGet(qw(geometry Geometry));
     if ($geometry) {
	 $top->geometry($geometry);
     }
 }
 $Windows{$top} = $top;
 return $top;
}

sub _Destroyed
{
 my $top = shift;
 $top->SUPER::_Destroyed;
 delete $Windows{$top};
}

sub InitBindings
{
 my $mw = shift;
 $mw->bind('all','<Tab>','focusNext');
 # <<LeftTab>> is named <<PrevWindow>> in Tcl/Tk
 $mw->eventAdd(qw[<<LeftTab>> <Shift-Tab>]);
 # This is needed for XFree86 systems
 catch { $mw->eventAdd(qw[<<LeftTab>> <ISO_Left_Tab>]) };
 # This seems to be correct on *some* HP systems.
 catch { $mw->eventAdd(qw[<<LeftTab>> <hpBackTab>]) };
 $mw->bind('all','<<LeftTab>>','focusPrev');
 if ($mw->windowingsystem eq 'x11')
  {
   $mw->eventAdd(qw[<<Cut>> <Control-Key-x> <Lock-Control-Key-X> <Key-F20> <Meta-Key-w>]);
   $mw->eventAdd(qw[<<Copy>> <Control-Key-c> <Lock-Control-Key-C> <Key-F16> <Control-Key-w>]);
   $mw->eventAdd(qw[<<Paste>> <Control-Key-v> <Lock-Control-Key-V> <Key-F18> <Control-Key-y>]);
   $mw->eventAdd(qw[<<PasteSelection>> <ButtonRelease-2>]);
   $mw->eventAdd(qw[<<Undo>> <Control-Key-z> <Key-Undo> <Key-F14>
                    <Control-Key-underscore>]);
   $mw->eventAdd(qw[<<Redo>> <Control-Key-y> <Shift-Key-Undo> <Key-F12> <Shift-Key-F14>]);
  }
 elsif ($mw->windowingsystem eq 'win32')
  {
   $mw->eventAdd(qw[<<Cut>> <Control-Key-x> <Shift-Key-Delete>]);
   $mw->eventAdd(qw[<<Copy>> <Control-Key-c> <Control-Key-Insert>]);
   $mw->eventAdd(qw[<<Paste>> <Control-Key-v> <Shift-Key-Insert>]);
   $mw->eventAdd(qw[<<Undo>> <Control-Key-z>]);
   $mw->eventAdd(qw[<<Redo>> <Control-Key-y>]);
  }
 elsif ($mw->windowingsystem eq 'aqua')
  {
   $mw->eventAdd(qw[<<Cut>> <Command-Key-x> <Key-F2>]);
   $mw->eventAdd(qw[<<Copy>> <Command-Key-c> <Key-F3>]);
   $mw->eventAdd(qw[<<Paste>> <Command-Key-v> <Key-F4>]);
   $mw->eventAdd(qw[<<PasteSelection>> <ButtonRelease-2>]);
   $mw->eventAdd(qw[<<Clear>> <Clear>]);
   $mw->eventAdd(qw[<<Undo>> <Command-Key-z>]);
   $mw->eventAdd(qw[<<Redo>> <Command-Key-y>]);
  }
 elsif ($mw->windowingsystem eq 'classic')
  {
   $mw->eventAdd(qw[<<Cut>> <Control-Key-x> <Key-F2>]);
   $mw->eventAdd(qw[<<Copy>> <Control-Key-c> <Key-F3>]);
   $mw->eventAdd(qw[<<Paste>> <Control-Key-v> <Key-F4>]);
   $mw->eventAdd(qw[<<PasteSelection>> <ButtonRelease-2>]);
   $mw->eventAdd(qw[<<Clear>> <Clear>]);
   $mw->eventAdd(qw[<<Undo>> <Control-Key-z> <Key-F1>]);
   $mw->eventAdd(qw[<<Redo>> <Control-Key-Z>]);
  }

 # FIXME - Should these move to Menubutton ?
 my $c = ($Tk::platform eq 'unix') ? 'all' : 'Tk::Menubutton';
 $mw->bind($c,'<Alt-KeyPress>',['TraverseToMenu',Tk::Ev('K')]);
 $mw->bind($c,'<F10>','FirstMenu');
}

sub Existing
{
 my @Windows;
 foreach my $name (keys %Windows)
  {
   my $obj = $Windows{$name};
   if (Tk::Exists($obj))
    {
     push(@Windows,$obj);
    }
   else
    {
     delete $Windows{$name};
    }
  }
 return @Windows;
}

END
{
 if (Tk::IsParentProcess())
  {
   foreach my $top (values %Windows)
    {
     if ($top->IsWidget)
      {
       # Tk data structuctures are still in place
       # this can occur if non-callback perl code did a 'die'.
       # It will also handle some cases of non-Tk 'exit' being called
       # Destroy this mainwindow and hence is descendants ...
       $top->destroy;
      }
    }
  }
}

sub CmdLine { return shift->command }

sub WMSaveYourself
{
 my $mw  = shift;
 my @args = @{$mw->command};
# warn 'preWMSaveYourself:'.join(' ',@args)."\n";
 @args = ($0) unless (@args);
 my $i = 1;
 while ($i < @args)
  {
   if ($args[$i] eq '-iconic')
    {
     splice(@args,$i,1);
    }
   elsif ($args[$i] =~ /^-(geometry|iconposition)$/)
    {
     splice(@args,$i,2);
    }
  }

 my @ip = $mw->wm('iconposition');
# print 'ip ',join(',',@ip),"\n";
 my $icon = $mw->iconwindow;
 if (defined($icon))
  {
   @ip = $icon->geometry =~ /\d+x\d+([+-]\d+)([+-]\d+)/;
  }
 splice(@args,1,0,'-iconposition' => join(',',@ip)) if (@ip == 2);

 splice(@args,1,0,'-iconic') if ($mw->state() eq 'iconic');

 splice(@args,1,0,'-geometry' => $mw->geometry);
# warn 'postWMSaveYourself:'.join(' ',@args)."\n";
 $mw->command([@args]);
}

1;

__END__

