# Copyright (c) 1995-2004 Nick Ing-Simmons. All rights reserved.
# This program is free software; you can redistribute it and/or
# modify it under the same terms as Perl itself.
package Tk::Widget;
use vars qw($VERSION @DefaultMenuLabels);
$VERSION = '4.035'; # was: sprintf '4.%03d', q$Revision: #30 $ =~ /\D(\d+)\s*$/;

require Tk;
use AutoLoader;
use strict;
use Carp;
use base qw(DynaLoader Tk);

# stubs for 'autoloaded' widget classes
sub Button;
sub Canvas;
sub Checkbutton;
sub Entry;
sub Frame;
sub Label;
sub Labelframe;
sub Listbox;
sub Menu;
sub Menubutton;
sub Message;
sub Panedwindow;
sub Radiobutton;
sub Scale;
sub Scrollbar;
sub Spinbox;
sub Text;
sub Toplevel;

sub Pixmap;
sub Bitmap;
sub Photo;

sub ScrlListbox;
sub Optionmenu;

sub import
{
 my $package = shift;
 carp 'use Tk::Widget () to pre-load widgets is deprecated' if (@_);
 my $need;
 foreach $need (@_)
  {
   unless (defined &{$need})
    {
     require "Tk/${need}.pm";
    }
   croak "Cannot locate $need" unless (defined &{$need});
  }
}

@DefaultMenuLabels = qw[~File ~Help];

# Some tidy-ness functions for winfo stuff

sub True  { 1 }
sub False { 0 }

use Tk::Submethods( 'grab' =>  [qw(current status release -global)],
                    'focus' => [qw(-force -lastfor)],
                    'pack'  => [qw(configure forget info propagate slaves)],
                    'grid'  => [qw(bbox columnconfigure configure forget info location propagate rowconfigure size slaves)],
                    'form'  => [qw(check configure forget grid info slaves)],
                    'event' => [qw(add delete generate info)],
                    'place' => [qw(configure forget info slaves)],
                    'wm'    => [qw(capture release)],
                    'font'  => [qw(actual configure create delete families measure metrics names subfonts)]
                  );

BEGIN  {
 # FIXME - these don't work in the compiler
 *IsMenu         = \&False;
 *IsMenubutton   = \&False;
 *configure_self = \&Tk::configure;
 *cget_self      = \&Tk::cget;
}



Direct Tk::Submethods (
  'winfo' => [qw(cells class colormapfull depth exists
               geometry height id ismapped manager name parent reqheight
               reqwidth rootx rooty screen screencells screendepth screenheight
               screenmmheight screenmmwidth  screenvisual screenwidth visual
               visualsavailable  vrootheight viewable vrootwidth vrootx vrooty
               width x y toplevel children pixels pointerx pointery pointerxy
               server fpixels rgb )],
   'tk'   => [qw(appname caret scaling useinputmethods windowingsystem)]);


sub DESTROY
{
 my $w = shift;
 $w->destroy if ($w->IsWidget);
}

sub Install
{
 # Dynamically loaded widgets add their core commands
 # to the Tk base class here
 my ($package,$mw) = @_;
}

sub ClassInit
{
 # Carry out class bindings (or whatever)
 my ($package,$mw) = @_;
 return $package;
}

sub CreateOptions
{
 return ();
}

sub CreateArgs
{
 my ($package,$parent,$args) = @_;
 # Remove from hash %$args any configure-like
 # options which only apply at create time (e.g. -colormap for Frame),
 # or which may as well be applied right away
 # return these as a list of -key => value pairs
 # Augment same hash with default values for missing mandatory options,
 # allthough this can be done later in InitObject.

 # Honour -class => if present, we have hacked Tk_ConfigureWidget to
 # allow -class to be passed to any widget.
 my @result = ();
 my $class = delete $args->{'-class'};
 ($class) = $package =~ /([A-Z][A-Z0-9_]*)$/i unless (defined $class);
 @result = (-class => "\u$class") if (defined $class);
 foreach my $opt ($package->CreateOptions)
  {
   push(@result, $opt => delete $args->{$opt}) if exists $args->{$opt};
  }
 return @result;
}

sub InitObject
{
 my ($obj,$args) = @_;
 # per object initialization, for example populating
 # with sub-widgets, adding a few object bindings to augment
 # inherited class bindings, changing binding tags.
 # Also another chance to mess with %$args before configure...
}

sub SetBindtags
{
 my ($obj) = @_;
 $obj->bindtags([ref($obj),$obj,$obj->toplevel,'all']);
}

sub new
{
 local $SIG{'__DIE__'} = \&Carp::croak;
 my $package = shift;
 my $parent  = shift;
 $package->InitClass($parent);
 $parent->BackTrace("Odd number of args to $package->new(...)") unless ((@_ % 2) == 0);
 my %args  = @_;
 my @args  = $package->CreateArgs($parent,\%args);
 my $cmd   = $package->Tk_cmd;
 my $pname = $parent->PathName;
 $pname    = '' if ($pname eq '.');
 my $leaf  = delete $args{'Name'};
 if (defined $leaf)
  {
   $leaf =~ s/[^a-z0-9_#]+/_/ig;
   $leaf = lcfirst($leaf);
  }
 else
  {
   ($leaf) = "\L$package" =~ /([a-z][a-z0-9_]*)$/;
  }
 my $lname  = $pname . '.' . $leaf;
 # create a hash indexed by leaf name to speed up
 # creation of a lot of sub-widgets of the same type
 # e.g. entries in Table
 my $nhash = $parent->TkHash('_names_');
 $nhash->{$leaf} = 0 unless (exists $nhash->{$leaf});
 while (defined ($parent->Widget($lname)))
  {
   $lname = $pname . '.' . $leaf . ++$nhash->{$leaf};
  }
 my $obj = eval { &$cmd($parent, $lname, @args) };
 confess $@ if $@;
 unless (ref $obj)
  {
   die "No value from $cmd $lname" unless defined $obj;
   warn "$cmd '$lname' returned '$obj'" unless $obj eq $lname;
   $obj = $parent->Widget($lname = $obj);
   die "$obj from $lname" unless ref $obj;
  }
 bless $obj,$package;
 $obj->SetBindtags;
 my $notice = $parent->can('NoticeChild');
 $parent->$notice($obj,\%args) if $notice;
 $obj->InitObject(\%args);
# ASkludge(\%args,1);
 $obj->configure(%args) if (%args);
# ASkludge(\%args,0);
 return $obj;
}

sub DelegateFor
{
 my ($w,$method) = @_;
 while(exists $w->{'Delegates'})
  {
   my $delegate = $w->{'Delegates'};
   my $widget = $delegate->{$method};
   $widget = $delegate->{DEFAULT} unless (defined $widget);
   $widget = $w->Subwidget($widget) if (defined $widget && !ref $widget);
   last unless (defined $widget);
   last if $widget == $w;
   $w = $widget;
  }
 return $w;
}

sub Delegates
{
 my $cw = shift;
 my $specs = $cw->TkHash('Delegates');
 while (@_)
  {
   my $key = shift;
   my $val = shift;
   $specs->{$key} = $val;
  }
 return $specs;
}

sub Construct
{
 my ($base,$name) = @_;
 my $class = (caller(0))[0];
 no strict 'refs';

 # Hack for broken ->isa in perl5.6.0
 delete ${"$class\::"}{'::ISA::CACHE::'} if $] == 5.006;

 # Pre ->isa scheme
 *{$base.'::Is'.$name}  = \&False;
 *{$class.'::Is'.$name} = \&True;

 # DelegateFor  trickyness is to allow Frames and other derived things
 # to force creation in a delegate e.g. a ScrlText with embeded windows
 # need those windows to be children of the Text to get clipping right
 # and not of the Frame which contains the Text and the scrollbars.
 *{$base.'::'."$name"}  = sub { $class->new(shift->DelegateFor('Construct'),@_) };
}

sub IS
{
 return (defined $_[1]) && $_[0] == $_[1];
}

sub _AutoloadTkWidget
{
 my ($self,$method) = @_;
 my $what = "Tk::Widget::$method";
 unless (defined &$what)
  {
   require "Tk/$method.pm";
  }
 return $what;
}

# require UNIVERSAL; don't load .pm use XS code from perl core though

sub AUTOLOAD
{
 # Take a copy into a 'my' variable so we can recurse
 my $what = $Tk::Widget::AUTOLOAD;
 my $save = $@;
 my $name;
 # warn "AUTOLOAD $what ".(ref($_[0]) || $_[0])."\n";
 # Braces used to preserve $1 et al.
 {
  my ($pkg,$func) = $what =~ /(.*)::([^:]+)$/;
  confess("Attempt to load '$what'") unless defined($pkg) && $func =~ /^[\w:]+$/;
  $pkg =~ s#::#/#g;
  if (defined($name=$INC{"$pkg.pm"}))
   {
    $name =~ s#^(.*)$pkg\.pm$#$1auto/$pkg/$func.al#;
   }
  else
   {
    $name = "auto/$what.al";
    $name =~ s#::#/#g;
   }
 }
 # This may fail, catch error and prevent user's __DIE__ handler
 # from triggering as well...
 eval {local $SIG{'__DIE__'}; require $name};
 if ($@)
  {
   croak $@ unless ($@ =~ /Can't locate\s+(?:file\s+)?'?\Q$name\E'?/);
   my($package,$method) = ($what =~ /^(.*)::([^:]*)$/);
   if (ref $_[0] && !$_[0]->can($method)
       && $_[0]->can('Delegate')
       && $method !~ /^(ConfigSpecs|Delegates)/ )
    {
     my $delegate = $_[0]->Delegates;
     if (%$delegate || tied %$delegate)
      {
       my $widget = $delegate->{$method};
       $widget = $delegate->{DEFAULT} unless (defined $widget);
       if (defined $widget)
        {
         my $subwidget = (ref $widget) ? $widget : $_[0]->Subwidget($widget);
         if (defined $subwidget)
          {
           no strict 'refs';
           # print "AUTOLOAD: $what\n";
           *{$what} = sub { shift->Delegate($method,@_) };
          }
         else
          {
           croak "No delegate subwidget '$widget' for $what";
          }
        }
      }
    }
   if (!defined(&$what) && ref($_[0]) && $method =~ /^[A-Z]\w+$/)
    {
     # Use ->can as ->isa is broken in perl5.6.0
     my $sub = UNIVERSAL::can($_[0],'_AutoloadTkWidget');
     if ($sub)
      {
       carp "Assuming 'require Tk::$method;'" unless $_[0]->can($method);
       $what = $_[0]->$sub($method)
      }
    }
  }
 $@ = $save;
 $DB::sub = $what; # Tell debugger what is going on...
 unless (defined &$what)
  {
   no strict 'refs';
   *{$what} = sub { croak("Failed to AUTOLOAD '$what'") };
  }
 goto &$what;
}

sub _Destroyed
{
 my $w = shift;
 my $a = delete $w->{'_Destroy_'};
 if (ref($a))
  {
   while (@$a)
    {
     my $ent = pop(@$a);
     if (ref $ent)
      {
       eval {local $SIG{'__DIE__'}; $ent->Call };
      }
     else
      {
       delete $w->{$ent};
      }
    }
  }
}

sub _OnDestroy
{
 my $w = shift;
 $w->{'_Destroy_'} = [] unless (exists $w->{'_Destroy_'});
 push(@{$w->{'_Destroy_'}},@_);
}

sub OnDestroy
{
 my $w = shift;
 $w->_OnDestroy(Tk::Callback->new(@_));
}

sub TkHash
{
 my ($w,$key) = @_;
 return $w->{$key} if exists $w->{$key};
 my $hash = $w->{$key} = {};
 $w->_OnDestroy($key);
 return $hash;
}

sub privateData
{
 my $w = shift;
 my $p = shift || caller;
 $w->{$p} ||= {};
}

my @image_types;
my %image_method;

sub ImageMethod
{
 shift if (@_ & 1);
 while (@_)
  {
   my ($name,$method) = splice(@_,0,2);
   push(@image_types,$name);
   $image_method{$name} = $method;
  }
}

sub Getimage
{
 my ($w, $name) = @_;
 my $mw = $w->MainWindow;
 croak "Usage \$widget->Getimage('name')" unless defined($name);
 my $images = ($mw->{'__Images__'} ||= {});

 return $images->{$name} if $images->{$name};

 ImageMethod(xpm => 'Pixmap',
    gif => 'Photo',
    ppm => 'Photo',
    xbm => 'Bitmap' ) unless @image_types;

 foreach my $type (@image_types)
  {
   my $method = $image_method{$type};
   my $file = Tk->findINC( "$name.$type" );
   next unless( $file && $method );
   my $sub = $w->can($method);
   unless (defined &$sub)
    {
     require Tk::widgets;
     Tk::widgets->import($method);
    }
   $images->{$name} = $w->$method( -file => $file );
   return $images->{$name};
  }

 # Try built-in bitmaps
 $images->{$name} = $w->Pixmap( -id => $name );
 return $images->{$name};
}

sub SaveGrabInfo
{
 my $w = shift;
 $Tk::oldGrab = $w->grabCurrent;
 if (defined $Tk::oldGrab)
  {
   $Tk::grabStatus = $Tk::oldGrab->grabStatus;
  }
}

sub grabSave
{
 my ($w) = @_;
 my $grab = $w->grabCurrent;
 return sub {} if (!defined $grab);
 my $method = ($grab->grabStatus eq 'global') ? 'grabGlobal' : 'grab';
 return sub { eval {local $SIG{'__DIE__'};  $grab->$method() } };
}

sub focusCurrent
{
 my ($w) = @_;
 $w->Tk::focus('-displayof');
}

sub focusSave
{
 my ($w) = @_;
 my $focus = $w->focusCurrent;
 return sub {} if (!defined $focus);
 return sub { eval {local $SIG{'__DIE__'};  $focus->focus } };
}

# This is supposed to replicate Tk::after behaviour,
# but does auto-cancel when widget is deleted.
require Tk::After;

sub afterCancel
{
 my ($w,$what) = @_;
 if (defined $what)
  {
   return $what->cancel if ref($what);
   carp "dubious cancel of $what" if 0 && $^W;
   $w->Tk::after('cancel' => $what);
  }
}

sub afterIdle
{
 my $w = shift;
 return Tk::After->new($w,'idle','once',@_);
}

sub afterInfo {
    my ($w, $id) = @_;
    if (defined $id) {
	return ($id->[4], $id->[2], $id->[3]);
    } else {
	return sort( keys %{$w->{_After_}} );
    }
}

sub after
{
 my $w = shift;
 my $t = shift;
 if (@_)
  {
   if ($t ne 'cancel')
    {
     require Tk::After;
     return Tk::After->new($w,$t,'once',@_)
    }
   while (@_)
    {
     my $what = shift;
     $w->afterCancel($what);
    }
  }
 else
  {
   $w->Tk::after($t);
  }
}

sub repeat
{
 require Tk::After;
 my $w = shift;
 my $t = shift;
 return Tk::After->new($w,$t,'repeat',@_);
}

sub FindMenu
{
 # default FindMenu is that there is no menu.
 return undef;
}

sub XEvent { shift->{'_XEvent_'} }

sub propertyRoot
{
 my $w = shift;
 return $w->property(@_,'root');
}

# atom, atomname, containing, interps, pathname
# don't work this way - there is no window arg
# So we pretend there was an call the C versions from Tk.xs

sub atom       { shift->InternAtom(@_)  }
sub atomname   { shift->GetAtomName(@_) }
sub containing { shift->Containing(@_)  }

# interps not done yet
# pathname not done yet

# walk and descendants adapted from Stephen's composite
# versions as they only use core features they can go here.
# hierachy is reversed in that descendants calls walk rather
# than vice versa as this avoids building a list.
# Walk should possibly be enhanced so allow early termination
# like '-prune' of find.

sub Walk
{
 # Traverse a widget hierarchy while executing a subroutine.
 my($cw, $proc, @args) = @_;
 my $subwidget;
 foreach $subwidget ($cw->children)
  {
   $subwidget->Walk($proc,@args);
   &$proc($subwidget, @args);
  }
} # end walk

sub Descendants
{
 # Return a list of widgets derived from a parent widget and all its
 # descendants of a particular class.
 # If class is not passed returns the entire widget hierarchy.

 my($widget, $class) = @_;
 my(@widget_tree)    = ();

 $widget->Walk(
               sub { my ($widget,$list,$class) = @_;
                     push(@$list, $widget) if  (!defined($class) or $class eq $widget->class);
                   },
               \@widget_tree, $class
              );
 return @widget_tree;
}

sub Palette
{
 my $w = shift->MainWindow;
 unless (exists $w->{_Palette_})
  {
   my %Palette = ();
   my $c = $w->Checkbutton();
   my $e = $w->Entry();
   my $s = $w->Scrollbar();
   $Palette{'activeBackground'}    = ($c->configure('-activebackground'))[3] ;
   $Palette{'activeForeground'}    = ($c->configure('-activeforeground'))[3];
   $Palette{'background'}          = ($c->configure('-background'))[3];
   $Palette{'disabledForeground'}  = ($c->configure('-disabledforeground'))[3];

   $Palette{'foreground'}          = ($c->configure('-foreground'))[3];
   $Palette{'highlightBackground'} = ($c->configure('-highlightbackground'))[3];
   $Palette{'highlightColor'}      = ($c->configure('-highlightcolor'))[3];
   $Palette{'insertBackground'}    = ($e->configure('-insertbackground'))[3];
   $Palette{'selectColor'}         = ($c->configure('-selectcolor'))[3];
   $Palette{'selectBackground'}    = ($e->configure('-selectbackground'))[3];
   $Palette{'selectForeground'}    = ($e->configure('-selectforeground'))[3];
   $Palette{'troughColor'}         = ($s->configure('-troughcolor'))[3];
   $c->destroy;
   $e->destroy;
   $s->destroy;
   $w->{_Palette_} = \%Palette;
  }
 return $w->{_Palette_};
}

# tk_setPalette --
# Changes the default color scheme for a Tk application by setting
# default colors in the option database and by modifying all of the
# color options for existing widgets that have the default value.
#
# Arguments:
# The arguments consist of either a single color name, which
# will be used as the new background color (all other colors will
# be computed from this) or an even number of values consisting of
# option names and values. The name for an option is the one used
# for the option database, such as activeForeground, not -activeforeground.
# Additional special option names are:
#   priority: set the priority for the option database entries, see Tk::option
sub setPalette
{
 my $w = shift->MainWindow;
 # Just return on monochrome displays, otherwise errors will occur
 return if $w->depth == 1;
 my %new = (@_ == 1) ? (background => $_[0]) : @_;
 my $priority = delete($new{'priority'}) || 'widgetDefault';

 # Create an array that has the complete new palette. If some colors
 # aren't specified, compute them from other colors that are specified.

 die 'must specify a background color' if (!exists $new{background});
 my @bg = $w->rgb($new{'background'});

 if (!exists $new{foreground})
  {
   # Note that the range of each value in the triple returned by
   # [winfo rgb] is 0-65535, and your eyes are more sensitive to
   # green than to red, and more to red than to blue.
   my($r,$g,$b) = @bg;
   if ($r+1.5*$g+0.5*$b > 100000)
    {
     $new{'foreground'} = 'black';
    }
   else
    {
     $new{'foreground'} = 'white';
    }
  }
 my @fg = $w->rgb($new{'foreground'});
 my $darkerBg = sprintf('#%02x%02x%02x',9*$bg[0]/2560,9*$bg[1]/2560,9*$bg[2]/2560);
 foreach my $i ('activeForeground','insertBackground','selectForeground','highlightColor')
  {
   $new{$i} = $new{'foreground'} unless (exists $new{$i});
  }
 unless (exists $new{'disabledForeground'})
  {
   $new{'disabledForeground'} = sprintf('#%02x%02x%02x',(3*$bg[0]+$fg[0])/1024,(3*$bg[1]+$fg[1])/1024,(3*$bg[2]+$fg[2])/1024);
  }
 $new{'highlightBackground'} = $new{'background'} unless (exists $new{'highlightBackground'});

 unless (exists $new{'activeBackground'})
  {
   my @light;
   # Pick a default active background that is lighter than the
   # normal background. To do this, round each color component
   # up by 15% or 1/3 of the way to full white, whichever is
   # greater.
   foreach my $i (0, 1, 2)
    {
     $light[$i] = $bg[$i]/256;
     my $inc1 = $light[$i]*15/100;
     my $inc2 = (255-$light[$i])/3;
     if ($inc1 > $inc2)
      {
       $light[$i] += $inc1
      }
     else
      {
       $light[$i] += $inc2
      }
     $light[$i] = 255 if ($light[$i] > 255);
    }
   $new{'activeBackground'} = sprintf('#%02x%02x%02x',@light);
  }
 $new{'selectBackground'} = $darkerBg unless (exists $new{'selectBackground'});
 $new{'troughColor'} = $darkerBg unless (exists $new{'troughColor'});
 $new{'selectColor'} = '#b03060' unless (exists $new{'selectColor'});

 # Before doing this, make sure that the Tk::Palette variable holds
 # the default values of all options, so that tkRecolorTree can
 # be sure to only change options that have their default values.
 # If the variable exists, then it is already correct (it was created
 # the last time this procedure was invoked). If the variable
 # doesn't exist, fill it in using the defaults from a few widgets.
 my $Palette = $w->Palette;

 # let's make one of each of the widgets so we know what the 
 # defaults are currently for this platform.
 $Tk::___tk_set_palette = $w->Toplevel(Name => "___tk_set_palette");
 $Tk::___tk_set_palette->withdraw;
 foreach my $q (qw(Button Canvas Checkbutton Entry Frame Label Labelframe
		   Listbox Menubutton Menu Message Radiobutton Scale Scrollbar
		   Spinbox Text
		 ))
  {     
   $Tk::___tk_set_palette->Component($q, $q);
  }

 # Walk the widget hierarchy, recoloring all existing windows.
 my $res = $w->RecolorTree(\%new);
 if ($res->{addOptionDB})
  {
   for (@{ $res->{addOptionDB} })
    {
     $w->optionAdd(@$_);
    }
  }

 $Tk::___tk_set_palette->destroy;

 # Change the option database so that future windows will get the
 # same colors.
 foreach my $option (keys %new)
  {
   $w->option('add',"*$option",$new{$option},$priority);
   # Save the options in the global variable Tk::Palette, for use the
   # next time we change the options.
   $Palette->{$option} = $new{$option};
  }
}

# tkRecolorTree --
# This procedure changes the colors in a window and all of its
# descendants, according to information provided by the colors
# argument. It only modifies colors that have their default values
# as specified by the Tk::Palette variable.
#
# Arguments:
# w - The name of a window. This window and all its
# descendants are recolored.
# colors - The name of an array variable in the caller,
# which contains color information. Each element
# is named after a widget configuration option, and
# each value is the value for that option.
sub RecolorTree
{
 my ($w,$colors) = @_;
 local ($@);
 my @addOptionDB;
 my $prototype = (defined $Tk::___tk_set_palette ? $Tk::___tk_set_palette->Subwidget($w->Class) || undef : undef);
 foreach my $dbOption (keys %$colors)
  {
   my $option = "-\L$dbOption";
   my $class = ucfirst($dbOption);
   my @value;
   eval {local $SIG{'__DIE__'}; @value = $w->configure($option) };
   if (@value)
    {
     # if the option database has a preference for this
     # dbOption, then use it, otherwise use the defaults
     # for the widget.
     my $defaultcolor = $w->optionGet($dbOption, $class);
     no warnings 'uninitialized';
     if ($defaultcolor eq '' ||
	 ($prototype && $prototype->cget($option) ne $defaultcolor))
      {
       $defaultcolor = $value[3];
      }
     if ($defaultcolor ne '')
      {
       $defaultcolor = join ',', $w->rgb($defaultcolor);
      }
     my $chosencolor = $value[4];
     if ($chosencolor ne '')
      {
       $chosencolor = join ',', $w->rgb($chosencolor);
      }
     if ($defaultcolor eq $chosencolor)
      {
       # Change the option database so that future windows will get
       # the same colors.
       push @addOptionDB, ['*'.$w->Class.".$dbOption", $colors->{$dbOption}, 60];
       $w->configure($option,$colors->{$dbOption});
      }
    }
  }
 foreach my $child ($w->children)
  {
   my $res = $child->RecolorTree($colors);
   if ($res->{addOptionDB})
    {
     push @addOptionDB, @{ $res->{addOptionDB} };
    }
  }
 return { addOptionDB => \@addOptionDB };
}
# tkDarken --
# Given a color name, computes a new color value that darkens (or
# brightens) the given color by a given percent.
#
# Arguments:
# color - Name of starting color.
# perecent - Integer telling how much to brighten or darken as a
# percent: 50 means darken by 50%, 110 means brighten
# by 10%.
sub Darken
{
 my ($w,$color,$percent) = @_;
 my @l = $w->rgb($color);
 my $red = $l[0]/256;
 my $green = $l[1]/256;
 my $blue = $l[2]/256;
 $red = int($red*$percent/100);
 $red = 255 if ($red > 255);
 $green = int($green*$percent/100);
 $green = 255 if ($green > 255);
 $blue = int($blue*$percent/100);
 $blue = 255 if ($blue > 255);
 sprintf('#%02x%02x%02x',$red,$green,$blue)
}
# tk_bisque --
# Reset the Tk color palette to the old "bisque" colors.
#
# Arguments:
# None.
sub bisque
{
 shift->setPalette('activeBackground' => '#e6ceb1',
               'activeForeground' => 'black',
               'background' => '#ffe4c4',
               'disabledForeground' => '#b0b0b0',
               'foreground' => 'black',
               'highlightBackground' => '#ffe4c4',
               'highlightColor' => 'black',
               'insertBackground' => 'black',
               'selectColor' => '#b03060',
               'selectBackground' => '#e6ceb1',
               'selectForeground' => 'black',
               'troughColor' => '#cdb79e'
              );
}

sub PrintConfig
{
 require Tk::Pretty;
 my ($w) = (@_);
 my $c;
 foreach $c ($w->configure)
  {
   print Tk::Pretty::Pretty(@$c),"\n";
  }
}

sub BusyRecurse
{
 my ($restore,$w,$cursor,$recurse,$top) = @_;
 my $c = $w->cget('-cursor');
 my @tags = $w->bindtags;
 if ($top || defined($c) || $w->isa('Tk::Toplevel'))
  {
   push(@$restore, sub { return unless Tk::Exists($w); $w->configure(-cursor => $c); $w->bindtags(\@tags) });
   $w->configure(-cursor => $cursor);
  }
 else
  {
   push(@$restore, sub { return unless Tk::Exists($w); $w->bindtags(\@tags) });
  }
 $w->bindtags(['Busy',@tags]);
 if ($recurse)
  {
   foreach my $child ($w->children)
    {
     BusyRecurse($restore,$child,$cursor,1,0);
    }
  }
 return $restore;
}

sub Busy
{
 my ($w,@args) = @_;
 return unless $w->viewable;
 my($sub, %args);
 for(my $i=0; $i<=$#args; $i++)
  {
   if (ref $args[$i] eq 'CODE')
    {
     if (defined $sub)
      {
       croak "Multiple code definitions not allowed in Tk::Widget::Busy";
      }
     $sub = $args[$i];
    }
   else
    {
     $args{$args[$i]} = $args[$i+1]; $i++;
    }
  }
 my $cursor  = delete $args{'-cursor'};
 my $recurse = delete $args{'-recurse'};
 $cursor  = 'watch' unless defined $cursor;
 unless (exists $w->{'Busy'})
  {
   my @old = ($w->grabSave);
   my $key;
   my @config;
   foreach $key (keys %args)
    {
     push(@config,$key => $w->Tk::cget($key));
    }
   if (@config)
    {
     push(@old, sub { $w->Tk::configure(@config) });
     $w->Tk::configure(%args);
    }
   unless ($w->Tk::bind('Busy'))
    {
     $w->Tk::bind('Busy','<Any-KeyPress>',[_busy => 1]);
     $w->Tk::bind('Busy','<Any-KeyRelease>',[_busy => 0]);
     $w->Tk::bind('Busy','<Any-ButtonPress>',[_busy => 1]);
     $w->Tk::bind('Busy','<Any-ButtonRelease>',[_busy => 0]);
     $w->Tk::bind('Busy','<Any-Motion>',[_busy => 0]);
    }
   $w->{'Busy'} = BusyRecurse(\@old,$w,$cursor,$recurse,1);
  }
 my $g = $w->grabCurrent;
 if (defined $g)
  {
   # warn "$g has the grab";
   $g->grabRelease;
  }
 $w->update;
 if (Tk::Exists($w))
  {
   eval {local $SIG{'__DIE__'};  $w->grab };
   $w->update;
  }
 if ($sub && Tk::Exists($w))
  {
   eval { $sub->() };
   my $err = $@;
   $w->Unbusy(-recurse => $recurse);
   die $err if $err;
  }
}

sub _busy
{
 my ($w,$f) = @_;
 $w->bell if $f;
 $w->break;
}

sub Unbusy
{
 my ($w) = @_;
 $w->update;
 $w->grabRelease if Tk::Exists($w);
 my $old = delete $w->{'Busy'};
 if (defined $old)
  {
   local $SIG{'__DIE__'};
   eval { &{pop(@$old)} } while (@$old);
  }
 $w->update if Tk::Exists($w);
}

sub waitVisibility
{
 my ($w) = shift;
 $w->tkwait('visibility',$w);
}

sub waitVariable
{
 my ($w) = shift;
 $w->tkwait('variable',@_);
}

sub waitWindow
{
 my ($w) = shift;
 $w->tkwait('window',$w);
}

sub EventWidget
{
 my ($w) = @_;
 return $w->{'_EventWidget_'};
}

sub Popwidget
{
 my ($ew,$method,$w,@args) = @_;
 $w->{'_EventWidget_'} = $ew;
 $w->$method(@args);
}

sub ColorOptions
{
 my ($w,$args) = @_;
 my $opt;
 $args = {} unless (defined $args);
 foreach $opt (qw(-foreground -background -disabledforeground
                  -activebackground -activeforeground
              ))
  {
   $args->{$opt} = $w->cget($opt) unless (exists $args->{$opt})
  }
 return (wantarray) ? %$args : $args;
}

sub XscrollBind
{
 my ($mw,$class) = @_;
 $mw->bind($class,'<Left>',         ['xview','scroll',-1,'units']);
 $mw->bind($class,'<Control-Left>', ['xview','scroll',-1,'pages']);
 $mw->bind($class,'<Control-Prior>',['xview','scroll',-1,'pages']);
 $mw->bind($class,'<Right>',        ['xview','scroll',1,'units']);
 $mw->bind($class,'<Control-Right>',['xview','scroll',1,'pages']);
 $mw->bind($class,'<Control-Next>', ['xview','scroll',1,'pages']);

 $mw->bind($class,'<Home>',         ['xview','moveto',0]);
 $mw->bind($class,'<End>',          ['xview','moveto',1]);
 $mw->XMouseWheelBind($class);
}

sub PriorNextBind
{
 my ($mw,$class) = @_;
 $mw->bind($class,'<Next>',     ['yview','scroll',1,'pages']);
 $mw->bind($class,'<Prior>',    ['yview','scroll',-1,'pages']);
}

sub XMouseWheelBind
{
 my ($mw,$class) = @_;
 # <4> and <5> are how mousewheel looks on X
 # <4> and <5> are how mousewheel looks on X
 $mw->bind($class,'<Shift-4>',      ['xview','scroll',-1,'units']);
 $mw->bind($class,'<Shift-5>',      ['xview','scroll',1,'units']);
 $mw->bind($class,'<Button-6>',     ['xview','scroll',-1,'units']);
 $mw->bind($class,'<Button-7>',     ['xview','scroll',1,'units']);
}

sub YMouseWheelBind
{
 my ($mw,$class) = @_;
 # <4> and <5> are how mousewheel looks on X
 $mw->bind($class,'<4>',         ['yview','scroll',-1,'units']);
 $mw->bind($class,'<5>',         ['yview','scroll',1,'units']);
}

sub YscrollBind
{
 my ($mw,$class) = @_;
 $mw->PriorNextBind($class);
 $mw->bind($class,'<Up>',       ['yview','scroll',-1,'units']);
 $mw->bind($class,'<Down>',     ['yview','scroll',1,'units']);
 $mw->YMouseWheelBind($class);
}

sub XYscrollBind
{
 my ($mw,$class) = @_;
 $mw->YscrollBind($class);
 $mw->XscrollBind($class);
 # <4> and <5> are how mousewheel looks on X
}

sub MouseWheelBind
{
 my($mw,$class) = @_;

 # The MouseWheel will typically only fire on Windows. However, one
 # could use the "event generate" command to produce MouseWheel
 # events on other platforms.

 $mw->Tk::bind($class, '<MouseWheel>',
	       [ sub { $_[0]->yview('scroll',-($_[1]/120)*3,'units') }, Tk::Ev("D")]);

 if ($Tk::platform eq 'unix')
  {
   # Support for mousewheels on Linux/Unix commonly comes through mapping
   # the wheel to the extended buttons.  If you have a mousewheel, find
   # Linux configuration info at:
   #   http://www.inria.fr/koala/colas/mouse-wheel-scroll/
   $mw->Tk::bind($class, '<4>',
		 sub { $_[0]->yview('scroll', -3, 'units')
			   unless $Tk::strictMotif;
		   });
   $mw->Tk::bind($class, '<5>',
		 sub { $_[0]->yview('scroll', 3, 'units')
			   unless $Tk::strictMotif;
		   });
  }
}

sub ScrlListbox
{
 my $parent = shift;
 return $parent->Scrolled('Listbox',-scrollbars => 'w', @_);
}

sub AddBindTag
{
 my ($w,$tag) = @_;
 my $t;
 my @tags = $w->bindtags;
 foreach $t (@tags)
  {
   return if $t eq $tag;
  }
 $w->bindtags([@tags,$tag]);
}

sub Callback
{
 my $w = shift;
 my $name = shift;
 my $cb = $w->cget($name);
 if (defined $cb)
  {
   return $cb->Call(@_) if (ref $cb);
   return $w->$cb(@_);
  }
 return (wantarray) ? () : undef;
}

sub packAdjust
{
# print 'packAdjust(',join(',',@_),")\n";
 require Tk::Adjuster;
 my ($w,%args) = @_;
 my $delay = delete($args{'-delay'});
 $delay = 1 unless (defined $delay);
 $w->pack(%args);
 %args = $w->packInfo;
 my $adj = Tk::Adjuster->new($args{'-in'},
            -widget => $w, -delay => $delay, -side => $args{'-side'});
 $adj->packed($w,%args);
 return $w;
}

sub gridAdjust
{
 require Tk::Adjuster;
 my ($w,%args) = @_;
 my $delay = delete($args{'-delay'});
 $delay = 1 unless (defined $delay);
 $w->grid(%args);
 %args = $w->gridInfo;
 my $adj = Tk::Adjuster->new($args{'-in'},-widget => $w, -delay => $delay);
 $adj->gridded($w,%args);
 return $w;
}

sub place
{
 local $SIG{'__DIE__'} = \&Carp::croak;
 my $w = shift;
 if (@_ && $_[0] =~ /^(?:configure|forget|info|slaves)$/x)
  {
   $w->Tk::place(@_);
  }
 else
  {
   # Two things going on here:
   # 1. Add configure on the front so that we can drop leading '-'
   $w->Tk::place('configure',@_);
   # 2. Return the widget rather than nothing
   return $w;
  }
}

sub pack
{
 local $SIG{'__DIE__'} = \&Carp::croak;
 my $w = shift;
 if (@_ && $_[0] =~ /^(?:configure|forget|info|propagate|slaves)$/x)
  {
   # maybe array/scalar context issue with slaves
   $w->Tk::pack(@_);
  }
 else
  {
   # Two things going on here:
   # 1. Add configure on the front so that we can drop leading '-'
   $w->Tk::pack('configure',@_);
   # 2. Return the widget rather than nothing
   return $w;
  }
}

sub grid
{
 local $SIG{'__DIE__'} = \&Carp::croak;
 my $w = shift;
 if (@_ && $_[0] =~ /^(?:bbox|columnconfigure|configure|forget|info|location|propagate|rowconfigure|size|slaves)$/x)
  {
   my $opt = shift;
   Tk::grid($opt,$w,@_);
  }
 else
  {
   # Two things going on here:
   # 1. Add configure on the front so that we can drop leading '-'
   Tk::grid('configure',$w,@_);
   # 2. Return the widget rather than nothing
   return $w;
  }
}

sub form
{
 local $SIG{'__DIE__'} = \&Carp::croak;
 my $w = shift;
 if (@_ && $_[0] =~ /^(?:configure|check|forget|grid|info|slaves)$/x)
  {
   $w->Tk::form(@_);
  }
 else
  {
   # Two things going on here:
   # 1. Add configure on the front so that we can drop leading '-'
   $w->Tk::form('configure',@_);
   # 2. Return the widget rather than nothing
   return $w;
  }
}

sub Scrolled
{
 my ($parent,$kind,%args) = @_;
 $kind = 'Pane' if $kind eq 'Frame';
 # Find args that are Frame create time args
 my @args = Tk::Frame->CreateArgs($parent,\%args);
 my $name = delete $args{'Name'};
 push(@args,'Name' => $name) if (defined $name);
 my $cw = $parent->Frame(@args);
 @args = ();
 # Now remove any args that Frame can handle
 foreach my $k ('-scrollbars',map($_->[0],$cw->configure))
  {
   push(@args,$k,delete($args{$k})) if (exists $args{$k})
  }
 # Anything else must be for target widget - pass at widget create time
 my $w  = $cw->$kind(%args);
 # Now re-set %args to be ones Frame can handle
 %args = @args;
 $cw->ConfigSpecs('-scrollbars' => ['METHOD','scrollbars','Scrollbars','se'],
                  '-background' => [$w,'background','Background'],
                  '-foreground' => [$w,'foreground','Foreground'],
                 );
 $cw->AddScrollbars($w);
 $cw->Default("\L$kind" => $w);
 $cw->Delegates('bind' => $w, 'bindtags' => $w, 'menu' => $w);
 $cw->ConfigDefault(\%args);
 $cw->configure(%args);
 return $cw;
}

sub Populate
{
 my ($cw,$args) = @_;
}

sub ForwardEvent
{
 my $self = shift;
 my $to   = shift;
 $to->PassEvent($self->XEvent);
}

# Save / Return abstract event type as in Tix.
sub EventType
{
 my $w = shift;
 $w->{'_EventType_'} = $_[0] if @_;
 return $w->{'_EventType_'};
}

sub PostPopupMenu
{
 my ($w, $X, $Y) = @_;
 if (@_ < 3)
  {
   my $e = $w->XEvent;
   $X = $e->X;
   $Y = $e->Y;
  }
 my $menu = $w->menu;
 $menu->Post($X,$Y) if defined $menu;
}

sub FillMenu
{
 my ($w,$menu,@labels) = @_;
 foreach my $lab (@labels)
  {
   my $method = $lab.'MenuItems';
   $method =~ s/~//g;
   $method =~ s/[\s-]+/_/g;
   if ($w->can($method))
    {
     $menu->Menubutton(-label => $lab, -tearoff => 0, -menuitems => $w->$method());
    }
  }
 return $menu;
}

sub menu
{
 my ($w,$menu) = @_;
 if (@_ > 1)
  {
   $w->_OnDestroy('_MENU_') unless exists $w->{'_MENU_'};
   $w->{'_MENU_'} = $menu;
  }
 return unless defined wantarray;
 unless (exists $w->{'_MENU_'})
  {
   $w->_OnDestroy('_MENU_');
   $w->{'_MENU_'} = $menu = $w->Menu(-tearoff => 0);
   $w->FillMenu($menu,$w->MenuLabels);
  }
 return $w->{'_MENU_'};
}

sub MenuLabels
{
 return @DefaultMenuLabels;
}

sub FileMenuItems
{
 my ($w) = @_;
 return [ ["command"=>'E~xit', -command => [ $w, 'WmDeleteWindow']]];
}

sub WmDeleteWindow
{
 shift->toplevel->WmDeleteWindow
}

sub BalloonInfo
{
 my ($widget,$balloon,$X,$Y,@opt) = @_;
 foreach my $opt (@opt)
  {
   my $info = $balloon->GetOption($opt,$widget);
   return $info if defined $info;
  }
}

sub ConfigSpecs {

    my $w = shift;

    return map { ( $_->[0], [ $w, @$_[ 1 .. 4 ] ] ) } $w->configure;

}

*GetSelection =
    ($Tk::platform eq 'unix'
     ? sub
        {
         my $w = shift;
         my $sel = @_ ? shift : "PRIMARY";
         my $txt = eval { local $SIG{__DIE__};
			  $w->SelectionGet(-selection => $sel, -type => "UTF8_STRING")
  		        };
         if ($@)
	  {
  	   $txt = eval { local $SIG{__DIE__};
			 $w->SelectionGet(-selection => $sel)
  		       };
  	 if ($@)
  	  {
  	   die "could not find default selection";
            }
          }
         $txt;
        }
     : sub
        {
	 my $w = shift;
	 my $sel = @_ ? shift : "PRIMARY";
	 my $txt = eval { local $SIG{__DIE__};
			  $w->SelectionGet(-selection => $sel)
		        };
	 if ($@)
	  {
	   die "could not find default selection";
          }
	 $txt;
        }
    );

1;
__END__

sub bindDump {

    # Dump lots of good binding information.  This pretty-print subroutine
    # is, essentially, the following code in disguise:
    #
    # print "Binding information for $w\n";
    # foreach my $tag ($w->bindtags) {
    #     printf "\n Binding tag '$tag' has these bindings:\n";
    #     foreach my $binding ($w->Tk::bind($tag)) {
    #         printf "  $binding\n";
    #     }
    # }

    my ($w) = @_;

    my (@bindtags) = $w->bindtags;
    my $digits = length( scalar @bindtags );
    my ($spc1, $spc2) = ($digits + 33, $digits + 35);
    my $format1 = "%${digits}d.";
    my $format2 = ' ' x ($digits + 2);
    my $n = 0;

    my @out;
    push @out, sprintf( "\n## Binding information for '%s', %s ##", $w->PathName, $w );

    foreach my $tag (@bindtags) {
        my (@bindings) = $w->Tk::bind($tag);
        $n++;                   # count this bindtag

        if ($#bindings == -1) {
            push @out, sprintf( "\n$format1 Binding tag '$tag' has no bindings.\n", $n );
        } else {
            push @out, sprintf( "\n$format1 Binding tag '$tag' has these bindings:\n", $n );

            foreach my $binding ( @bindings ) {
                my $callback = $w->Tk::bind($tag, $binding);
                push @out, sprintf( "$format2%27s : %-40s\n", $binding, $callback );

                if ($callback =~ /SCALAR/) {
                    if (ref $$callback) {
                        push @out, sprintf( "%s %s\n", ' ' x $spc1, $$callback );
                    } else {
                        push @out, sprintf( "%s '%s'\n", ' ' x $spc1, $$callback );
                    }
                } elsif ($callback =~ /ARRAY/) {
                    if (ref $callback->[0]) {
                        push @out, sprintf( "%s %s\n", ' ' x $spc1, $callback->[0], "\n" );
                    } else {
                        push @out, sprintf( "%s '%s'\n", ' ' x $spc1, $callback->[0], "\n" );
                    }
                    foreach my $arg (@$callback[1 .. $#$callback]) {
                        if (ref $arg) {
                            push @out, sprintf( "%s %-40s", ' ' x $spc2, $arg );
                        } else {
                            push @out, sprintf( "%s '%s'", ' ' x $spc2, $arg );
                        }
			
                        if (ref $arg eq 'Tk::Ev') {
                            if ($arg =~ /SCALAR/) {
                                push @out, sprintf( ": '$$arg'" );
                            } else {
                                push @out, sprintf( ": '%s'", join("' '", @$arg) );
                            }
                        }

                        push @out, sprintf( "\n" );
                    } # forend callback arguments
                } # ifend callback

            } # forend all bindings for one tag

        } # ifend have bindings

    } # forend all tags
    push @out, sprintf( "\n" );
    return @out;

} # end bindDump


sub ASkludge
{
 my ($hash,$sense) = @_;
 foreach my $key (%$hash)
  {
   if ($key =~ /-.*variable/ && ref($hash->{$key}) eq 'SCALAR')
    {
     if ($sense)
      {
       my $val = ${$hash->{$key}};
       require Tie::Scalar;
       tie ${$hash->{$key}},'Tie::StdScalar';
       ${$hash->{$key}} = $val;
      }
     else
      {
       untie ${$hash->{$key}};
      }
    }
  }
}



# clipboardKeysyms --
# This procedure is invoked to identify the keys that correspond to
# the "copy", "cut", and "paste" functions for the clipboard.
#
# Arguments:
# copy - Name of the key (keysym name plus modifiers, if any,
# such as "Meta-y") used for the copy operation.
# cut - Name of the key used for the cut operation.
# paste - Name of the key used for the paste operation.
#
# This method is obsolete use clipboardOperations and abstract
# event types instead. See Clipboard.pm and Mainwindow.pm

sub clipboardKeysyms
{
 my @class = ();
 my $mw    = shift;
 if (ref $mw)
  {
   $mw = $mw->DelegateFor('bind');
  }
 else
  {
   push(@class,$mw);
   $mw = shift;
  }
 if (@_)
  {
   my $copy  = shift;
   $mw->Tk::bind(@class,"<$copy>",'clipboardCopy')   if (defined $copy);
  }
 if (@_)
  {
   my $cut   = shift;
   $mw->Tk::bind(@class,"<$cut>",'clipboardCut')     if (defined $cut);
  }
 if (@_)
  {
   my $paste = shift;
   $mw->Tk::bind(@class,"<$paste>",'clipboardPaste') if (defined $paste);
  }
}

sub pathname
{
 my ($w,$id) = @_;
 my $x = $w->winfo('pathname',-displayof  => oct($id));
 return $x->PathName;
}

# ::tk::UnderlineAmpersand --
# This procedure takes some text with ampersand and returns
# text w/o ampersand and position of the ampersand.
# Double ampersands are converted to single ones.
# Position returned is -1 when there is no ampersand.
#
sub UnderlineAmpersand
{
 my (undef,$text) = @_;
 if ($text =~ m{(?<!&)&(?!&)}g)
  {
   my $idx = pos $text;
   $text =~ s{(?<!&)&(?!&)}{};
   ($text, $idx);
  }
 else
  {
   ($text, -1);
  }
}

# ::tk::SetAmpText -- 
# Given widget path and text with "magic ampersands",
# sets -text and -underline options for the widget
#
sub SetAmpText
{
 my ($w,$text) = @_;
 my ($newtext,$under) =  $w->UnderlineAmpersand($text);
 $w->configure(-text => $newtext, -underline => $under);
}

# ::tk::AmpWidget --
# Creates new widget, turning -text option into -text and
# -underline options, returned by ::tk::UnderlineAmpersand.
#
sub AmpWidget
{
 my ($w,$class,%args) = @_;
 my @options;
 while(my($opt,$val) = each %args)
  {
   if ($opt eq "-text")
    {
     my ($newtext,$under) = $w->UnderlineAmpersand($val);
     push @options, -text => $newtext, -underline => $under;
    }
   else
    {
     push @options, $opt, $val;
    }
  }
 my $result = $w->$class(@options);
 if ($result->can('AmpWidgetPostHook'))
  {
   $result->AmpWidgetPostHook;
  }
 return $result;
}

# ::tk::FindAltKeyTarget --
# search recursively through the hierarchy of visible widgets
# to find button or label which has $char as underlined character
#
sub FindAltKeyTarget
{
 my ($w,$char) = @_;
 $char = lc $char;
 if ($w->isa('Tk::Button') || $w->isa('Tk::Label'))
  {
   if ($char eq lc substr($w->cget(-text), $w->cget(-underline), 1))
    {
     return $w;
    }
   else
    {
     return undef;
    }
  }
 else
  {
   for my $cw ($w->gridSlaves, $w->packSlaves, $w->placeSlaves) # Cannot handle $w->formSlaves here?
    {
     my $target = $cw->FindAltKeyTarget($char);
     return $target if ($target);
    }
  }
 undef;
}

# ::tk::AltKeyInDialog --
# <Alt-Key> event handler for standard dialogs. Sends <<AltUnderlined>>
# to button or label which has appropriate underlined character
#
sub AltKeyInDialog
{
 my ($w, $key) = @_;
 my $target = $w->FindAltKeyTarget($key);
 return if !$target;
 $target->eventGenerate('<<AltUnderlined>>');
}

# ::tk::SetFocusGrab --
#   swap out current focus and grab temporarily (for dialogs)
# Arguments:
#   grab	new window to grab
#   focus	window to give focus to
# Results:
#   Returns nothing
#
sub SetFocusGrab
{
 my ($grab,$focus) = @_;
 my $index = "$grab,$focus";
 $Tk::FocusGrab{$index} ||= [];
 my $data = $Tk::FocusGrab{$index};
 push @$data, $grab->focusCurrent;
 my $oldGrab = $grab->grabCurrent;
 push @$data, $oldGrab;
 if (Tk::Exists($oldGrab))
  {
   push @$data, $oldGrab->grabStatus;
  }
 # The "grab" command will fail if another application
 # already holds the grab.  So catch it.
 Tk::catch { $grab->grab };
 if (Tk::Exists($focus))
  {
   $focus->focus;
  }
}

# ::tk::RestoreFocusGrab --
#   restore old focus and grab (for dialogs)
# Arguments:
#   grab	window that had taken grab
#   focus	window that had taken focus
#   destroy	destroy|withdraw - how to handle the old grabbed window
# Results:
#   Returns nothing
#
sub RestoreFocusGrab
{
 my ($grab, $focus, $destroy) = @_;
 $destroy = 'destroy' if !$destroy;
 my $index = "$grab,$focus";
 my ($oldFocus, $oldGrab, $oldStatus);
 if (exists $Tk::FocusGrab{$index})
  {
   ($oldFocus, $oldGrab, $oldStatus) = $Tk::FocusGrab{$index};
   delete $Tk::FocusGrab{$index};
  }
 else
  {
   $oldGrab = "";
  }

 Tk::catch { $oldFocus->focus };
 if (Tk::Exists($grab))
  {
   $grab->grabRelease;
   if ($destroy eq "withdraw")
    {
     $grab->withdraw;
    }
   else
    {
     $grab->destroy;
    }
  }
 if (Tk::Exists($oldGrab) && $oldGrab->ismapped)
  {
   if ($oldStatus eq "global")
    {
     $oldGrab->grabGlobal;
    }
   else
    {
     $oldGrab->grab;
    }
  }
}

# See http://rt.cpan.org/Ticket/Display.html?id=30929 and
# http://rt.cpan.org/Ticket/Display.html?id=31016
sub MasterMenu
 {
 }

