# Copyright (c) 1995-2003 Nick Ing-Simmons. All rights reserved.
# This program is free software; you can redistribute it and/or
# modify it under the same terms as Perl itself.
package Tk::Derived;
require Tk::Widget;
require Tk::Configure;
use strict;
use Carp;

use vars qw($VERSION);
$VERSION = '4.011'; # sprintf '4.%03d', q$Revision: #10 $ =~ /\D(\d+)\s*$/;

$Tk::Derived::Debug = 0;

my $ENHANCED_CONFIGSPECS = 0; # disable for now

use Tk qw(NORMAL_BG BLACK);

sub Subwidget
{
 my $cw = shift;
 my @result = ();
 if (exists $cw->{SubWidget})
  {
   if (@_)
    {
     foreach my $name (@_)
      {
       push(@result,$cw->{SubWidget}{$name}) if (exists $cw->{SubWidget}{$name});
      }
    }
   else
    {
     @result = values %{$cw->{SubWidget}};
    }
  }
 return (wantarray) ? @result : $result[0];
}

sub _makelist
{
 my $widget = shift;
 my (@specs) = (ref $widget && ref $widget eq 'ARRAY') ? (@$widget) : ($widget);
 return @specs;
}

sub Subconfigure
{
 # This finds the widget or widgets to to which to apply a particular
 # configure option
 my ($cw,$opt) = @_;
 my $config = $cw->ConfigSpecs;
 my $widget;
 my @subwidget = ();
 my @arg = ();
 if (defined $opt)
  {
   $widget = $config->{$opt};
   unless (defined $widget)
    {
     $widget = ($opt =~ /^-(.*)$/) ? $config->{$1} : $config->{-$opt};
    }
   # Handle alias entries
   if (defined($widget) && !ref($widget))
    {
     $opt    = $widget;
     $widget = $config->{$widget};
    }
   push(@arg,$opt) unless ($opt eq 'DEFAULT');
  }
 $widget = $config->{DEFAULT} unless (defined $widget);
 if (defined $widget)
  {
   $cw->BackTrace("Invalid ConfigSpecs $widget") unless (ref($widget) && (ref $widget eq 'ARRAY'));
   $widget = $widget->[0];
  }
 else
  {
   $widget = 'SELF';
  }
 foreach $widget (_makelist($widget))
  {
   $widget = 'SELF' if (ref($widget) && $widget == $cw);
   if (ref $widget)
    {
     my $ref = ref $widget;
     if ($ref eq 'ARRAY')
      {
       $widget = Tk::Configure->new(@$widget);
       push(@subwidget,$widget)
      }
     elsif ($ref eq 'HASH')
      {
       foreach my $key (%$widget)
        {
         foreach my $sw (_makelist($widget->{$key}))
          {
           push(@subwidget,Tk::Configure->new($sw,$key));
          }
        }
      }
     else
      {
       push(@subwidget,$widget)
      }
    }
   elsif ($widget eq 'ADVERTISED')
    {
     push(@subwidget,$cw->Subwidget)
    }
   elsif ($widget eq 'DESCENDANTS')
    {
     push(@subwidget,$cw->Descendants)
    }
   elsif ($widget eq 'CHILDREN')
    {
     push(@subwidget,$cw->children)
    }
   elsif ($widget eq 'METHOD')
    {
     my ($method) = ($opt =~ /^-?(.*)$/);
     push(@subwidget,Tk::Configure->new($method,$method,$cw))
    }
   elsif ($widget eq 'SETMETHOD')
    {
     my ($method) = ($opt =~ /^-?(.*)$/);
     push(@subwidget,Tk::Configure->new($method,'_cget',$cw,@arg))
    }
   elsif ($widget eq 'SELF')
    {
     push(@subwidget,Tk::Configure->new('Tk::configure', 'Tk::cget', $cw,@arg))
    }
   elsif ($widget eq 'PASSIVE')
    {
     push(@subwidget,Tk::Configure->new('_configure','_cget',$cw,@arg))
    }
   elsif ($widget eq 'CALLBACK')
    {
     push(@subwidget,Tk::Configure->new('_callback','_cget',$cw,@arg))
    }
   else
    {
     push(@subwidget,$cw->Subwidget($widget));
    }
  }
 $cw->BackTrace("No delegate subwidget '$widget' for $opt") unless (@subwidget);
 return (wantarray) ? @subwidget : $subwidget[0];
}

sub _cget
{
 my ($cw,$opt) = @_;
 $cw->BackTrace('Wrong number of args to cget') unless (@_ == 2);
 return $cw->{Configure}{$opt}
}

sub _configure
{
 my ($cw,$opt,$val) = @_;
 $cw->BackTrace('Wrong number of args to configure') unless (@_ == 3);
 $cw->{Configure}{$opt} = $val;
}

sub _callback
{
 my ($cw,$opt,$val) = @_;
 $cw->BackTrace('Wrong number of args to configure') unless (@_ == 3);
 $val = Tk::Callback->new($val) if defined($val) && ref($val);
 $cw->{Configure}{$opt} = $val;
}

sub cget
{my ($cw,$opt) = @_;
 my @result;
 local $SIG{'__DIE__'};
 foreach my $sw ($cw->Subconfigure($opt))
  {
   if (wantarray)
    {
     eval {  @result = $sw->cget($opt) };
    }
   else
    {
     eval {  $result[0] = $sw->cget($opt) };
    }
   last unless $@;
  }
 return wantarray ? @result : $result[0];
}

sub Configured
{
 # Called whenever a derived widget is re-configured
 my ($cw,$args,$changed) = @_;
 if (@_ > 1)
  {
   $cw->afterIdle(['ConfigChanged',$cw,$changed]) if (%$changed);
  }
 return exists $cw->{'Configure'};
}

sub configure
{
 # The default composite widget configuration method uses hash stored
 # in the widget's hash to map configuration options
 # onto subwidgets.
 #
 my @results = ();
 my $cw = shift;
 if (@_ <= 1)
  {
   # Enquiry cases
   my $spec = $cw->ConfigSpecs;
   if (@_)
    {
     # Return info on the nominated option
     my $opt  = $_[0];
     my $info = $spec->{$opt};
     unless (defined $info)
      {
       $info = ($opt =~ /^-(.*)$/) ? $spec->{$1} : $spec->{-$opt};
      }
     if (defined $info)
      {
       if (ref $info)
        {
         # If the default slot is undef then ask subwidgets in turn
         # for their default value until one accepts it.
         if ($ENHANCED_CONFIGSPECS && !defined($info->[3]))
          {local $SIG{'__DIE__'};
           my @def;
           foreach my $sw ($cw->Subconfigure($opt))
            {
             eval { @def = $sw->configure($opt) };
             last unless $@;
            }
           $info->[3] = $def[3];
           $info->[1] = $def[1] unless defined $info->[1];
           $info->[2] = $def[2] unless defined $info->[2];
          }
         push(@results,$opt,$info->[1],$info->[2],$info->[3],$cw->cget($opt));
        }
       else
        {
         # Real (core) Tk widgets return db name rather than option name
         # for aliases so recurse to get that ...
         my @real = $cw->configure($info);
         push(@results,$opt,$real[1]);
        }
      }
     else
      {
       push(@results,$cw->Subconfigure($opt)->configure($opt));
      }
    }
   else
    {
     my $opt;
     my %results;
     if (exists $spec->{'DEFAULT'})
      {
       foreach $opt ($cw->Subconfigure('DEFAULT')->configure)
        {
         $results{$opt->[0]} = $opt;
        }
      }
     foreach $opt (keys %$spec)
      {
       $results{$opt} = [$cw->configure($opt)] if ($opt ne 'DEFAULT');
      }
     foreach $opt (sort keys %results)
      {
       push(@results,$results{$opt});
      }
    }
  }
 else
  {
   my (%args) = @_;
   my %changed = ();
   my ($opt,$val);
   my $config = $cw->TkHash('Configure');

   while (($opt,$val) = each %args)
    {
     my $var = \$config->{$opt};
     my $old = $$var;
     $$var = $val;
     my $accepted = 0;
     my $error = "No widget handles $opt";
     foreach my $subwidget ($cw->Subconfigure($opt))
      {
       next unless (defined $subwidget);
       eval {local $SIG{'__DIE__'};  $subwidget->configure($opt => $val) };
       if ($@)
        {
         my $val2 = (defined $val) ? $val : 'undef';
         $error = "Can't set $opt to `$val2' for $cw: " . $@;
         undef $@;
        }
       else
        {
         $accepted = 1;
        }
      }
     $cw->BackTrace($error) unless ($accepted);
     $val = $$var;
     $changed{$opt} = $val if (!defined $old || !defined $val || "$old" ne "$val");
    }
   $cw->Configured(\%args,\%changed);
  }
 return (wantarray) ? @results : \@results;
}

sub ConfigDefault
{
 my ($cw,$args) = @_;

 $cw->BackTrace('Bad args') unless (defined $args && ref $args eq 'HASH');

 my $specs = $cw->ConfigSpecs;
 # Should we enforce a Delagates(DEFAULT => )  as well ?
 $specs->{'DEFAULT'} = ['SELF'] unless (exists $specs->{'DEFAULT'});

 #
 # This is a pain with Text or Entry as core widget, they don't
 # inherit SELF's cursor. So comment it out for Tk402.001
 #
 # $specs->{'-cursor'} = ['SELF',undef,undef,undef] unless (exists $specs->{'-cursor'});

 # Now some hacks that cause colours to propogate down a composite widget
 # tree - really needs more thought, other options adding such as active
 # colours too and maybe fonts

 my $child = ($cw->children)[0]; # 1st child window (if any)

 unless (exists($specs->{'-background'}))
  {
   Tk::catch { $cw->Tk::cget('-background') };
   my (@bg) = $@ ? ('PASSIVE') : ('SELF');
   push(@bg,'CHILDREN') if $child;
   $specs->{'-background'} = [\@bg,'background','Background',NORMAL_BG];
  }
 unless (exists($specs->{'-foreground'}))
  {
   Tk::catch { $cw->Tk::cget('-foreground') };
   my (@fg) = $@ ? ('PASSIVE') : ('SELF');
   push(@fg,'CHILDREN') if $child;
   $specs->{'-foreground'} = [\@fg,'foreground','Foreground',BLACK];
  }
 $cw->ConfigAlias(-fg => '-foreground', -bg => '-background');

 # Pre-scan args for aliases - this avoids defaulting
 # options specified via alias
 foreach my $opt (keys %$args)
  {
   my $info = $specs->{$opt};
   if (defined($info) && !ref($info))
    {
     $args->{$info} = delete $args->{$opt};
    }
  }

 # Now walk %$specs supplying defaults for all the options
 # which have a defined default value, potentially looking up .Xdefaults database
 # options for the name/class of the 'frame'

 foreach my $opt (keys %$specs)
  {
   if ($opt ne 'DEFAULT')
    {
     unless (exists $args->{$opt})
      {
       my $info = $specs->{$opt};
       if (ref $info)
        {
         # Not an alias
         if ($ENHANCED_CONFIGSPECS && !defined $info->[3])
          {
           # configure inquire to fill in default slot from subwidget
           $cw->configure($opt);
          }
         if (defined $info->[3])
          {
           if (defined $info->[1] && defined $info->[2])
            {
             # Should we do this on the Subconfigure widget instead?
             # to match *Entry.Background
             my $db = $cw->optionGet($info->[1],$info->[2]);
             $info->[3] = $db if (defined $db);
            }
           $args->{$opt} = $info->[3];
          }
        }
      }
    }
  }
}

sub ConfigSpecs
{
 my $cw = shift;
 my $specs = $cw->TkHash('ConfigSpecs');
 while (@_)
  {
   my $key = shift;
   my $val = shift;
   $specs->{$key} = $val;
  }
 return $specs;
}

sub _alias
{
 my ($specs,$opt,$main) = @_;
 if (exists($specs->{$opt}))
  {
   unless (exists $specs->{$main})
    {
     my $targ = $specs->{$opt};
     if (ref($targ))
      {
       # opt is a real option
       $specs->{$main} = $opt
      }
     else
      {
       # opt is itself an alias
       # make main point to same place
       $specs->{$main} = $targ unless $targ eq $main;
      }
    }
   return 1;
  }
 return 0;
}

sub ConfigAlias
{
 my $cw = shift;
 my $specs = $cw->ConfigSpecs;
 while (@_ >= 2)
  {
   my $opt  = shift;
   my $main = shift;
   unless (_alias($specs,$opt,$main) || _alias($specs,$main,$opt))
    {
     $cw->BackTrace("Neither $opt nor $main exist");
    }
  }
 $cw->BackTrace('Odd number of args to ConfigAlias') if (@_);
}

sub Delegate
{
 my ($cw,$method,@args) = @_;
 my $widget = $cw->DelegateFor($method);
 if ($widget == $cw)
  {
   $method = "Tk::Widget::$method"
  }
 my @result;
 if (wantarray)
  {
   @result   = $widget->$method(@args);
  }
 else
  {
   $result[0] = $widget->$method(@args);
  }
 return (wantarray) ? @result : $result[0];
}

sub InitObject
{
 my ($cw,$args) = @_;
 $cw->Populate($args);
 $cw->ConfigDefault($args);
}

sub ConfigChanged
{
 my ($cw,$args) = @_;
}

sub Advertise
{
 my ($cw,$name,$widget)  = @_;
 confess 'No name' unless (defined $name);
 croak 'No widget' unless (defined $widget);
 my $hash = $cw->TkHash('SubWidget');
 $hash->{$name} = $widget;              # advertise it
 return $widget;
}

sub Component
{
 my ($cw,$kind,$name,%args) = @_;
 $args{'Name'} = "\l$name" if (defined $name && !exists $args{'Name'});
 # my $pack = delete $args{'-pack'};
 my $delegate = delete $args{'-delegate'};
 my $w = $cw->$kind(%args);            # Create it
 # $w->pack(@$pack) if (defined $pack);
 $cw->Advertise($name,$w) if (defined $name);
 $cw->Delegates(map(($_ => $w),@$delegate)) if (defined $delegate);
 return $w;                            # and return it
}

1;
__END__


