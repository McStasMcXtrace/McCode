package Tk::DragDrop;
require Tk::DragDrop::Common;
require Tk::Toplevel;
require Tk::Label;

use vars qw($VERSION);
$VERSION = '4.015'; # sprintf '4.%03d', q$Revision: #14 $ =~ /\D(\d+)\s*$/;

use base  qw(Tk::DragDrop::Common Tk::Toplevel);

# This is a little tricky, ISA says 'Toplevel' but we
# define a Tk_cmd to actually build a 'Label', then
# use wmRelease in Populate to make it a toplevel.

my $useWmRelease = Tk::Wm->can('release'); # ($^O ne 'MSWin32');

sub Tk_cmd { ($useWmRelease) ? \&Tk::label : \&Tk::toplevel }

Construct Tk::Widget 'DragDrop';

use strict;
use vars qw(%type @types);
use Carp;


# There is a snag with having a token window and moving to
# exactly where cursor is - the cursor is "inside" the token
# window - hence it is not "inside" the dropsite window
# so we offset X,Y by OFFSET pixels.
sub OFFSET () {3}

sub ClassInit
{
 my ($class,$mw) = @_;
 $mw->bind($class,'<Map>','Mapped');
 $mw->bind($class,'<Any-KeyPress>','Done');
 $mw->bind($class,'<Any-ButtonRelease>','Drop');
 $mw->bind($class,'<Any-Motion>','Drag');
 return $class;
}

sub Populate
{
 my ($token,$args) = @_;
 my $parent = $token->parent;
 if ($useWmRelease)
  {
   $token->wmRelease;
   $token->ConfigSpecs(-text => ['SELF','text','Text',$parent->class]);
  }
 else
  {
   my $lab = $token->Label->pack(-expand => 1, -fill => 'both');
   bless $lab,ref($token);
   $lab->bindtags([ref($token), $lab, $token, 'all']);
   $token->ConfigSpecs(-text => [$lab,'text','Text',$parent->class],
                       DEFAULT => [$lab]);
  }
 $token->withdraw;
 $token->overrideredirect(1);
 $token->ConfigSpecs(-sitetypes       => ['METHOD','siteTypes','SiteTypes',undef],
                     -startcommand    => ['CALLBACK',undef,undef,undef],
                     -endcommand      => ['CALLBACK',undef,undef,undef],
                     -predropcommand  => ['CALLBACK',undef,undef,undef],
                     -postdropcommand => ['CALLBACK',undef,undef,undef],
                     -delta           => ['PASSIVE','delta','Delta',10],
                     -cursor          => ['SELF','cursor','Cursor','hand2'],
                     -handlers        => ['SETMETHOD','handlers','Handlers',[[[$token,'SendText']]]],
                     -selection       => ['SETMETHOD','selection','Selection','XdndSelection'],
                     -event           => ['SETMETHOD','event','Event','<B1-Motion>']
                    );
 $token->{InstallHandlers} = 0;
 $args->{-borderwidth} = 3;
 $args->{-relief} = 'flat';
 $args->{-takefocus} = 1;
}

sub sitetypes
{
 my ($w,$val) = @_;
 confess "Not a widget $w" unless (ref $w);
 my $var = \$w->{Configure}{'-sitetypes'};
 if (@_ > 1)
  {
   if (defined $val)
    {
     $val = [$val] unless (ref $val);
     my $type;
     foreach $type (@$val)
      {
       Tk::DragDrop->import($type);
      }
    }
   $$var = $val;
  }
 return (defined $$var) ? $$var : \@types;
}

sub SendText
{
 my ($w,$offset,$max) = @_;
 my $s = substr($w->cget('-text'),$offset);
 $s = substr($s,0,$max) if (length($s) > $max);
 return $s;
}

sub handlers
{
 my ($token,$opt,$value) = @_;
 $token->{InstallHandlers} = (defined($value) && @$value);
 $token->{'handlers'}  = $value;
}

sub selection
{
 my ($token,$opt,$value) = @_;
 my $handlers = $token->{'handlers'};
 $token->{InstallHandlers} = (defined($handlers) && @$handlers);
}

sub event
{
 my ($w,$opt,$value) = @_;
 # delete old bindings
 $w->parent->Tk::bind($value,[$w,'StartDrag']);
}

#

sub FindSite
{
 my ($token,$X,$Y,$e) = @_;
 my $site;
 my $types = $token->sitetypes;
 if (defined $types && @$types)
  {
   foreach my $type (@$types)
    {
     my $class = $type{$type};
     last if (defined($class) && ($site = $class->FindSite($token,$X,$Y)));
    }
  }
 else
  {
   warn 'No sitetypes';
  }
 my $new = $site || 'undef';
 my $over = $token->{'Over'};
 if ($over)
  {
   if (!$over->Match($site))
    {
     $over->Leave($token,$e);
     delete $token->{'Over'};
    }
  }
 if ($site)
  {
   unless ($token->{'Over'})
    {
     $site->Enter($token,$e);
     $token->{'Over'} = $site;
    }
   $site->Motion($token,$e) if (defined $site)
  }
 return $site;
}

sub Mapped
{
 my ($token) = @_;
 my $e = $token->parent->XEvent;
 $token = $token->toplevel;
 $token->grabGlobal;
 $token->focus;
 if (defined $e)
  {
   my $X = $e->X;
   my $Y = $e->Y;
   $token->MoveToplevelWindow($X+OFFSET,$Y+OFFSET);
   $token->NewDrag;
   $token->FindSite($X,$Y,$e);
  }
}

sub NewDrag
{
 my ($token) = @_;
 my $types = $token->sitetypes;
 if (defined $types && @$types)
  {
   my $type;
   foreach $type (@$types)
    {
     my $class = $type{$type};
     if (defined $class)
      {
       $class->NewDrag($token);
      }
    }
  }
}

sub Drag
{
 my $token = shift;
 my $e = $token->XEvent;
 my $X  = $e->X;
 my $Y  = $e->Y;
 $token = $token->toplevel;
 $token->MoveToplevelWindow($X+OFFSET,$Y+OFFSET);
 $token->FindSite($X,$Y,$e);
}

sub Done
{
 my $token = shift;
 my $e     = $token->XEvent;
 $token    = $token->toplevel;
 my $over  = delete $token->{'Over'};
 $over->Leave($token,$e) if (defined $over);
 my $w     = $token->parent;
 eval {local $SIG{__DIE__}; $token->grabRelease };
 $token->withdraw;
 delete $w->{'Dragging'};
 $w->update;
}

sub AcceptDrop
{
 my ($token) = @_;
 $token->configure(-relief => 'sunken');
 $token->{'Accepted'} = 1;
}

sub RejectDrop
{
 my ($token) = @_;
 $token->configure(-relief => 'flat');
 $token->{'Accepted'} = 0;
}

sub HandleLoose
{
 my ($w,$seln) = @_;
 return '';
}

sub InstallHandlers
{
 my ($token,$seln) = @_;
 my $w = $token->parent;
 $token->configure('-selection' => $seln) if $seln;
 $seln = $token->cget('-selection');
 if ($token->{InstallHandlers})
  {
   foreach my $h (@{$token->cget('-handlers')})
    {
     $w->SelectionHandle('-selection' => $seln,@$h);
    }
   $token->{InstallHandlers} = 0;
  }
 if (!$w->IS($w->SelectionOwner('-selection'=>$seln)))
  {
   $w->SelectionOwn('-selection' => $seln, -command => [\&HandleLoose,$w,$seln]);
  }
}

sub Drop
{
 my $ewin  = shift;
 my $e     = $ewin->XEvent;
 my $token = $ewin->toplevel;
 my $site  = $token->FindSite($e->X,$e->Y,$e);
 Tk::catch { $token->grabRelease };
 if (defined $site)
  {
   my $seln = $token->cget('-selection');
   unless ($token->Callback(-predropcommand => $seln, $site))
    {
# XXX This is ugly if the user restarts a drag within the 2000 ms:
#     my $id = $token->after(2000,[$token,'Done']);
     my $w = $token->parent;
     $token->InstallHandlers;
     $site->Drop($token,$seln,$e);
     $token->Callback(-postdropcommand => $seln);
     $token->Done;
    }
  }
 else
  {
   $token->Done;
  }
 $token->Callback('-endcommand');
}

sub StartDrag
{
 my $token = shift;
 my $w     = $token->parent;
 unless ($w->{'Dragging'})
  {
   my $e = $w->XEvent;
   my $X = $e->X;
   my $Y = $e->Y;
   my $was = $token->{'XY'};
   if ($was)
    {
     my $dx = $was->[0] - $X;
     my $dy = $was->[1] - $Y;
     if (sqrt($dx*$dx+$dy*$dy) > $token->cget('-delta'))
      {
       unless ($token->Callback('-startcommand',$token,$e))
        {
         delete $token->{'XY'};
         $w->{'Dragging'} = $token;
         $token->MoveToplevelWindow($X+OFFSET,$Y+OFFSET);
         $token->raise;
         $token->deiconify;
         $token->FindSite($X,$Y,$e);
        }
      }
    }
   else
    {
     $token->{'XY'} = [$X,$Y];
    }
  }
}


1;
