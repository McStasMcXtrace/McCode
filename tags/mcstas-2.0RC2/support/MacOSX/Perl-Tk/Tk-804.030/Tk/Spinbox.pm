package Tk::Spinbox;
use strict;

use vars qw($VERSION);
$VERSION = '4.007'; # sprintf '4.%03d',q$Revision: #6 $ =~ /#(\d+)/;

use base 'Tk::Entry';

sub Tk_cmd { \&Tk::spinbox }

# Also inherits Entry's methods
Tk::Methods( "identify", "invoke", "set" );
use Tk::Submethods ( 'selection' => ["element"] );

Construct Tk::Widget 'Spinbox';

sub ClassInit
{
 my ($class,$mw) = @_;

 $class->SUPER::ClassInit($mw);

 $mw->bind($class, '<Up>', [invoke => 'buttonup']);
 $mw->bind($class, '<4>', [invoke => 'buttonup']);
 $mw->bind($class, '<Down>',[invoke => 'buttondown']);
 $mw->bind($class, '<5>', [invoke => 'buttondown']);

 return $class;
}

sub Invoke
{
 my ($w,$elem) = @_;
 unless ($w->{_outside})
  {
   $w->invoke($elem);
   $w->{_repeated}++;
  }
 my $delay = $w->cget('-repeatinterval');
 if ($delay > 0)
  {
   $w->RepeatId($w->after($delay,[Invoke => $w,$elem]));
  }
}

sub Button1
{
 my ($w,$x,$y) = @_;
 my $elem = $w->identify($x,$y);
 $w->{_element} = $elem || 'entry';
 if ($w->{_element} eq 'entry')
  {
   $w->SUPER::Button1($x,$y);
  }
 elsif ($w->cget('-state') ne 'disabled')
  {
   $w->selectionElement($elem);
   $w->{_repeated} = 0;
   $w->{_outside} = 0;
   $w->{_relief}  = $w->cget("-${elem}relief");
   $w->CancelRepeat;
   my $delay = $w->cget('-repeatdelay');
   $w->RepeatId($w->after($delay,[Invoke => $w,$elem])) if $delay > 0;
  }
}

sub Motion
{
 my ($w,$x,$y) = @_;
 my $elem = $w->identify($x,$y);
 $w->{_element} = $elem || 'entry' unless $w->{_element};
 if ($w->{_element} eq 'entry')
  {
   $w->SUPER::Motion($x,$y);
  }
 else
  {
   if (!defined($elem) || $elem ne $w->{_element})
    {
     # Moved outside the button
     unless ($w->{_outside})
      {
       $w->{_outside} = 1;
       $w->selectionElement('none');
      }
    }
   elsif ($w->{_outside})
    {
     # Moved back over the button
     $w->selectionElement($elem);
     $w->{_outside} = 0;
    }
  }
}

sub Button1Release
{
 my ($w,$x,$y) = @_;
 $w->SUPER::Button1Release($x,$y);
 my $elem = $w->{_element};
 if (defined($elem) && $elem ne 'entry')
  {
   my $repeated = $w->{_repeated};
   if (defined($repeated) && !$repeated)
    {
     $w->invoke($elem);
    }
   my $relief = delete $w->{_relief};
   $w->configure("-${elem}relief",$relief) if $relief
  }
 $w->selectionElement('none');
}

1;
__END__
