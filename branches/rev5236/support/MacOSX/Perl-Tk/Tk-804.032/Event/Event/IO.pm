package Tk::Event::IO;
use strict;
use Carp;

use vars qw($VERSION @EXPORT_OK);
$VERSION = '4.009'; # sprintf '4.%03d', q$Revision: #8 $ =~ /\D(\d+)\s*$/;

use base qw(Exporter);
use Symbol ();

@EXPORT_OK = qw(READABLE WRITABLE);

sub PrintArgs
{
 my $func = (caller(1))[3];
 print "$func(",join(',',@_),")\n";
}

sub PRINT
{
 my $obj = shift;
 $obj->wait(WRITABLE);
 my $h = $obj->handle;
 return print $h @_;
}

sub PRINTF
{
 my $obj = shift;
 $obj->wait(WRITABLE);
 my $h = $obj->handle;
 return printf $h @_;
}

sub WRITE
{
 my $obj = $_[0];
 $obj->wait(WRITABLE);
 return syswrite($obj->handle,$_[1],$_[2]);
}

my $depth = 0;
sub READLINE
{
 my $obj = shift;
 $obj->wait(READABLE);
 my $h = $obj->handle;
 my $w = <$h>;
 return $w;
}

sub READ
{
 my $obj = $_[0];
 $obj->wait(READABLE);
 my $h = $obj->handle;
 return sysread($h,$_[1],$_[2],defined $_[3] ? $_[3] : 0);
}

sub GETC
{
 my $obj = $_[0];
 $obj->wait(READABLE);
 my $h = $obj->handle;
 return getc($h);
}

sub CLOSE
{
 my $obj = shift;
 $obj->unwatch;
 my $h = $obj->handle;
 return close($h);
}

sub EOF
{
 my $obj = shift;
 my $h = $obj->handle;
 return eof($h);
}

sub FILENO
{
 my $obj = shift;
 my $h = $obj->handle;
 return fileno($h);
}

sub imode
{
 my $mode = shift;
 my $imode = ${{'readable' => READABLE(),
                'writable' => WRITABLE()}}{$mode};
 croak("Invalid handler type '$mode'") unless (defined $imode);
 return $imode;
}

sub fileevent
{
 my ($widget,$file,$mode,$cb) = @_;
 my $imode = imode($mode);
 unless (ref $file)
  {
   no strict 'refs';
   $file = Symbol::qualify($file,(caller)[0]);
   $file = \*{$file};
  }
 my $obj = tied(*$file);
 unless ($obj && $obj->isa('Tk::Event::IO'))
  {
   $obj = tie *$file,'Tk::Event::IO', $file;
  }
 if (@_ == 3)
  {
   # query return the handler
   return $obj->handler($imode);
  }
 else
  {
   # set the handler
   my $h = $obj->handler($imode,$cb);
   undef $obj;  # Prevent warnings about untie with ref to object
   unless ($h)
    {
     untie *$file;
    }
  }
}

1;
__END__
