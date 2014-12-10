package Tk::IO;
use strict;
use vars qw($VERSION);
$VERSION = '4.006';

require 5.002;
use Tk::Event qw($XS_VERSION);

use Carp;
use base  qw(DynaLoader IO::Handle);

bootstrap Tk::IO;

my %fh2obj;
my %obj2fh;

sub new
{
 my ($package,%args) = @_;
 # Do whatever IO::Handle does
 my $fh  = $package->SUPER::new;
 %{*$fh} = ();  # The hash is used for configure options
 ${*$fh} = '';  # The scalar is used as the 'readable' buffer
 @{*$fh} = ();  # The array
 $fh->configure(%args);
 return $fh;
}

sub pending
{
 my $fh = shift;
 return ${*$fh};
}

sub cget
{
 my ($fh,$key) = @_;
 return ${*$fh}{$key};
}

sub configure
{
 my ($fh,%args) = @_;
 my $key;
 foreach $key (keys %args)
  {
   my $val = $args{$key};
   $val = Tk::Callback->new($val) if ($key =~ /command$/);
   ${*$fh}{$key} = $val;
  }
}

sub killpg
{
 my ($fh,$sig) = @_;
 my $pid = $fh->pid;
 croak 'No child' unless (defined $pid);
 kill($sig,-$pid);
}

sub kill
{
 my ($fh,$sig) = @_;
 my $pid = $fh->pid;
 croak 'No child' unless (defined $pid);
 kill($sig,$pid) || croak "Cannot kill($sig,$pid):$!";
}

sub readable
{
 my $fh     = shift;
 my $count  = sysread($fh,${*$fh},1,length(${*$fh}));
 if ($count < 0)
  {
   if (exists ${*$fh}{-errorcommand})
    {
     ${*$fh}{-errorcommand}->Call($!);
    }
   else
    {
     warn "Cannot read $fh:$!";
     $fh->close;
    }
  }
 elsif ($count)
  {
   if (exists ${*$fh}{-linecommand})
    {
     my $eol = index(${*$fh},"\n");
     if ($eol >= 0)
      {
       my $line = substr(${*$fh},0,++$eol);
       substr(${*$fh},0,$eol) = '';
       ${*$fh}{-linecommand}->Call($line);
      }
    }
  }
 else
  {
   $fh->close;
  }
}

sub pid
{
 my $fh = shift;
 return ${*$fh}{-pid};
}

sub command
{
 my $fh  = shift;
 my $cmd = ${*$fh}{'-exec'};
 return (wantarray) ? @$cmd : $cmd;
}

sub exec
{
 my $fh  = shift;
 my $pid = open($fh,'-|');
 if ($pid)
  {
   ${*$fh} = '' unless (defined ${*$fh});
   ${*$fh}{'-exec'} = [@_];
   ${*$fh}{'-pid'}  = $pid;
   if (exists ${*$fh}{-linecommand})
    {
     my $w = ${*$fh}{-widget};
     $w = 'Tk' unless (defined $w);
     $w->fileevent($fh,'readable',[$fh,'readable']);
     ${*$fh}{_readable} = $w;
    }
   else
    {
     require Tk::Pretty;
     croak Tk::Pretty::Pretty(\%{*$fh});
    }
   return $pid;
  }
 else
  {
   # make STDERR same as STDOUT here
   setpgrp;
   exec(@_) || die 'Cannot exec ',join(' ',@_),":$!";
  }
}

sub wait
{
 my $fh = shift;
 my $code;
 my $ch = delete ${*$fh}{-childcommand};
 ${*$fh}{-childcommand} = Tk::Callback->new(sub { $code = shift });
 Tk::Event::DoOneEvent(0) until (defined $code);
 if (defined $ch)
  {
   ${*$fh}{-childcommand} = $ch;
   $ch->Call($code,$fh)
  }
 return $code;
}

sub close
{
 my $fh = shift;
 my $code;
 if (defined fileno($fh))
  {
   my $w = delete ${*$fh}{_readable};
   $w->fileevent($fh,'readable','') if (defined $w);
   $code = close($fh);
   if (exists ${*$fh}{-childcommand})
    {
     ${*$fh}{-childcommand}->Call($?,$fh);
    }
  }
 return $code;
}

1;
__END__


