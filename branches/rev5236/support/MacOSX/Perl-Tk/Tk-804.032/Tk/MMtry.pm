# Copyright (c) 1995-2003 Nick Ing-Simmons. All rights reserved.
# This program is free software; you can redistribute it and/or
# modify it under the same terms as Perl itself.
package Tk::MMtry;
use Config;
require Exporter;

use vars qw($VERSION @EXPORT $VERBOSE);
#$VERSION = sprintf '4.%03d', q$Revision: #9 $ =~ /\D(\d+)\s*$/;
$VERSION = '4.011';

use base  qw(Exporter);
@EXPORT = qw(try_compile try_run);
use strict;
use File::Basename;
use File::Spec;

my $stderr_too = ($^O eq 'MSWin32') ? '' : '2>&1';

sub try_compile
{
 my ($file,$inc,$lib,$def)  = @_;
 $inc = [] unless $inc;
 $lib = [] unless $lib;
 $def = [] unless $def;
 my $stderr_too = $VERBOSE ? '' : $stderr_too;
 my $out   = basename($file,'.c').$Config{'exe_ext'};
 warn "Test Compiling $file\n";
 my $msgs  = `$Config{'cc'} -o $out $Config{'ccflags'} @$inc $file $Config{ldflags} @$lib @$def $stderr_too`;
 my $ok = ($? == 0);
 warn "$msgs\n" if $VERBOSE && $msgs;
 unlink($out) if (-f $out);
 return $ok;
}

sub try_run
{
 my ($file,$inc,$lib,$def)  = @_;
 $inc = [] unless $inc;
 $lib = [] unless $lib;
 $def = [] unless $def;
 my $stderr_too = $VERBOSE ? '' : $stderr_too;
 my $out   = basename($file,'.c').$Config{'exe_ext'};
 warn "Test Compile/Run $file\n";
 my $cmdline = "$Config{'cc'} -o $out $Config{'ccflags'} @$inc $file $Config{ldflags} @$lib @$def";
 my $msgs  = `$cmdline $stderr_too`;
 my $ok = ($? == 0);
 warn "$cmdline:\n$msgs\n" if $VERBOSE && $msgs;
 if ($ok)
  {
   my $path = File::Spec->rel2abs($out);
   $msgs = `$path $stderr_too`;
   $ok = ($? == 0);
   warn "$path:$msgs\n" if $VERBOSE && $msgs;
  }
 unlink($out) if (-f $out);
 return $ok;
}

1;
