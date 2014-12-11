# Copyright (c) 1995-2003 Nick Ing-Simmons. All rights reserved.
# This program is free software; you can redistribute it and/or
# modify it under the same terms as Perl itself.
package Tk::Pretty;
require Exporter;

use vars qw($VERSION @EXPORT);
$VERSION = '4.006'; # $Id: //depot/Tkutf8/Tk/Pretty.pm#6 $

use base  qw(Exporter);

@EXPORT = qw(Pretty PrintArgs);

sub pretty_list
{
 join(',',map(&Pretty($_),@_));
}

sub Pretty
{
 return pretty_list(@_) if (@_ > 1);
 my $obj = shift;
 return 'undef' unless defined($obj);
 my $type = "$obj";
 return $type if ($type =~ /=HASH/ && exists($obj->{"_Tcl_CmdInfo_\0"}));
 my $result = '';
 if (ref $obj)
  {
   my $class;
   if ($type =~ /^([^=]+)=(.*)$/)
    {
     $class = $1;
     $type  = $2;
     $result .= 'bless(';
    }
   if ($type =~ /^ARRAY/)
    {
     $result .= '[';
     $result .= pretty_list(@$obj);
     $result .= ']';
    }
   elsif ($type =~ /^HASH/)
    {
     $result .= '{';
     if (%$obj)
      {
       my ($key, $value);
       while (($key,$value) = each %$obj)
        {
         $result .= $key . '=>' . Pretty($value) . ',';
        }
       chop($result);
      }
     $result .= '}';
    }
   elsif ($type =~ /^REF/)
    {
     $result .= "\\" . Pretty($$obj);
    }
   elsif ($type =~ /^SCALAR/)
    {
     $result .= Pretty($$obj);
    }
   else
    {
     $result .= $type;
    }
   $result .= ",$class)" if (defined $class);
  }
 else
  {
   if ($obj =~ /^-?[0-9]+(.[0-9]*(e[+-][0-9]+)?)?$/ ||
       $obj =~ /^[A-Z_][A-Za-z_0-9]*$/ ||
       $obj =~ /^[a-z_][A-Za-z_0-9]*[A-Z_][A-Za-z_0-9]*$/
      )
    {
     $result .= $obj;
    }
   else
    {
     $result .= "'" . $obj . "'";
    }
  }
 return $result;
}

sub PrintArgs
{
 my $name = (caller(1))[3];
 print "$name(",Pretty(@_),")\n";
}

1;
