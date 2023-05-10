# Library of common routines for McStas frontends needing 2D data support.
#
#   This file is part of the McStas neutron ray-trace simulation package
#   Copyright (C) 1997-2006, All rights reserved
#   Risoe National Laborartory, Roskilde, Denmark
#   Institut Laue Langevin, Grenoble, France
#
#   This program is free software; you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation; version 3 of the License.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
#
#   You should have received a copy of the GNU General Public License
#   along with this program; if not, write to the Free Software
#   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
#

use PDL;

use FileHandle;

require "mcrunlib.pl";
require "mcfrontlib.pl";

# Read 2D numeric data, skipping comment lines.
# MOD: E. Farhi, Sep 28th, 2001: to cope with I_err and N in 2D files
sub read_data_file_2D {
    my ($file) = @_;
    my $h = new FileHandle;
    if(open($h, $file)) {
      my @list = ();
      while(<$h>) {
        last if /^\s*#\s+Errors\s*(.*)$/i;
        last if /^\s*#\s+Events\s*(.*)$/i;
        last if /^\s*#\s+EndDate\s*(.*)$/i;
        last if /^\s*#\s+end\s+I\s*$/i;
        next if /^\s*\n/;
        next if /^\s*#/;
        push(@list, new PDL (split " ")); 
        }
        close $h;
        return cat @list;
      } else {
      print STDOUT "Warning: failed to read data file \"$file\"\n";
      return undef;
    }
}

# Read 2D embedded numeric data.
sub read_array2D {
    my ($h,$m,$n) = @_;
    my @list = ();
    while(<$h>) {
      if(/^[-+0-9eE. \t]+$/) {
              push(@list, new PDL (split " "));
      } else {
              last if /^\s*end\s+array2D\s*/i;
              last if /^\s*end\s+array_2d\.*/i;
              last if /^\s*end\s+array_1d\.*/i;
              next if /^\s*\n/;
              next if /^\s*#\s*/;
              die "Bad embedded numeric data in array2D in file: $_\n";
      }
    }
    return cat @list;
}


# Get numerical data for 2D detector, reading it from file if necessary
# the first time.
sub get_detector_data_2D {
    my ($info) = @_;
    if (defined($info->{'Numeric Data'})) {
      return $info->{'Numeric Data'};
    } else {
      $info->{'Numeric Data'} = read_data_file_2D($info->{'Filename'});
      return $info->{'Numeric Data'};
    }
}


1;
