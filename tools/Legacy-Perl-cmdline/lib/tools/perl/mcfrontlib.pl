# Library of common routines for McStas frontends.

# PDL requirement removed - used from mcfrontlib2D.pl
# use PDL;

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

use FileHandle;

require "mcrunlib.pl";

# Get numerical data for 1D detector, reading it from file if necessary
# the first time.
sub get_detector_data_1D {
    my ($info) = @_;
    if(defined($info->{'Numeric Data'})) {
      return $info->{'Numeric Data'};
    } else {
      my ($file) = @_;
      my $a = read_data_file_2D($info->{'Filename'});
      return undef unless defined($a);
      my ($m,$n) = $a->dims;
      my $vars = $info->{'Variables'};
      my $i;
      my $r = {};
      for $i (0..$m-1) {
          my $key = $vars->[$i] ? $vars->[$i] : "column $i";
          $r->{$key} = $a->slice("($i),");
      }
      $info->{'Numeric Data'} = $r;
      return $r;
    }
}


# Unquote a C-style quoted string. Limited to the four quote
# combinations '\n', '\r', '\"', and '\\'.
# The basic technique is to do a simple substitution, but it is
# complicated by the possibility of having multiple backslashes in a
# row (ie '\\\n'). To solve this problem, we first change all '\\'
# sequences to '\!'.
sub str_unquote {
    my ($val) = @_;
    # First replace any initial '\\' with '\!'.
    $val =~ s/^\\\\/\\!/;
    # Now replace any other '\\' with '\!'.
    while($val =~ s/([^\\])\\\\/$1\\!/g) {}
    # Finally replace all quote-combinations with their intended character.
    $val =~ s/\\n/\n/g;
    $val =~ s/\\r/\r/g;
    $val =~ s/\\"/"/g;
    $val =~ s/\\!/\\/g;
    return $val;
}

sub read_simulation_info {
    my ($handle) = @_;
    my $inf = { Params => {} };
    while(<$handle>) {
      if(/^\#*\s*Date:\s*(.*?)\s*$/i) {
          $inf->{'Date'} = $1;
      } elsif(/^\s*Ncount:\s*([-+0-9.eE]+)\s*$/i) {
          $inf->{'Ncount'} = $1;
      } elsif(/^\s*Numpoints:\s*([-+0-9.eE]+)\s*$/i) {
          $inf->{'Numpoints'} = $1;
      } elsif(/^\s*Seed:\s*([-+0-9.eE]+)\s*$/i) {
          $inf->{'Seed'} = $1;
      } elsif(/^\s*Trace:\s*(no|yes)\s*$/i) {
          $inf->{'Trace'} = get_yes_no($1);
      } elsif(/^\#*\s*Param:\s*([a-zA-Z_0-9]+)\s*=\s*([-+0-9.eE]+)\s*$/i){
          $inf->{'Params'}{$1} = $2;
      } elsif(/^\#*\s*Param:\s*([a-zA-Z_0-9]+)\s*=\s*"(.*)"\s*$/i){
          my ($param, $val) = ($1, $2);
          $inf->{'Params'}{$param} = str_unquote($val);
      } elsif(/^\#*\s*Param:\s*([a-zA-Z_0-9]+)\s*=\s*(.*?)\s*$/i){
          my ($param, $val) = ($1, $2);
          $inf->{'Params'}{$param} = str_unquote($val);
      } elsif(/^\s*end\s+simulation\s*$/i) {
          last;
      } else {
          # print "\# $_";
      }
    }
    return $inf;
}

sub read_data_info {
    my ($handle, $basedir) = @_;
    my ($type, $fname, $data, $xvar, $yvar, $yerr, @xvars, @yvars, @yerrs);
    my @vars = qw/X N I p2/;
    my @vals = ();
    my ($compname,$title,$xlabel,$ylabel,$stats) = ("","","","","");
    my ($xmin,$xmax,$ymin,$ymax) = (0,1,0,1);
    while(<$handle>) {
      if(/^\#*\s*type:\s*(.*?)\s*$/i) {
          $type = $1;
      } elsif(/^\#*\s*component:\s*([a-zA-Z_0-9]+)\s*$/i) {
          $compname = $1;
      } elsif(/^\#*\s*title:\s*(.*?)\s*$/i) {
          $title = strip_quote($1);
      } elsif(/^\#*\s*filename:\s*(.*?)\s*$/i) {
          $fname = strip_quote($1);
      } elsif(/^\#*\s*variables:\s*([a-zA-Z_0-9 \t]*?)\s*$/i) {
          @vars = split(" ", $1);
      } elsif(/^\#*\s*values:\s*([-+0-9.eE \t]*?)\s*$/i) {
          @vals = split(" ", $1);
      } elsif(/^\#*\s*xvar:\s*([a-zA-Z_0-9]+?)\s*$/i) {
          $xvar = $1;
      } elsif(/^\#*\s*yvar:\s*([a-zA-Z_0-9]+?)\s*$/i) {
          $yvar = $1;
          $yerr = undef;
      } elsif(/^\#*\s*yvar:\s*
            \(\s*([a-zA-Z_0-9]+)\s*,
              \s*([a-zA-Z_0-9]+)\s*
            \)\s*$/ix) {
          $yvar = $1;
          $yerr = $2;
      } elsif(/^\#*\s*xvars:\s*
            ([a-zA-Z_0-9]+
             (\s+[a-zA-Z_0-9]+)*
            )\s*$/ix) {
          @xvars = split(" ", $1);
      } elsif(/^\#*\s*yvars:
            ((
             \s*\([a-zA-Z_0-9]+,[a-zA-Z_0-9]+\)
            )+)\s*$/ix) {
          @yvars = ();
          @yerrs = ();
          for (split(" ", $1)) {
            if(/\(([a-zA-Z_0-9]+),([a-zA-Z_0-9]+)\)/) {
                push @yvars, $1;
                push @yerrs, $2;
            } else {
                die "Internal: mcfrontlib/yvars";
            }
          }
      } elsif(/^\#*\s*xlabel:\s*(.*?)\s*$/i) {
          $xlabel = strip_quote($1);
      } elsif(/^\#*\s*ylabel:\s*(.*?)\s*$/i) {
          $ylabel = strip_quote($1);
      } elsif(/^\#*\s*xylimits:\s*
            ([-+0-9.eE]+)\s+
            ([-+0-9.eE]+)\s+
            ([-+0-9.eE]+)\s+
            ([-+0-9.eE]+)\s*$/ix) {
          ($xmin,$xmax,$ymin,$ymax) = ($1,$2,$3,$4);
      } elsif(/^\#*\s*xylimits:\s*
            ([-+0-9.eE]+)\s+
            ([-+0-9.eE]+)\s+
            ([-+0-9.eE]+)\s+
            ([-+0-9.eE]+)\s*
            ([-+0-9.eE]+)\s+
            ([-+0-9.eE]+)\s*$/ix) {
          ($xmin,$xmax,$ymin,$ymax) = ($1,$2,$3,$4);
      } elsif(/^\#*\s*xlimits:\s*
            ([-+0-9.eE]+)\s+
            ([-+0-9.eE]+)\s*$/ix) {
          ($xmin,$xmax) = ($1,$2);
      } elsif(/^\#*\s*begin array2D\s*\(([0-9]+),([0-9]+)\)\s*/i) {
          $data = read_array2D($handle,$1,$2);
      } elsif(/^\#*\s*begin array_2D\s*\(([0-9]+),([0-9]+)\)\s*/i) {
          $data = read_array2D($handle,$1,$2);
      } elsif(/^\#*\s*begin array_1D\s*\(([0-9]+)\)\s*/i) {
          $data = read_array2D($handle,4,$2);
      } elsif(/^\#*\s*statistics:\s*(.*?)\s*$/i) {
          $stats = $1;
      } elsif(/^\#*\s*end\s+data\s*$/i) {
          last;
      } else {
          # print "\# $_";
      }
    }
    die "Missing type for component $compname"
      unless $type;
    # Use first of multiple X variables as single X variable.
    $xvar = $xvars[0] if @xvars && !$xvar;
    # Use first of multiple Y variables as single Y variable.
    $yvar = $yvars[0] if @yvars && !$yvar;
    $yerr = $yerrs[0] if @yerrs && !$yvar;
    # Convert 2D array to 1D array hash for 1D detector type.
    if(defined($data) && $type =~ /^\s*array_1d\s*\(\s*([0-9]+)\s*\)\s*$/i) {
      my $r = {};
      my ($m,$n) = $data->dims;
      my $i;
      for $i (0..$m-1) {
          my $key = $vars[$i] ? $vars[$i] : "column $i";
          $r->{$key} = $data->slice("($i),");
      }
      $data = $r;
    }
    # Select some reasonable defaults for axis variables if not present.
    if($type !~ /^\s*array_0d\s*$/i) {
      $xvar = $vars[0] ? $vars[0] : "column 0" unless $xvar;
      $yvar = $vars[1] ? $vars[1] : "column 1" unless $yvar;
      die "Missing filename for component $compname"
          unless $fname || $data;
      $fname = "$basedir/$fname" if($fname && $basedir);
    }
    # Convert type multiarray_1d to multiple array_1d (for mcrun scan output).
    if($type =~ /^multiarray_1d\((.*)\)$/i) {
      my $size = $1;
      my $res = [];
      my $i;
      for($i = 0; $i < @yvars; $i++) {
	  $compname = $yvars[$i];
          push @$res, { Type => "array_1d($size)",
                    Component => $compname,
                    Title => $title,
                    Variables => \@vars,
                    Values => \@vals,
                    Xvar => [$xvar],
                    Yvar => [$yvars[$i]],
                    Yerr => [$yerrs[$i]],
                    Filename => $fname,
                    "Numeric Data" => $data,
                    Xlabel => $xlabel,
                    Ylabel => "$ylabel $yvars[$i]",
                    Limits => [$xmin,$xmax,$ymin,$ymax],
                    Stats  => $stats,
		    Origin => "scanfile"
                    };
      }
      return @$res;
    } else {
      return { Type => $type,
             Component => $compname,
             Title => $title,
             Variables => \@vars,
             Values => \@vals,
             Xvar => [$xvar],
             Yvar => [$yvar],
             Yerr => [$yerr],
             Filename => $fname,
             "Numeric Data" => $data,
             Xlabel => $xlabel,
             Ylabel => $ylabel,
             Limits => [$xmin,$xmax,$ymin,$ymax],
             Stats  => $stats
             };
    }
}

sub read_sim_info {
    my ($handle, $basedir) = @_;
    my @datalist = ();
    my $instrument_info={};
    my $simulation_info={};
    my $error = "no error";
    while(<$handle>) {
      if(/^\s*begin\s+data\s*$/i) {
          my @info = read_data_info($handle, $basedir);
          push @datalist, grep($_->{Type} !~ /^\s*array_0d\s*$/, @info);
      } elsif(/^\s*begin\s+instrument\s*$/i) {
          $instrument_info = read_instrument_info($handle);
      } elsif(/^\s*begin\s+simulation\s*$/i) {
          $simulation_info = read_simulation_info($handle);
      } elsif(/^\s*#(.*?)\s*$/i) {
          # This is a McStas single data file
          my @info = read_data_info($handle, $basedir);
          push @datalist, grep($_->{Type} !~ /^\s*array_0d\s*$/, @info);
          $simulation_info = read_simulation_info($handle);
          # instr info
          $instrument_info->{'Name'} = $info->{'Filename'};
          $instrument_info->{'Parameters'} = $simulation_info->{'Params'};
          $instrument_info->{'Instrument-source'} = $info->{'Component'};
          return ($instrument_info, $simulation_info, \@datalist, $error);
      } elsif(/^\s*$/) {
          next;
      } else {
          # print "Invalid line in siminfo file (read_sim_info):\n'$_'";
      }
    }
    return ($instrument_info, $simulation_info, \@datalist, $error);
}

sub read_sim_file {
    my ($file) = @_;
    my $basedir;
    $basedir = $1 if $file && $file =~ m|^(.*)/[^/]*$|;
    my $handle = new FileHandle;
    open $handle, $file or die "Could not open file '$file'";
    read_sim_info($handle, $basedir);
}

1;
