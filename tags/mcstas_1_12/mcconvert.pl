#!/usr/bin/perl
#
# Script for converting between 
#   Matlab->Scilab
#   Scilab->Matlab 
# McStas data
#
# Cyclic conversion, e.g. Matlab->Scilab->Matlab is NOT supported!
#
# Peter Willendrup, Risoe, 20031205
#
#   This file is part of the McStas neutron ray-trace simulation package
#   Copyright (C) 1997-2004, All rights reserved
#   Risoe National Laborartory, Roskilde, Denmark
#   Institut Laue Langevin, Grenoble, France
#
#   This program is free software; you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation; version 2 of the License.
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
#   Based on / inspired from m2sci.pl:
#     (m2sci.pl by Torbjorn Pettersen <Torbjorn.Pettersen@broadpark.no>
#      available from http://home.broadpark.no/~tpette-4/)
#
use File::Basename;
use File::Find;
use Cwd;
use Config;

# Declaration of the global scope vars,
my ($i,$j);
my ($outdir,$indir);
my ($format,$randstr,$randdir,$quiet);
my $cwd = getcwd();
my @junk;

# Assume no random output dir...
$randdir = 0;
# Assume we want status written to output
$quiet = 0;

# Echo something meaningful to stdout
print "mcconvert.pl, part of the McStas simulation package\n";
print "This is free software, licensed under the GPL\n";
print "Copyright (C) 1997-2004, All rights reserved\n";
print "Risoe National Laborartory, Roskilde, Denmark\n";
print "Institut Laue Langevin, Grenoble, France\n\n";

# Parse the commandline args
for ($i = 0; $i< @ARGV; $i++) {
  if ($ARGV[$i] =~ /^--format=([a-zA-Z0-9_\"]+)$/) {
    $format = $1;
  } elsif ($ARGV[$i] =~ /^--outdir=([a-zA-Z0-9_\\\/\"\:\.~]+)$/) {
    $outdir = $1;
  } elsif ($ARGV[$i] =~ /^--indir=([a-zA-Z0-9_\\\/\"\:\.~]+)$/) {
    $indir = $1;
  } elsif ($ARGV[$i] eq "--quiet") {
    $quiet = 1;
  } elsif ($ARGV[$i] eq "--help") {
    # Print some help info, next exit
    print "\nmcconvert.pl script from McStas\n";
    print "\nConverts between Matlab and Scilab version of\nMcStas output data.\n";
    print "\nusage:\n";
    print "\nNo arguments:\n\tWill try to guess wanted output format\n\tWill place files in random subdir\n";
    print "\tWill try to convert files in current dir\n";
    print "\n--format=FORMAT\n\tSpecifies wanted output format, i.e. Matlab or Scilab\n";
    print "\n--outdir=DIR\n\tSpecifies wanted output directory\n";
    print "\n--indir=DIR\n\tSpecifies wanted input directory\n";
    print "\n--quiet\n\tSuppress status info (e.g. mcstas.m -> mcstas.sci)\n";
    print "\nIf no --indir  is specified, --indir=\".\" is assumed\n";
    print "\nWarning: mcconvert.pl searches the --indir RECURSIVELY\n\n";
    exit();
  } else {
    push @junk, $ARGV[$i];
  }
}

if (@junk>0) {
  print "Warning: Options @junk on commandline not understood...\n\n";
}

# Handle the indir first...
if (!$indir) {
    print "You did not give me any files to convert, assuming that you ment everything in .\n";
    $indir = ".";
}

if (! -e $indir) {
    die "mcconvert.pl: Input directory $indir does not exist...\n\n";
}

if (-e "$indir/.converted") {
    print STDERR "\nmcconvert.pl:You are seeminly trying to convert a set of converted data!";
    die "\nCyclic conversion not supported!\n\n";
}

if (!$format) {
    print "You did not specify a conversion format (Scilab/Matlab)\n";
    print "Trying to guess one... ";
    if ((-e "$indir/mcstas.sci") && (! -e "$indir/mcstas.m")) {
	$format = "Matlab";
    } elsif ((! -e "$indir/mcstas.sci") && (-e "$indir/mcstas.m")) {
	$format = "Scilab";
    } elsif ((-e "$indir/mcstas.sci") && (-e "$indir/mcstas.m")) {
	die "mcconvert.pl: You have mcstas.sci AND mcstas.m - don't know what to do then...\n\n";
    } else {
	die "mcconvert.pl: Could not find a Matlab/Scilab mcstas.* to convert from...\n\n";
    }
    print "Converting to $format format...\n";
}


if (!$outdir) {
    # Create a random directory for the converted data
    $randstr = generate_random_string(5);
    $outdir = "converted_${format}_${randstr}";
    $randdir = 1;
}
if (! -e $outdir) {
    mkdir($outdir) or die "mcconvert.pl: Could not create $outdir\n\n";
    open(TMP,">$outdir/.stop");
    close(TMP);
}
if ($Config{'osname'} eq 'MSWin32') {
    if (!($outdir =~ /^.:/)) {
	$outdir = "$cwd/$outdir";	
    }
} else {
    if (!($outdir =~ /^\//)) {
	$outdir = "$cwd/$outdir";
    }
}
if ($randdir == 1) {
  print "\nYour files, converted to $format format will be placed in $outdir\n\n";
}

# If this is Win32, let us simply make any \ in the filenames a /
if ($Config{'osname'} eq 'MSWin32') {
    @junk = split(/\\/,$outdir);
    $outdir = join('/',@junk);
    @junk = split(/\\/,$indir);
    $indir = join('/',@junk);
}

# We are now ready to do the work:
chdir($indir);
find(\&findit,".");

open(TMP,">$outdir/.converted");
close(TMP);
chdir($cwd);
print "\n\nmcconvert.pl done...\n\n";

sub findit {
    my $object = $File::Find::name;
    $object =~ s!^\.!$outdir!g;
    if (-e ".stop") {
	$File::Find::prune = 1;
    } else {
	if (-d $_) {
	    if (!($object eq $outdir)) {
		if ((! -e $object) && (! -e "$_/.stop")) {
		    mkdir($object) || die "mcconvert.pl: Could not create $object!\n\n";
		}
	    }
	} else {
	    convert($_,$object);
	}
    }
}


sub convert {
    my ($infile,$outfile) = @_;
    my ($tmp, $base, $dirname, $suffix, $NEXTFUNC);
    $tmp = $File::Find::dir;
    $tmp =~ s!^\.\/!$indir/!;

    # Split the filename in pieces:
    ($base, $dirname, $suffix) = fileparse($outfile,".m",".sci");
    if ($suffix eq ".m") {
	$outfile = "$dirname$base.sci";
    } elsif ($suffix eq ".sci") {
	$outfile = "$dirname$base.m";
    } else {
	$outfile = "$dirname$base";
    }

    # Adjust the $base, remove .'s
    $base =~ s!\.!_!g;

    if ($quiet == 0) {
      print "Reading from $tmp/$infile -> ";
    }
    open(IN,"<$infile");
    open(OUT,">$outfile") || die "mcconvert.pl: Unable to create $outfile: $!\n\n";
    if ($quiet == 0) {
      print "\t $outfile\n";
    }
    $NEXTFUNC = 0;
    while (<IN>) {
	if ($format eq "Scilab") {
	    if ((/^\w*function/) & $NEXTFUNC) {
		print OUT "endfunction\n"; # add endfunction 
	    } else { $NEXTFUNC = 1;};
	
	    unless (/\w*fprintf\w*/) {
		s/%/\/\//g;			# Comments are replaced
	    }
	
	    s/nargout/argn(1)/g;	# Outputarguments to function
	    s/nargin/argn(2)/g;		# Inputarguments to function
	
	    s/isempty\((\w*)\)/($1==[])/g;
	    s/\r$//g;
	
	    # Replacing matlab functions with corresponding scilab commands.
	    s/\bchar\b/ascii/;
	    s/\bcompan\b/companion/g;
	    s/\bfclose\b/mclose/g;
	    s/\bfigure\b/xbasc/g;
	    s/\bfopen\b/mopen/g;
	    s/\breshape\b/matrix/g;
	    s/\bfindstr\b/strindex/g;
	    s/\bif\ single_file\b/if\ single_file==1/g;
	    s/\.func=\'/\.func=\'get_/g;
	} elsif ($format eq "Matlab") {
	    if (/^\w*endfunction/) {
		print OUT "%" ; # comment out endfunction 
	    }
	    unless (/\w*fprintf\w*/) {
		s/\/\//%/g;			# Comments are replaced
	    }
	
	    s/argn\(1\)/nargout/g;	# Outputarguments to function
	    s/argn\(2\)/nargin/g;		# Inputarguments to function
	
	    s/(\w*)==\[\]/isempty\(($1)\)/g;
	    s/struct\(\)/\[\]/g;
	
	    # Replacing matlab functions with corresponding scilab commands.
	    s/\bascii\b/char/;
	    s/\bcompanion\b/compan/g;
	    s/\bmclose\b/fclose/g;
	    s/\bxbasc\b/figure/g;
	    s/\bmopen\b/fopen/g;
	    s/\bmatrix\b/reshape/g;
	    s/\bstrindex\b/findstr/g;
	    s/\bif\ single_file==1/if\ single_file/g;
	    s/\bmode\b/\%mode/g;
	}
	
	# Check if we are at one of the inline functions - print new according 
	# to format, then exit (closing the open files...).
	if (/^\w*function d=mc\w*_inline/) {
	    inline_replace(OUT,$format,$base);
	    close(IN);
	    close(OUT);
	}
	print OUT;
    }
    close(OUT);
}

# Function for replacing the inline_ Matlab/Scilab functions
sub inline_replace {
    my ($OUTFILE,$format,$base) = @_;
    if ($format eq "Scilab") {
	print $OUTFILE
"function d=mcload_inline(d)
// local inline func to load data
execstr(['S=['+part(d.type,10:(length(d.type)-1))+'];']);
if ~length(d.data)
 if ~length(strindex(d.format, 'binary'))
  exec(d.filename,-1);p=d.parent;
  if ~execstr('d2='+d.func+'();','errcatch'),d=d2; d.parent=p;end
 else
  if length(strindex(d.format, 'float')), t='f';
  elseif length(strindex(d.format, 'double')), t='d';
  else return; end
  fid=mopen(d.filename, 'rb');
  pS = prod(S);
  x = mget(3*pS, t, fid);
  d.data  =matrix(x(1:pS), S);
  if length(x) >= 3*pS,
  d.errors=matrix(x((pS+1):(2*pS)), S);
  d.events=matrix(x((2*pS+1):(3*pS)), S);end
  mclose(fid);
  return
 end
end
endfunction
function d=mcplot_inline(d,p)
// local inline func to plot data
if ~length(strindex(d.type,'0d')), d=mcload_inline(d); end
if ~p, return; end;
execstr(['l=[',d.xylimits,'];']); S=size(d.data);
t1=['['+d.parent+'] '+d.filename+': '+d.title];t = [t1;['  '+d.variables+'=['+d.values+']'];['  '+d.signal];['  '+d.statistics]];
mprintf('%s\\n',t(:));
if length(strindex(d.type,'0d')),return;
else
w=winsid();if length(w),w=w(\$)+1; else w=0; end
xbasr(w); xset('window',w);
if length(strindex(d.type,'2d'))
 d.x=linspace(l(1),l(2),S(2)); d.y=linspace(l(3),l(4),S(1)); z=d.data;
 xlab=d.xlabel; ylab=d.ylabel; x=d.x; y=d.y;
 fz=max(abs(z));fx=max(abs(d.x));fy=max(abs(d.y));
 if fx>0,fx=round(log10(fx)); x=x/10^fx; xlab=xlab+' [*10^'+string(fx)+']'; end
 if fy>0,fy=round(log10(fy)); y=y/10^fy; ylab=ylab+' [*10^'+string(fy)+']'; end
 if fz>0,fz=round(log10(fz)); z=z/10^fz; t1=t1+' [*10^'+string(fz)+']'; end
 xset('colormap',hotcolormap(64));
 plot3d1(x,y,z',90,0,xlab+'@'+ylab+'@'+d.zlabel); xtitle(t);
else
d.x=linspace(l(1),l(2),max(S));
 plot2d(d.x,d.data);xtitle(t,d.xlabel,d.ylabel);end
end
xname(t1);
endfunction
mc_$base=get_$base();\n"
} elsif ($format eq "Matlab") {
    print $OUTFILE
"function d=mcload_inline(d)
%% local inline function to load data
S=d.type; eval(['S=[ ' S(10:(length(S)-1)) ' ];']);
if isempty(d.data)
 if ~length(findstr(d.format, 'binary'))
  copyfile(d.filename,[d.func,'.m']);p=d.parent;path(path);
  eval(['d=',d.func,';']);d.parent=p;delete([d.func,'.m']);
 else
  if length(findstr(d.format, 'float')), t='single';
  elseif length(findstr(d.format, 'double')), t='double';
  else return; end
  if length(S) == 1, S=[S 1]; end
  fid=fopen(d.filename, 'r');
  pS = prod(S);
  x = fread(fid, 3*pS, t);
  d.data  =reshape(x(1:pS), S);
  if prod(size(x)) >= 3*pS,
  d.errors=reshape(x((pS+1):(2*pS)), S);
  d.events=reshape(x((2*pS+1):(3*pS)), S);end
  fclose(fid);
  return
 end
end
return;
function d=mcplot_inline(d,p)
%% local inline function to plot data
if isempty(findstr(d.type,'0d')), d=mcload_inline(d); end\nif ~p, return; end;
eval(['l=[',d.xylimits,'];']); S=size(d.data);
t1=['[',d.parent,'] ',d.filename,': ',d.title];t = strvcat(t1,['  ',d.variables,'=[',d.values,']'],['  ',d.signal],['  ',d.statistics]);
disp(t);
if ~isempty(findstr(d.type,'0d')), return; end
figure; if ~isempty(findstr(d.type,'2d'))
d.x=linspace(l(1),l(2),S(2)); d.y=linspace(l(3),l(4),S(1)); 
surface(d.x,d.y,d.data);
else\nd.x=linspace(l(1),l(2),max(S));\nplot(d.x,d.data);end
xlabel(d.xlabel); ylabel(d.ylabel); title(t); axis tight;
set(gca,'position',[.18,.18,.7,.65]); set(gcf,'name',t1);grid on;
if ~isempty(findstr(d.type,'2d')), colorbar; end"
}
}

# This function generates random strings of a given length
sub generate_random_string
{
	my $length_of_randomstring=shift;#the length of the random string to generate

	my @chars=('a'..'z','A'..'Z','0'..'9','_');
	my $random_string;
	foreach (1..$length_of_randomstring) 
	{
		#rand @chars will generate a random number between 0 and scalar @chars
		$random_string.=$chars[rand @chars];
	}
	return $random_string;
}

