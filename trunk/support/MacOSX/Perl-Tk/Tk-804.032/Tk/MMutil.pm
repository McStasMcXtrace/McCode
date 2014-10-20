# Copyright (c) 1995-2003 Nick Ing-Simmons. All rights reserved.
# This program is free software; you can redistribute it and/or
# modify it under the same terms as Perl itself.
package Tk::MMutil;
use ExtUtils::MakeMaker;
use Cwd;
use Config;
use Carp;
use File::Basename;

use vars qw($VERSION);
$VERSION = '4.025';

# warn __FILE__." $VERSION\n";

use Tk::MakeDepend;

use Tk::Config qw(!$VERSION);
use vars qw($IsWin32);

*IsWin32 = \$main::IsWin32;
$IsWin32 = ($^O eq 'MSWin32' || $Config{'ccflags'} =~ /-D_?WIN32_?/)
	   unless defined $IsWin32;

@MYEXPORT = qw(pasthru perldepend cflags const_config constants installbin c_o xs_o makefile manifypods);

sub arch_prune
{
 my $hash = shift;
 foreach (keys %$hash)
  {
   if ($win_arch eq 'x')
    {
     delete $hash->{$_} if /Win[A-Z0-9]/ or /OS2/ or /ImgUtil/ or /^x/;
    }
   elsif ($win_arch eq 'open32')
    {
     delete $hash->{$_} if /Unix|Mwm/ and not /tclUnix/;
     delete $hash->{$_} if /winMain|dllMain/;
    }
   elsif ($win_arch eq 'pm')
    {
     delete $hash->{$_}
       if /Unix|Mwm/ and not
	 /tclUnix|Unix(3d|Button|Dialog|Color|Embed|Focus|Font|Menu|Scrlbr|Send|Int\.|Scale)/;
     delete $hash->{$_} if /os2Main|dllMain|tkOS2Dll|^x(colors\.c|gc\.)/;
     delete $hash->{$_} if /ImgUtil|tkWin[A-Z0-9]/ and not /OS2/;
    }
   elsif ($win_arch eq 'MSWin32')
    {
     delete $hash->{$_} if /Mwm/ and not /tclUnix/;
     delete $hash->{$_} if /winMain|dllMain/;
     # delete $hash->{$_} if /^Xrm/;
    }
  }
}

sub mTk_postamble
{
 my ($self) = @_;
 my $dep = "config :: \$(C_FILES) \$(H_FILES)\n\t$self->{NOECHO}\$(NOOP)\n";
 my $mTk = $self->{'MTK'};
 $dep .= "# Begin Munging dependencies\n";
 foreach my $file (sort keys %$mTk)
  {
   $dep .= "$file : ".$mTk->{$file}." \$(TKDIR)/pTk/Tcl-pTk\n";
   $dep .= "\t\$(PERL) \$(TKDIR)/pTk/Tcl-pTk ".$mTk->{$file}." $file\n";
  }
 $dep .= "# End Munging dependencies\n\n";
 return $dep;
}

sub mTk_CHO
{
 my $self = shift;
 my $mTk  = shift;
 my $exc  = shift;
 my %c;
 my %h;
 foreach (@{$self->{H}}) { $h{$_} = 1 }
 foreach (@{$self->{C}}) { $c{$_} = 1 }
 foreach (keys %$mTk)
  {
   if (/\.c$/)
    {
     $c{$_} = 1;
    }
   elsif (/\.h$/)
    {
     $h{$_} = 1;
    }
  }
 foreach (keys %$exc)
  {
   if (/\.c$/)
    {
     delete $c{$_};
    }
   elsif (/\.h$/)
    {
     delete $h{$_};
    }
  }
 while (@_)
  {
   my $name = shift;
   cluck("No $name") unless (exists $c{$name});
   delete $c{$name}
  }
 arch_prune(\%h);
 arch_prune(\%c);
 $self->{'H'}     = [sort keys %h];
 $self->{'C'}     = [sort keys %c];
 my(@o_files)     = @{$self->{C}};
 $self->{O_FILES} = [grep s/\.c(pp|xx|c)?$/$self->{OBJ_EXT}/i, @o_files] ;
 $self->{'MTK'}   = $mTk;
 my $tk = installed_tk();
 my $perl = $self->{'PERL'};
 if ($IsWin32 && !-f $perl && -f "$perl.exe")
  {
   print "perl=$perl X=$^X\n";
   $perl = "$perl.exe";
   $self->{'PERL'} = $perl;
  }
 foreach my $file (sort keys %$mTk)
  {
   unless (-f $file && -M $file < -M $mTk->{$file})
    {
     warn "Extracting $file\n";
     system($perl,"$tk/pTk/Tcl-pTk",$mTk->{$file},$file);
    }
  }
}

my %visited;

sub abspath
{
 my $dir = shift;
 my $here = getcwd() || die "Cannot get current directory:$!";
 if (chdir($dir))
  {
   $dir = getcwd();
   chdir($here) || die "Cannot cd back to $here:$!";
  }
 return $dir;
}

sub relpath
{
 my ($path,$dir) = @_;
 unless (defined $dir)
  {
   $dir = (-d $path) ? $path : dirname($path);
  }
 if (defined $dir and -d $dir)
  {
   if ($path =~ m#^\Q$dir\E([/\\]?.*)$#)
    {
     my $base  = $1;
     my $here  = getcwd;
     if ($here =~ m#^\Q$dir\E([/\\]?.*)#)
      {
       my $depth = reverse($1);
       if ($depth)
	{
	 $depth =~ s,[^/\\]+,..,g;
	}
       else
	{
	 $depth = '.' ;
	}
       $depth =~ s,[/\\]+$,,;
       $base =~ s,^[/\\]+,,;
       $depth .= "/$base" if ($base);
       if (-e $depth)
	{
	 # print "$path is $depth from $here\n";
	 return $depth;
	}
       else
	{
	 warn "Cannot find $depth\n";
	}
      }
     else
      {
       unless(exists $visited{$here})
	{
	 $visited{$here} = 1;
	 warn "$here does not start with $dir\n";
	 warn "i.e. building outside Tk itself\n";
	}
      }
    }
   else
    {
     die "'$path' not under '$dir'\n";
    }
  }
 else
  {
   die "Cannot get directory for $path\n";
  }
 return $path;
}

use strict;

sub upgrade_pic
{
 my $flags = '';
 die 'upgrade_pic is obsolete';
 return $flags;
}

sub pasthru
{
 my $self = shift;
 my $str = $self->MM::pasthru;
 if ($str =~ s/^\s+INC=.*\n//m)
  {
   $str = "# - Do NOT pasthru INC for Tk - it is computed by subdir\n$str"
  }
 if ($str =~ s/\bLIB="\$\(LIB\)"//)
  {
   $str = qq[# - Drop LIB="\$(LIB)" - not used\n$str];
  }
 $str = "#Tk::MMutil pasthru\n$str";
 return $str;
}

sub perldepend
{
 my $self = shift;
 my $str = $self->MM::perldepend;
 my $name;
 my %c;
 foreach my $file (@{$self->{'C'}})
  {
   $c{$file} = 1;
  }
 foreach my $file (keys %{$self->{'XS'}})
  {
   $c{$file} = 1;
   delete $c{$self->{'XS'}{$file}};
  }
 my @files = grep(-f $_,sort(keys %c));
 if (@files)
  {
   my $tk = installed_tk();
   my @inc   = split(/\s+/,$self->{'INC'});
   my @def   = split(/\s+/,$self->{'DEFINE'});
   push(@def,qw(-DWIN32 -D__WIN32__)) if ($IsWin32);
   if ($^O eq 'cygwin')
    {
     push(@def,qw(-D__CYGWIN__));
     if ($win_arch eq 'MSWin32')
      {
       push(@def,qw(-D__WIN32__)) unless $self->{'DEFINE'} =~ /-D__WIN32__/;
       push(@def,qw(-DWIN32)) if $self->{'NAME'} eq 'Tk::pTk';
       push(@def,qw(-dWINARCH_MSWIN32));
      }
     elsif ($win_arch eq 'x')
      {
       push(@def,qw(-U_WIN32));
       push(@def,qw(-DWINARCH_X));
      }
    }
   foreach (@inc)
    {
     s/\$\(TKDIR\)/$tk/g;
     warn "Odd:$_" if /\$\(/;
    }
   $str .= Tk::MakeDepend::command_line(@inc,@def,@files) unless ($ENV{'TKNOMAKEDEPEND'});
  }
 return $str;
}

sub const_config
{
 my $self = shift;
 my $name;
 foreach $name (grep /(%|\.(old|bak|q4|orig|rej))$/,keys %{$self->{PM}})
  {
   delete $self->{PM}->{$name};
  }
 my $flags = $self->{'CCCDLFLAGS'};
 $flags =~ s/(-[fK]?\s*)pic\b/${1}PIC/;
 $self->{'CCCDLFLAGS'} = $flags;
 if ($^O eq 'MSWin32' && $Config{'ccflags'} =~ /-DPERL_OBJECT/)
  {
   $self->{'LDFLAGS'} =~ s/-(debug|pdb:\w+)\s+//g;
   $self->{'LDDLFLAGS'} =~ s/-(debug|pdb:\w+)\s+//g;
  }
 elsif ($^O eq 'darwin' )
  {
   $self->{'LDDLFLAGS'} =~ s/-flat_namespace//;
   $self->{'LDDLFLAGS'} =~ s/-undefined\s+suppress//;
## These lines seem to be not necessary for Panther, both
## builds with and without shared libperl, and seem to
## be dangerous for other MacOSX versions using perl builds
## without shared libperl, so disabled completely.
## See http://rt.cpan.org/Public/Bug/Display.html?id=39593
#   if ( -e "$Config{'archlib'}/CORE/$Config{'libperl'}" ) {
#    $self->{'LDDLFLAGS'} .= " -L\${PERL_ARCHLIB}/CORE -lperl ";
#   }
#   elsif ( -e "/System/Library/Perl/darwin/CORE/libperl.dylib" ) {
#    $self->{'LDDLFLAGS'} .= " -L/System/Library/Perl/darwin/CORE -lperl ";
#   }
#   else {
#    warn "Can't find libperl.dylib";
#   }
   $self->{'LDFLAGS'} =~ s/-flat_namespace//;
   $self->{'LDFLAGS'} =~ s/-undefined\s+suppress//;
  } elsif ($^O =~ /(openbsd)/i)
  {
   # -Bforcearchive is bad news for Tk - we don't want all of libpTk.a in all .so-s.
   $self->{'LDDLFLAGS'} =~ s/-Bforcearchive\s*//g;
  }
 return $self->MM::const_config;
}

sub constants
{
 my $self = shift;
 local $_ = $self->MM::constants;
 s/(\.SUFFIXES)/$1:\n$1/;
 $_ .= "\nGCCOPT = $Tk::Config::gccopt\n";
 if ($IsWin32)
  {
  }
 $_;
}

sub cflags
{
 my $self = shift;
 local $_ = $self->MM::cflags;
 if (0 && $IsWin32)
  {
   if ($Config::Config{cc} =~ /^bcc/i) {
     # s/(CCFLAGS\s*=)/$1/;
   }
   else {
     s/(CCFLAGS\s*=)/$1 \$(cflags) \$(cvarsdll)/;
     s/(OPTIMIZE\s*=).*/$1 \$(cdebug)/;
   }
  }
 $_;
}

sub c_o
{
 my $self = shift;
 local $_ = $self->MM::c_o;
 s/\$\(DEFINE\)/\$(DEFINE) \$(GCCOPT)/g;
 $_;
}

sub xs_o
{
 my $self = shift;
 local $_ = $self->MM::xs_o;
 s/\$\(DEFINE\)/\$(DEFINE) \$(GCCOPT)/g;
 $_;
}

sub manifypods
{
 my $self = shift;
 # Maybe always call UNIX version - we HTMLize them later
 local $_ = $self->MM::manifypods;
 if ($] >= 5.00565)
  {
   s/(POD2MAN_EXE.*pod2man.*)/$1 --center "perl\/Tk Documentation" --release "Tk\$(VERSION)"/;
  }
 elsif ($] >= 5.003)
  {
   s/(POD2MAN_EXE.*pod2man.*)/$1 -center "perl\/Tk Documentation" -release "Tk\$(VERSION)"/;
  }
 else
  {
   s/(POD2MAN_EXE.*pod2man.*)/$1 -center \\"perl\/Tk Documentation\\" -release \\"Tk\$(VERSION)\\"/;
  }
 s/\bpod::/Tk::/mg;
 s/\bpTk:://mg;
 $_;
}

sub findINC
{
 my $file = shift;
 my $dir;
 foreach $dir (@INC)
  {
   my $try = "$dir/$file";
   return $try if (-f $try);
  }
 die "Cannot find $file in \@INC\n";
}


sub makefile
{
 my $self = shift;
 my $str  = $self->MM::makefile;
 my $mm = findINC('Tk/MMutil.pm');
 my $cf = findINC('Tk/Config.pm');
 $str =~ s/(\$\(CONFIGDEP\))/$1 $cf $mm/;
 $str =~ s/\$\(OBJECT\)\s*:.*\n//;
 return $str;
}

sub installed_tk
{
 my $tk;
 my $dir;
 foreach $dir (@INC)
  {
   if (-f "$dir/tkGlue.h")
    {
     $tk = relpath($dir);
     last;
    }
   my $try = "$dir/Tk";
   if (-f "$try/tkGlue.h")
    {
     $tk = relpath($try,$dir);
     last;
    }
  }
 die "Cannot find perl/Tk include files\n" unless (defined $tk);
 $tk =~ s,^(\./)+,,;
 return $tk;
}

sub installbin
{
 my ($self) = @_;
 my $str  = $self->MM::installbin;
 my $prog = 'perl'; # $self->{'MAP_TARGET'} || 'perl';
 my $inc  = findINC('Tk/MMutil.pm');
 $inc =~ s,/Tk/MMutil.pm$,,;
 $inc = relpath($inc);
 $str =~ s/^\tcp\s/\t\$(PERL) -I$inc -MTk::install -e installbin $prog /mg;
 return $str;
}

sub findpTk
{
 my $ptk;
 my $dir;
 foreach $dir (map(abspath($_),@_),@INC)
  {
   my $try = "$dir/pTk";
   if (-d $try && (-f "$try/Lang.h" || -f "$try/libpTk\$(LIB_EXT)"))
    {
     $ptk = relpath($try,$dir);
     last;
    }
  }
 confess "Cannot locate pTk\n" unless (defined $ptk);
 return $ptk;
}

sub find_subdir
{
 my %dir;
 opendir(DIR,'.') || die "Cannot opendir:$!";
 foreach my $dir (readdir(DIR))
  {
   next if $dir =~ /^\.\.?$/;
   next if -l $dir;
   next unless -d $dir;
   if (-f "$dir/Makefile.PL")
    {
     my $exc = ($win_arch eq 'x') ? 'Unix' : 'Win';
     if (-f "$dir/Not${exc}.exc")
      {
       warn "Skip $dir on $win_arch\n"
      }
     else
      {
       $dir{$dir} = 1
      }
    }
  }
 closedir(DIR);
 return \%dir;
}

sub TkExtMakefile
{
 my (%att) = @_;
 if ($Config{'ccflags'} =~ /-DPERL_OBJECT/)
  {
   $att{'CAPI'} = 'TRUE' unless exists $att{'CAPI'};
  }
 unless (exists $att{'DIR'})
  {
   my $dir = find_subdir();
   $att{'DIR'} = [sort(keys %$dir)];
  }
 unless (exists $att{'NAME'})
  {
   my $dir = getcwd;
   my ($pack) = $dir =~ m#/([^/]+)$#;
   if (defined $pack)
    {
     $att{NAME} = 'Tk::'.$pack;
    }
   else
    {
     warn "No Name and cannot deduce from '$dir'";
    }
  }
 my $tk = installed_tk();
 $att{'macro'} = {} unless (exists $att{'macro'});
 $att{'macro'}{'TKDIR'} = $tk;
 my @opt = ('VERSION'     => $Tk::Config::VERSION,
	    'XS_VERSION'  => $Tk::Config::VERSION);
 push(@opt,'clean' => {} ) unless (exists $att{'clean'});
 $att{'clean'}->{FILES} = '' unless (exists $att{'clean'}->{FILES});
 $att{'clean'}->{FILES} .= ' *.bak';
 unless (exists($att{'linkext'}) && $att{linkext}{LINKTYPE} eq '')
  {
   my $ptk = findpTk($tk);
   my @tm = (findINC('Tk/typemap'));
   unshift(@tm,@{$att{'TYPEMAPS'}}) if (exists $att{'TYPEMAPS'});
   $att{'TYPEMAPS'} = \@tm;
   my $i = delete ($att{'INC'});
   $i = (defined $i) ? "$i $inc" : $inc;
   if (delete $att{'dynamic_ptk'})
    {
     push(@opt,
	  'MYEXTLIB' => "$ptk/libpTk\$(LIB_EXT)",
#         'dynamic_lib' => { INST_DYNAMIC_DEP => "$ptk/libpTk\$(LIB_EXT)" }
	 );
    }
   # Several loadable widgets use things from -lm
   # if platform does not have a shared -lm need to link against it
   if ($Config{libs} =~/-lm\b/)
    {
     my $libs = $att{'LIBS'}->[0];
     $att{'LIBS'}->[0] = "$libs -lm" unless $libs =~ /-lm\b/;
    }
   if ($IsWin32 && $Config{'cc'} =~ /^bcc/)
    {
     # Borland compiler is very dumb at finding files
     $i = "-I$tk $i";
     $i = "-I$ptk $i";
    }
   if ($IsWin32 && $Config{'cc'} =~ /gcc/i)
    {
     my $base  = $Config{'libpth'};
     #$base =~ s#lib$#i386-mingw32/lib#;
     my $extra = "-L$base -limm32 -lcomctl32 -lcomdlg32 -lgdi32";
     my $libs = $att{'LIBS'}->[0];
     $att{'LIBS'}->[0] = "$extra $libs";
    }
   if ($^O eq 'cygwin')
    {
     # NOTE: use gcc -shared instead of dllwrap (ld2),
     # dllwrap tries to resolve all symbols, even those
     # that are brought in from libraries like libpTk.a
     push(@opt,'LD' => 'gcc -shared');
     if ($win_arch eq 'MSWin32')
      {
       my $extra = "-L/lib/w32api -limm32 -lcomctl32 -lcomdlg32 -lgdi32";
       my $libs = $att{'LIBS'}->[0];
       $att{'LIBS'}->[0] = "$extra $libs";
       $att{'DEFINE'} .= ' -D__WIN32__ -D_WIN32 -DWINARCH_MSWIN32';
       $att{'DEFINE'} .= ' -DWIN32' if($att{'NAME'} eq 'Tk::pTk');
      }
     elsif ($win_arch eq 'x')
      {
       $att{'DEFINE'} .= ' -U_WIN32 -DWINARCH_X';
      }
    }
   if (delete $att{'ptk_include'})
    {
     $i = "-I$ptk $i" unless ($ptk eq '.');
    }
   else
    {
     $i = "-I$tk $i" unless ($tk eq '.');
    }
   push(@opt,'DEFINE' => $define, 'INC' => $i);
  }
 WriteMakefile(@opt, %att);
}

sub import
{
 no strict 'refs';
 my $class = shift;
 my @list = (@_) ? @_ : @{"${class}::MYEXPORT"};
 my $name;
 foreach $name (@list)
  {
   *{"MY::$name"} = \&{"$name"};
  }
}


1;
