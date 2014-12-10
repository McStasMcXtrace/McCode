# -*- cperl -*-
use strict;
use Tk::MMutil;
use Tk::Config ();
use Tk::MMtry;
use File::Copy;

my $l = $Config::Config{'lib_ext'};
my $cc = $Config::Config{'cc'};
my $zlib = "libz$l";

foreach my $file (qw(zlib/Makefile.PL libpng/Makefile.PL))
 {
  next unless -f $file;
  chmod(0666,$file) unless -w $file;
  unlink($file);
 }

my $all_cflags;
my $all_libs;
{
 my $libpng_cflags;
 my $libpng_libs;
 system('pkg-config --exists libpng');
 if ($? == 0)
  {
   chomp($libpng_cflags = `pkg-config --cflags libpng`);
   chomp($libpng_libs   = `pkg-config --libs libpng`);
  }
 else
  {
   warn "'pkg-config libpng' failed, continue with fallback values for cflags and libs...\n";
   $libpng_cflags = '-I/usr/local/include';
   $libpng_libs   = '-lpng -lm';
  }

 my $zlib_cflags;
 my $zlib_libs;
 system('pkg-config --exists zlib');
 if ($? == 0)
  {
   chomp($zlib_cflags = `pkg-config --cflags zlib`);
   chomp($zlib_libs   = `pkg-config --libs zlib`);
  }
 else
  {
   warn "'pkg-config zlib' failed, continue with fallback values for cflags and libs...\n";
   $zlib_cflags = '-I/usr/local/include';
   $zlib_libs   = '-lz';
  }

 $all_cflags = "$libpng_cflags $zlib_cflags";
 $all_libs   = "$libpng_libs $zlib_libs";
}

if ($Tk::MMtry::VERSION ge '4.007' &&
    try_run("config/has_png.c",[$all_cflags],[$all_libs]))
 {
  warn "Using system's -lpng\n";
  Tk::MMutil::TkExtMakefile(
       NAME         => 'Tk::PNG',
       VERSION_FROM => 'PNG.pm',
       XS_VERSION   => $Tk::Config::VERSION,
       dist         => { COMPRESS => 'gzip -f9', SUFFIX => '.gz' },
       OBJECT       => '$(O_FILES)',
       INC          => $all_cflags,
       LIBS         => [$all_libs],
     );
 }
else
 {
  my $ok = 1;
  unless (copy("Makefile.libpng.maybe","libpng/Makefile.PL"))
   {
    warn "Cannot copy Makefile.libpng.maybe => libpng/Makefile.PL ($!)";
    $ok = 0;
   }
  unless (copy("Makefile.zlib.maybe","zlib/Makefile.PL"))
   {
    warn "Cannot copy Makefile.zlib.maybe => zlib/Makefile.PL ($!)";
    $ok = 0;
   }
  if ($ok)
   {
    warn "Building libpng$l\n";
    my $extlib = "libpng/libpng$l";
    if ($cc eq 'cl')
     {
      $extlib .= " zlib/zlib$l";
      $zlib   = "zlib$l";
     }
    else
     {
      $extlib .= " zlib/libz$l";
     }
    Tk::MMutil::TkExtMakefile(
           NAME         => 'Tk::PNG',
           VERSION_FROM => 'PNG.pm',
           XS_VERSION   => $Tk::Config::VERSION,
           dist         => { COMPRESS => 'gzip -f9', SUFFIX => '.gz' },
           OBJECT       => '$(O_FILES)',
           INC          => '-Ilibpng -Izlib',
           MYEXTLIB     => $extlib,
           LIBS         => ['-lm'],
         );
    }
   else
    {
     warn "Cannot build libpng$l\n";
     ExtUtils::MakeMaker::WriteEmptyMakefile();
    }
 }

sub MY::test_via_harness
{
 my($self, $perl, $tests) = @_;
 qq{\t$perl "-I../t" "-MTkTest" }.
 qq{"-e" "checked_test_harness('\$(TKDIR)/xt', \$(TEST_VERBOSE), '\$(INST_LIB)', '\$(INST_ARCHLIB)')" $tests\n};
}

package MY;

sub pasthru {
  if ($^O eq 'MSWin32' && $Config::Config{cc} =~ /gcc/)
   {
    # gcc usually has ranlib, also dmake does not understand "rem"
    # which is default setting for $Config::Config{ranlib} under
    # Windows, unless .USE_SHELL: is specified (see also special_targets
    # in ExtUtils::MM_Win32)
    qq[PASTHRU="CC=$Config::Config{cc} $Config::Config{cccdlflags}"];
   }
  else
   {
    qq[PASTHRU="CC=$Config::Config{cc} $Config::Config{cccdlflags}" RANLIB=\"$Config::Config{ranlib}\"];
   }
}

sub postamble {
  my $s = '';
  if (-f "libpng/Makefile.PL")
   {
    $s .=
"
libpng/libpng$l : zlib/$zlib
	cd libpng && \$(MAKE) libpng".$l.' $(PASTHRU)
';

   }
  if (-f "zlib/Makefile.PL")
   {
    $s .=
"
zlib/$zlib : FORCE
	cd zlib && \$(MAKE) $zlib".' $(PASTHRU)
';

   }
 return $s;
}

sub realclean {
  my $self = shift;
  my $str  = $self->MM::realclean(@_);
  $str =~ s/(::\s*clean)/$1 realclean2/;
  $str .= "\nrealclean2 ::\n";
  $str .= "\t\$(RM_F) ".join(" \\\n\t","libpng/Makefile","libpng/Makefile.PL","zlib/Makefile","zlib/Makefile.PL")."\n";
  $str .= "\t\$(RM_F) ".join(" \\\n\t", map { "libpng/$_" } qw(
								  config.h
								  config.log
								  config.status
								  libpng-config
								  libpng.pc
								  libtool
								  stamp-h1
							     ))."\n";
  $str .= "\t\$(RM_RF) ".join(" \\\n\t","libpng/.deps")."\n";
  return $str;
}



