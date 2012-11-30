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
  chmod(0666,$file) unless -w $file;
  unlink($file);
 }

if ($Tk::MMtry::VERSION ge '4.007' &&
    try_run("config/has_png.c",['-I/usr/local/include'],['-lpng -lz -lm']))
 {
  Tk::MMutil::TkExtMakefile(
       NAME         => 'Tk::PNG',
       VERSION_FROM => 'PNG.pm',
       XS_VERSION   => $Tk::Config::VERSION,
       dist         => { COMPRESS => 'gzip -f9', SUFFIX => '.gz' },
       OBJECT       => '$(O_FILES)',
       INC          => '-I/usr/local/include',
       LIBS         => ['-lpng -lz -lm'],
     );
 }
else
 {
  my $ok = 1;
  foreach my $file ("libpng/Makefile.PL","zlib/Makefile.PL")
   {
    next unless -f $file;
    chmod(0666,$file);
    unlink($file);
   }
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
 qq{"-e" "checked_test_harness('\$(TKDIR)/xt/skip_all.t', \$(TEST_VERBOSE), '\$(INST_LIB)', '\$(INST_ARCHLIB)')" $tests\n};
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



