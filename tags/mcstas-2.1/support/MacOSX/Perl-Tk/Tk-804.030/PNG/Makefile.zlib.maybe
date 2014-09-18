#!perl
use Cwd;
use Config;
use File::Copy;
#warn __FILE__." in ".getcwd()."\n";
if ($^O eq 'MSWin32')
 {
  my $file;
  chmod(0666,'Makefile');
  if ($Config{'cc'} =~ /gcc/)
   {
    $file = "win32/Makefile.gcc";
   }
  else
   {
    $file = 'win32/Makefile.msc';
    warn "Assuming ".$Config{'cc'}." is visual C of some kind\n";
   }
  copy($file,"Makefile")
   || die "Cannot copy $file to Makefile: $!";

  # overwrite zconf.h with original zconf.in.h
  copy("zconf.in.h", "zconf.h")
   || die "Cannot copy zconf.in.h to zconf.h: $!";
 }
else
 {
  $ENV{CC} = $Config{cc};
  local $ENV{CFLAGS} = "$Config{ccflags} $Config{cccdlflags}";
  system(sh => "./configure");
 }
1;
