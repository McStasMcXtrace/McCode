# -*- cperl -*-
use 5.007;
use open IO => ':bytes';
use Cwd;
use Config;
no  lib '.';
BEGIN
 {
  $IsWin32 = ($^O eq 'MSWin32' || $Config{'ccflags'} =~ /-D_?WIN32_?/);
  $xtra = "";

  open(M, "Tk.pm") or die "Can't open Tk.pm for reading VERSION: $!";
  while(<M>)
   {
    if (/\$Tk::VERSION\s+=\s+'([0-9._]+)'/)
     {
      $DISTVERSION = $1;
      ($VERSION = $DISTVERSION) =~ s{_}{};
      last;
     }
   }
  close M;
  if (!defined $VERSION)
   {
    die "Can't find \$Tk::VERSION in Tk.pm";
   }

  $win_arch = shift @ARGV if @ARGV and $ARGV[0] =~ /^(open32|pm|x|MSWin32)$/;
  require('fix_4_os2.pl'), OS2_massage() if $^O eq 'os2';

  # XXX cygwin should preferably use the MSWin32 windows architecture, but
  #     this is currently broken.
  $win_arch = $IsWin32 ? 'MSWin32' : 'x'
#  $win_arch = ($IsWin32 or $^O eq 'cygwin') ? 'MSWin32' : 'x'
    if not defined $win_arch; # Currently 'x', 'pm', 'open32', 'MSWin32'

  require "./myConfig";
  use lib ($Tk::MMutil::dir=getcwd);
 }

use Tk::MMutil;

if ($IsWin32)
 {
  *MY::makeaperl = \&makeWin32perl;
  @libs = ('-lcomctl32 -limm32');
 }
else
 {
  my $plibs = $Config{'libs'};
  my $libs  = "$xlib -lX11$xtra";   # Used to have -lpt in here as well.
  my @try   = qw(-lsocket -lnsl -lm);
  push(@try,'-lc') if $^O =~ /svr4/i;
  my $lib;
  # perl needs sockets and math library too
  # so only include these if they made it through perl's Configure
  foreach $lib (@try)
   {
    $libs .= " $lib" if ($plibs =~ /$lib\b/);
   }
  @libs = ("$libs");
 }

my $dir = Tk::MMutil::find_subdir();
delete $dir->{'pTk'};

my $eumm_recent_enough = $ExtUtils::MakeMaker::VERSION >= 6.54;

if (!$eumm_recent_enough) {
    *MY::dist_core = sub {
	<<'EOF';
dist :
	$(NOECHO) $(ECHO) "Sorry, use a newer EUMM!"

EOF
    };
}

Tk::MMutil::TkExtMakefile(
    'VERSION'  => $VERSION,
    'EXE_FILES' => [qw(ptksh ptked gedi)],
    'NAME'     => 'Tk',
    'DIR'      => ['pTk',reverse(sort(keys %$dir))],
    'DISTNAME' => "Tk",
    'DISTVNAME'=> "Tk-$DISTVERSION",
    'MYEXTLIB' => 'pTk/libpTk$(LIB_EXT)' . ($win_arch =~ /^(open32|pm)$/
					    ? ' pTk/dllInit$(LIB_EXT)'
					    : ''),
    'LIBS'    => \@libs,
    'OBJECT'  => '$(O_FILES)',
    'PREREQ_PM'  => { 'Encode' => 0,
		      'Test::More' => 0,
		    },
    'clean'      => { FILES => 'pTk/tkConfig.h Tk/Config.pm' },
    'LICENSE'    => 'unrestricted',

    ($eumm_recent_enough ?
      (META_ADD => { resources => { repository => 'http://github.com/eserte/perl-tk' } }) : ()),

    @macro
   );



sub MY::top_targets
{
 my ($self) = @_;
 my $str = $self->MM::top_targets;
 $str =~ s/\bmanifypods\b/html/g;
 return $str;
}

sub MY::post_initialize
{
 my ($self) = @_;
 my ($ret) = '';
 my %files = ();
 my $dir = $self->catdir('$(INST_ARCHLIBDIR)','Tk');
 my $name;
 foreach $name (grep /(%|\.q4|\.bck|\.old)$/,keys %{$self->{PM}})
  {
   delete $self->{PM}->{$name};
  }
 delete $self->{PM}->{'Tk/Config.pm'};
 $self->{PM}->{'Tk/Config.pm'} = $self->catfile($dir,'Config.pm');
 # $files{'typemap'} = 1;
 foreach $name ($self->lsdir("."))
  {
   next if ($name =~ /^\./);
   next unless (-f $name);
   $files{$name} = 1 if ($name =~ /\.[tm]$/);
   $files{$name} = 1 if ($name =~ /\.def$/);
  }
 foreach $name (sort(@{$self->{H}},keys %files))
  {
   $self->{PM}->{$name} = $self->catfile($dir,$name);
  }
 if ($IsWin32)
  {
   if ($Config{cc} =~ /^bcc/i) {
     $ret .= "\nRESFILES = pTk\\tk.res\n";
   }
   else {
     push(@{$self->{'O_FILES'}},
          ($Config{'cc'} =~ /gcc/i) ?
               'pTk\\tkres$(OBJ_EXT)' : 'pTk\\tk.res');
   }
  }
 if ($win_arch eq 'MSWin32' and $^O eq 'cygwin')
  {
     push(@{$self->{'O_FILES'}}, 'pTk/tkres$(OBJ_EXT)');
  }
 $ret;
}

sub needs_Test
{
 my $file = shift;
 local $_;
 unless (open(TFILE,"$file"))
  {
   warn "Cannot open $file:$!";
   return 1;
  }
 my $code = 0;
 while (<TFILE>)
  {
   last if ($code = /^\s*(use|require)\s+Test\b/);
  }
 close(TFILE);
 warn "Skipping test $file needs 'Test.pm':$_" if $code;
 return $code;
}

sub MY::test
{
 my ($self,%attrib) = @_;
 my @tests = sort glob($self->catfile('t','*.t'));
 eval { require Test };
 if ($@)
  {
   @tests = grep(!needs_Test($_),@tests);
  }
 $attrib{'TESTS'} = join(' ',@tests);

 # Temporarily remove sub-dirs from $self as we 'know'
 # there are no tests down there
 my $dir  = delete $self->{'DIR'};
 my @td;
 foreach my $sd (@$dir)
  {
   my @tests = sort glob($self->catfile($sd,'t','*.t'));
   if (@tests)
    {
     warn "Tests in $sd\n";
     push(@td,$sd);
    }
  }
 if (@td)
  {
   $self->{DIR} = \@td;
  }
 my $str  = $self->MM::test(%attrib);
 # Put sub-dirs back
 $self->{'DIR'} = $dir;
 return $str;
}

sub MY::test_via_harness
{
 my($self, $perl, $tests) = @_;
 qq{\t$perl "-It" "-MTkTest" }.
 qq{"-e" "checked_test_harness('\$(TKDIR)/xt', \$(TEST_VERBOSE), '\$(INST_LIB)', '\$(INST_ARCHLIB)')" $tests\n};
}

sub MY::postamble {
 my $str = '

html : subdirs manifypods
	@cd pod && $(MAKE) html $(PASTHRU)

config       :: tkGlue.t tkGlue.m
	@$(NOOP)

$(BASEEXT)$(OBJ_EXT) : tkGlue.h
Makefile     : myConfig

tkGlue.t     : tkGlue.h pTk/mkVFunc
	$(PERL) pTk/mkVFunc -t $(WINARCH) tkGlue.h

tkGlue.m     : tkGlue.h pTk/mkVFunc
	$(PERL) pTk/mkVFunc -m $(WINARCH) tkGlue.h

$(MYEXTLIB)  : config FORCE
	cd pTk && $(MAKE) DEFINE="$(DEFINE)" $(PASTHRU)

perlmain.c : config Makefile

pTk/tk.res : $(MYEXTLIB)

basic      : $(INST_DYNAMIC) pm_to_blib

MANIFEST : MANIFEST.SKIP $(FIRST_MAKEFILE)
	$(MAKE) manifest

';
 $str =~ s/DEFINE=.*// if($^O eq 'cygwin');
 $str;
}

{
 package MY;
 sub init_PM
 {
  my $self = shift;
  $self->SUPER::init_PM(@_);
  delete $self->{PM}->{"fix_4_os2.pl"};
 }
}
