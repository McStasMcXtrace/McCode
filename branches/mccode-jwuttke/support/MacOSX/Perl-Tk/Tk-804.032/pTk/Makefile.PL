# -*- cperl -*-
use Config;
use ExtUtils::MakeMaker;

use Tk::MMtry qw(try_compile);
use Tk::MMutil;
use Tk::Config '!$VERSION';

Tk::MMutil::TkExtMakefile(
    'NAME'	=> 'Tk::pTk',
    'VERSION'	=> $Tk::Config::VERSION,
    'LINKTYPE'	=> 'static',
    'OBJECT'	=> '$(O_FILES)',
    macro	=> { WINARCH     => $win_arch },
    'DEFINE'	=> $define,
    'INC'	=> "$inc -I. -Ibitmaps",
    'SKIP'	=> [qw( distclean dist makeaperl xs_o)],
    'clean'	=> {'FILES' => 'libpTk$(LIB_EXT)'}
);

sub MY::top_targets {
 my ($self) = @_;
 local $_ = $self->MM::top_targets;
 s/^(\$\(O_FILES\)\s*:.*)$/# Explicit dependencies provided\n# $1/m;
 return $_;
}

sub MY::xs_c {
'
# Rules for building .t table files from .h files
.SUFFIXES: .t .h .m
.h.t :
	$(PERL) mkVFunc -t $(WINARCH) $<
.h.m :
	$(PERL) mkVFunc -m $(WINARCH) $<
'
}

sub MY::post_initialize
{
 my ($self) = @_;
 my %mTk;
 my %exc;
 my $dir;
 my @list;
 if ($Tk::MMutil::IsWin32)
  {
   @list = qw(win xlib additions generic tixWin tixGeneric tclWin tclGeneric);
  }
 elsif ($win_arch eq 'MSWin32' and $^O eq 'cygwin')
  {
   @list = qw(win xlib additions generic tixWin tixGeneric tclUnix tclGeneric);
  }
 elsif ($win_arch eq 'open32')
  {
   @list = qw(open32 open32/h win xlib additions generic tixWin tixGeneric tclUnix tclGeneric );
  }
 elsif ($win_arch eq 'pm')
  {
   @list = qw(os2 os2/unix open32 xlib additions generic tixWin tixGeneric tclUnix unix tclGeneric );
  }
 else
  {
   @list = qw(unix additions generic tixUnix tixGeneric tclUnix tclGeneric );
  }
 print STDERR "Generating Dependencies for $^O, win_arch=$win_arch\n";
 foreach $dir (@list)
  {
   my $src = $self->catdir("mTk",$dir);
   next unless -d $src;
   if (open(EXC,$self->catfile($src,"pTk.inc")))
    {
     while (<EXC>)
      {
       chomp;
       s/\s*#.*$//;
       s/^\s+//;
       next unless /\S/;
       $mTk{$_} = $self->catfile($src,$_) unless (exists $mTk{$_});
      }
     close(EXC);
    }
   else
    {
     opendir(DIR,$src) || die "Cannot open $src:$!";
     if (open(EXC,$self->catfile($src,"pTk.exc")))
      {
       while (<EXC>)
        {
         chomp;
         s/\s*#.*$//;
         s/^\s+//;
         next unless /\S/;
         $exc{$_} = 1;
         if (-f $_)
          {
           chmod(0777,$_);
           unlink($_);
           warn "Loose $_\n";
          }
        }
       close(EXC);
      }
     else
      {
       warn "Cannot open $src/pTk.exc:$!";
      }
     my $file;
     while (defined($file = readdir(DIR)))
      {
       next if $exc{$file};
       if ($file =~ /\.[ch]$/)
        {
         $mTk{$file} = $self->catfile($src,$file) unless (exists $mTk{$file});
        }
      }
     closedir(DIR);
    }
  }
 my @loose = ();
 if (defined($Config{'i_stdlib'}) && try_compile("config/Hstrtoul.c"))
  {
   push(@loose,'strtoul.c');
  }
 if (defined($Config{'i_string'}) && try_compile("config/Hstrdup.c"))
  {
   push(@loose,'strdup.c');
  }
 if (try_compile("config/Hstrcasecmp.c"))
  {
   push(@loose,'strcasecmp.c');
  }
 $self->Tk::MMutil::mTk_CHO(\%mTk,\%exc,@loose);
 my %files = ();
 $files{'windows.h'} = 1 if $win_arch eq 'open32';
 my $name;
 $self->{'dir_targets'} = [];
 $dir = $self->catdir('$(INST_ARCHLIBDIR)','pTk');
 push(@{$self->{'dir_targets'}},$dir);
 foreach $name ($self->lsdir("."))
  {
   next if ($name =~ /^\./);
   next unless (-f $name);
   $files{$name} = 1 if ($name =~ /\.[tm]$/);
  }
 foreach $name (sort(@{$self->{H}},keys %files))
  {
   $self->{PM}->{$name} = $self->catfile($dir,$name);
  }
 foreach $name (qw(Tcl-pTk mkVFunc))
  {
   $self->{PM}->{$name} = $self->catfile($dir,$name);
  }

 if ($Tk::MMutil::IsWin32 or $win_arch eq 'open32' or $win_arch eq 'pm' or
     ($win_arch eq 'MSWin32' and $^O eq 'cygwin'))
  {my $ddir = $self->catdir('$(INST_ARCHLIBDIR)','X11');
   my $sdir = $self->catdir('mTk','xlib','X11');
   push(@{$self->{'dir_targets'}},$ddir);
   foreach $name ($self->lsdir($sdir))
    {
     next if $name =~ /^\.+$/;
     my $src = $self->catfile($sdir,$name);
     next unless -f $src;
     $self->{PM}->{$src} = $self->catfile($ddir,$name);
    }
  }
 $dir = $self->catdir($dir,"compat");
 push(@{$self->{'dir_targets'}},$dir);
 foreach $name ($self->lsdir("compat"))
  {
   next if $name =~ /^\.+$/;
   my $src = $self->catfile('compat',$name);
   next unless -f $src;
   $self->{PM}->{$src} = $self->catfile($dir,$name);
  }
 '';
}

sub MY::test
{
 q[
test ::
	@echo 'No tests defined for $(NAME)'
 ];
}

sub MY::post_constants
{
 '
INST_STATIC=libpTk$(LIB_EXT)
';
};

sub MY::install {
'
install :: all
';
}

sub MY::dynamic_lib { my $self = shift; "dynamic_lib :: static_lib\n\t".$self->{'NOECHO'}."\$(NOOP)\n" }

sub MY::dynamic     { my $self = shift; "dynamic :: static\n\t".$self->{'NOECHO'}."\$(NOOP)\n" }

sub MY::static
{
  my $self = shift;
  my $str  = $self->MM::static(@_);
  if ($win_arch eq 'open32' or $win_arch eq 'pm')
   {
    $str .= '
static :: dllInit$(LIB_EXT)
dllInit$(LIB_EXT): dllMain$(OBJ_EXT)
	$(AR) $(AR_STATIC_ARGS) $@ dllMain$(OBJ_EXT) && $(RANLIB) $@
';
   }
 return $str;
}

sub MY::realclean {
  my $self = shift;
  my $str  = $self->MM::realclean(@_);
  $str =~ s/(::\s*clean)/$1 mungeclean/;
  $str .= "\nmungeclean ::\n";
  $str .= "\t\$(RM_F) ".join(" \\\n\t",keys %{$self->{'MTK'}})."\n";
  return $str;
}


sub MY::postamble {
my $self = shift;
my $dep = $self->Tk::MMutil::mTk_postamble;

if (0 && $self->can('dir_target'))
 {
  $dep .= $self->dir_target(@{$self->{'dir_targets'}});

  $dep .= "config :: " . join(" \\\n\t",map($self->catfile($_,".exists"),@{$self->{'dir_targets'}})) .
          "\n\t".$self->{NOECHO}."\$(NOOP)\n";
 }

if ($Tk::MMutil::IsWin32 or ($win_arch eq 'MSWin32' and $^O eq 'cygwin'))
 {
  my $cc = $Config{'cc'};
  my $file = 'tk.res';
  if ($cc =~ /gcc/i)
   {
    $file = 'tkres$(OBJ_EXT)';
    $dep .= "$file : mTk/win/rc/tk.rc\n\t";
    $dep .= 'windres --use-temp-file -o $@ --include-dir . --include-dir mTk/win/rc mTk/win/rc/tk.rc';
   }
  elsif ($cc =~ /^bcc/i)
   {
    $dep .= "$file : mTk/win/rc/tk.rc\n\t";
    $dep .= 'brc32 -fo $@ -r -i . -i mTk\win\rc mTk\win\rc\tk.rc';
   }
  else
   {
    $dep .= "$file : mTk/win/rc/tk.rc\n\t";
    $dep .= 'rc -fo $@ -r -i . -i mTk\win\rc mTk\win\rc\tk.rc';
   }
  $dep .= "\nlibpTk\$(LIB_EXT) : $file\n";
 }
$dep . '
Lang.t  Lang.m : Lang.h  Lang.exc  mkVFunc
tk.t tk.m : tk.h    tk.exc    mkVFunc
tkInt.t tkInt.m : tkInt.h tkInt.exc mkVFunc
tkImgPhoto.t tkImgPhoto.m : tkImgPhoto.h tkImgPhoto.exc mkVFunc
Xlib.t Xlib.m : Xlib.h  Xlib.excwin mkVFunc
tix.t tix.m   : tix.h tix.exc mkVFunc
tixInt.t tixInt.m : tixInt.h tixInt.exc mkVFunc
tkDecls.t tkDecls.m : tkDecls.h tkDecls.exc mkVFunc
tclDecls.t tclDecls.m : tclDecls.h tclDecls.exc mkVFunc
tkIntDecls.t tkIntDecls.m : tkIntDecls.h tkIntDecls.exc mkVFunc

config :: Xlib.t Lang.t tk.t tkInt.t tkImgPhoto.t tix.t tixInt.t \
          tkDecls.t tkIntDecls.t tclDecls.t \
          Xlib.m Lang.m tk.m tkInt.m tkImgPhoto.m tix.m tixInt.m \
          tkDecls.m tkIntDecls.m tclDecls.m
	'.$self->{NOECHO}.'$(NOOP)

';
}





