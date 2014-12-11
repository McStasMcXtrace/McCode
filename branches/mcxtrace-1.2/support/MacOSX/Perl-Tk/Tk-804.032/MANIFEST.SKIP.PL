#!/usr/local/bin/nperl -w
use File::Find;
use File::Basename;

my %skip;
my %files;
my %maybe;

sub wanted
{
 my $s = $File::Find::name;
 $s =~ s#^\./##;
 return if $s =~ m#^\.pc/#; # quilt state dir
 $files{$s} = 1 if (-f $_);
 if (/\.xs$/)
  {
   $s =~ s#\.xs$#.c#;
   $skip{$s} = 1;
  }
 if ($s =~ m#^((.*)/mTk/.*?)/([^/]+\.[ch])$#x)
  {
   $maybe{$1}{$3} = $2 unless $_ eq 'Xlib.h';
  }
}

find(\&wanted,'.');

foreach my $dir (sort keys %maybe)
 {
  if (open(INC,"$dir/pTk.inc"))
   {
    warn "Reading $dir/pTk.inc\n";
    my %only;
    while (<INC>)
     {
      $only{$1} = 1 if (/(\S+)/);
     }
    close(INC);
    foreach my $name (keys %{$maybe{$dir}})
     {
      delete $maybe{$dir}{$name} unless exists $only{$name};
     }
   }
  elsif (open(EXC,"$dir/pTk.exc"))
   {
    warn "Reading $dir/pTk.exc\n";
    while (<EXC>)
     {
      delete $maybe{$dir}{$1} if /(\S+)/;
     }
    close(EXC);
   }
  foreach my $name (sort keys %{$maybe{$dir}})
   {
    my $wh = $maybe{$dir}{$name};
    $skip{"$wh/$name"} = 1;
   }
 }


chmod(0644,"MANIFEST.SKIP");
open(SKIP,">MANIFEST.SKIP") || die "Cannot open MANIFEST.SKIP:$!";
print SKIP <<'EOF'
\.[oaid]$
\.(bso?|old|bak|bck|base|exp|broke|out|hide|q4|gz|tcl|rtf|orig)$
^\.
/\.
C\+\+/Main$
\bcore$
\bConfig.pm$
\bpTk/tkConfig\.h$
\bpTk/(need|canvas_sources)$
\b(pm_to_blib|blibdirs|perl|wedge|efdebug)$
[%~]$
\b(blib|merge|Ilya|Pixmap|patches|pod2)/
\bTix/
tcl\.pm$
\bcore\b
Canvas/diag\.txt$
pod/html$
\bdoc/
Tk.ppd$
\b(ToDo|learn|Pending|Obsolete)/
\b(GNU)?[Mm]akefile(\.aperl)?$
-bug$
\bperlmain\.c$
\btkperl\b
\bHTML/doc\b
\bEvent/Play\b
\bEvent/fe
pod/tkman.ps$
extralibs\.ld
doc/index\.html$
doc/MANFILES$
doc/.*\.htm$
myConfig\.out
\bMYMETA.yml$
\bMYMETA.json$
JPEG/jpeg/testout*
JPEG/jpeg/config\.(log|status)
JPEG/jpeg/([cd]jpeg|jpegtran|(rd|wr)jpgcom)
JPEG/jpeg/Makefile\.PL$
PNG/libpng/Makefile\.PL$
PNG/libpng/pngtest$
PNG/libpng/config.h$
PNG/libpng/config.log$
PNG/libpng/config.status$
PNG/libpng/libpng-config$
PNG/libpng/libpng.pc$
PNG/libpng/libtool$
PNG/libpng/stamp-h1$
PNG/zlib/example$
PNG/zlib/Makefile\.PL$
PNG/zlib/minigzip$
\.todo$
\bswapm$
bin/patchls$
JPEG/jpeg/jconfig.h$
prep_rel$
EOF
;
foreach (sort keys %skip)
 {
  s#\.#\\.#;
  print SKIP "$_\$\n";
 }
close(SKIP);
