package Tk::install;
require Exporter;

use vars qw($VERSION @EXPORT);
$VERSION = '4.004'; # $Id: //depot/Tkutf8/Tk/install.pm#4 $

use base  qw(Exporter);
@EXPORT = qw(installbin);

use Config;

sub installbin
{
 my $prog  = shift(@ARGV);
 my $start = $Config{'startperl'};
 my $perl  = $Config{'perl'} || 'perl';
 $start =~ s/$perl$/$prog/;
 while (($src,$dst) = splice(@ARGV,0,2))
  {
   open(SRC,"<$src") || die "Cannot open $src:$!";
   my $line = <SRC>;
   $line =~ s/^#!\s*\S+/$start/;
   warn $line;
   chmod(0755,$dst) if (-f $dst);
   open(DST,">$dst") || die "Cannot open $dst:$!";
   print "installbin $src => $dst\n";
   do
    {
     print DST $line;
    } while (defined($line = <SRC>));
   close(SRC);
   close(DST);
   chmod(0555,$dst);
  }
}

1;
