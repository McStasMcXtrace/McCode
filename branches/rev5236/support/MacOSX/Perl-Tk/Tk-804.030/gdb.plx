#!/usr/local/bin/perl -w
use Config;
open(INIT,">.gdbinit") || die;
my $perl = $^X;
$perl = path_find($perl) unless (-f $perl);
foreach (@ARGV)
 {
  if (/[\s"]/)
   {
    s/"/\\"/g;
    $_ = '"'.$_.'"';
   }
 }
my $args = join(' ',@ARGV);
print INIT <<"END" if ($^O eq 'MSWin32');
break RunPerl;
run -Mblib $args
shared
break Perl_croak
break Perl_warn
break Perl_die
break XS_DynaLoader_dl_find_symbol
END
print INIT <<"END" unless ($^O eq 'MSWin32');
handle SIGWINCH pass nostop noprint
break XS_DynaLoader_dl_find_symbol
run -Mblib $args
shared
break Perl_croak
break Perl_warn
break Perl_die
END
close(INIT);
$perl =~ s#\\#/#g;
print join(' ','gdb',$perl),"\n";
system('gdb',$perl);
exit(0);

sub path_find
{
 my $prog = shift;
 foreach my $dir ('.',split($Config{'path_sep'},$ENV{'PATH'}))
  {
   my $try = "$dir/$prog";
   warn "Try $try\n";
   return $try if -f $try;
   $try .= $Config{'exe_ext'};
   return $try if -f $try;
  }
 return $prog;
}