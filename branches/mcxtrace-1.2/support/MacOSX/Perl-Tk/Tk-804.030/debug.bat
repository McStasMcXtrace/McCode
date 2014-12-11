@perl -Sx %0.bat %*
@goto end_of_perl
#!perl -w
my $perl = $^X;
$perl .= '.exe' unless $perl =~ /\.exe$/i;
unless (-x $^X)
 {
  my $leaf = $perl;
  foreach my $dir (split(/;/,$ENV{'Path'}))
   {
    my $try = "$dir/$leaf";
    if (-x $try)
     {
      $perl = $try;
      $perl =~ tr,\\,/,;
      last;
     }
   }
 }
print join(' ',$perl,$0,@ARGV),"\n";
system("gdb $perl");
__END__
:end_of_perl