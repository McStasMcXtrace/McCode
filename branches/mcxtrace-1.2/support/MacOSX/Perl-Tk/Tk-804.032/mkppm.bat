@perl -Sx %0 "%*"
@goto endofperl
#!perl -w
open(PPD,"Tk.ppd") || die "Cannot open Tk.ppd:$!";
my $tar;
while (<PPD>)
 {
  if (/<CODEBASE HREF="(.*)"/)
   {
    $tar = $1;
    last;
   }
 }
close(PPD);
if (defined $tar)
 {
  if ($tar =~ m#(.*)/#)
   {
    mkdir($1,0777);
   }
  $tar =~ s/\.gz$//;
  if (system("tar","cvf",$tar,'blib') == 0)
   {
    if (system("gzip","-9f",$tar) == 0)
     {
     }
   }
  else
   {
    die "Cannot tar $tar\n"
   }
 }
__END__
:endofperl
