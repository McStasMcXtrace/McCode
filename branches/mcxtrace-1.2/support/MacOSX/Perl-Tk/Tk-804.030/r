#!/tools/local/perl -w
use strict;
my $class = (@ARGV) ? shift : 'Tk';
my @elem  = split(/::/,$class);
my $path  = 'blib/arch/auto/'.join('/',@elem,$elem[-1].'.so');
warn "Reading $path\n";
open(F,"nm -p $path |");
my @syms;
while (<F>)
 {
  if (/ U / && /\b((?:[Tt]cl|Lang)\w+)/)
   {
    push(@syms,$1);
   }
  if (/ f / && /\b(\w+\.c)/)
   {
    push(@syms,$1);
   }
 }
close(F);

foreach my $sym (sort @syms)
{
 print $sym,"\n";
}


