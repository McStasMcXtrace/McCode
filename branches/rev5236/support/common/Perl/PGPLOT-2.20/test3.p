#!/usr/local/bin/perl

use PGPLOT;

print "\n\nTesting histogram routines...\n\n";

print "PGPLOT module version $PGPLOT::VERSION\n\n";

pgqinf("VERSION",$val,$len);
print "PGPLOT $val library\n\n";

$i=0; @data=();
while(<DATA>){
  $i++;
  chop;
  $data[$i-1] = $_;
}

$dev = "?" unless defined $dev;

pgbegin(0,$dev,1,1);

pgscf(2);
pgslw(4);
pgsch(1.6);

pgsci(6);

pgsci(7);

pghist($i,\@data,0,10,10,2);

pglabel("Data Value","Number of data items","Test histogram");

$len = 1; # -w fudge

pgend;

__DATA__
1
1
2
3
4
7
3
5
7
3
5
6
2
2
2
2
1
6
7
