#!/usr/local/bin/perl

use PGPLOT;

print "\n\nTesting scalars in array routines...\n\n";

print "PGPLOT module version $PGPLOT::VERSION\n\n";

pgqinf("VERSION",$val,$len);
print "PGPLOT $val library\n\n";

$dev = "?" unless defined $dev; # "?" will prompt for device

pgbegin(0,$dev,1,1);     # Open plot device 

pgscf(2);
pgslw(4);
pgsch(1.6);

pgenv(10.0,30.0,-2.0,6.0,0,0);

pgsci(6);

pglabel("X axis \\gP","Y axis \\gF","Top Label \\gW");

pgsci(7);


pgbbuf;

for($i=0; $i<10; $i++) {
   $x = $i+15;
   $y = $i-1;
   $e = 0.9;
   $x1 = $x - $e;
   $x2 = $x + 2.0* $e;
   $y1 = $y - 0.7* $e;
   $y2 = $y + 0.3* $e;
   pgsci(7);  
   pgpoint(1,$x,$y,$i+5);

   pgsci(3);  
   pgerrb(3,1,$x,$y,4,3.0);
   pgsci(2);

   pgerrx(1,$x1,$x2,$y,1);
   pgerry(1,$x,$y2,$y1,.1);
}

pgebuf;


pgiden;

$len=1; # -w fudge

pgend;
