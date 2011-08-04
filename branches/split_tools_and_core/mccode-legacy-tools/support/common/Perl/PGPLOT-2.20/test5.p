#!/usr/local/bin/perl

use PGPLOT;

print "\n\nTesting perl function passing to pgfunx, pgfunx, pgfunt...\n\n";

print "PGPLOT module version $PGPLOT::VERSION\n\n";

pgqinf("VERSION",$val,$len);
print "PGPLOT $val library\n\n";

$dev = "?" unless defined $dev;

pgbegin(0,$dev,2,2);

pgsci(3); pgscf(2); pgsch(1.4);

$pi=3.141592654;

# Anonymous subs!

pgfunx(sub{ sqrt($_[0]) },  500, 0, 10, 0);
pgfuny(sub{ sin(4*$_[0]) }, 360, 0, 2*$pi, 0);

# Pass by name and pass by reference

pgfunt("funt_x",  "funt_y",  360,0, 2*$pi, 0);
pgfunt(\&funt_x2, \&funt_y2, 360,0, 2*$pi, 0);

$len=1; # -w fudge

pgend;

sub funt_x {
   my($t)=$_[0];
   return cos($t);;
}

sub funt_y {
   my($t)=$_[0];
   return sin($t);
}

sub funt_x2 {
   my($t)=$_[0];
   return cos(4*$t)*cos($t);;
}

sub funt_y2 {
   my($t)=$_[0];
   return cos(4*$t)*sin($t);
}
