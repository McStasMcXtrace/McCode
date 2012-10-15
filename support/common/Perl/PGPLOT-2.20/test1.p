#!/usr/local/bin/perl  

use PGPLOT;  # Load PGPLOT module

print "\nTesting simple point plot...\n\n";

print "PGPLOT module version $PGPLOT::VERSION\n\n";

pgqinf("VERSION",$val,$len);
print "PGPLOT $val library\n\n";

# "?" will prompt for device

$dev = "?" unless defined $dev; 

pgbegin(0,$dev,1,1);  # Open plot device 

pgscf(2);             # Set character font
pgslw(4);             # Set line width
pgsch(1.6);           # Set character height

# Define data limits and plot axes

pgenv(0,10,-5,5,0,0);    

pglabel("X","Y","Data"); # Labels

pgsci(5);                # Change colour

@x=(); @y=(); $i=0;
while(<DATA>){               

   # Read data in 2 columns from file handle
   # and put in two perl arrays

   ($x[$i], $y[$i]) = split(' ');    
   $i++;
}

# Plot points - note how perl arrays are passed

pgpoint($i,\@x,\@y,17);                           
pgend;    # Close plot

__DATA__
1 -4.5
2 -4
3  -3.2
4 -2.1
5 -1
6 0.3
7 1.2
8 2.4
9 2.9
