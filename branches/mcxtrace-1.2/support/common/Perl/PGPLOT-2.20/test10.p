#!/usr/local/bin/perl

use PGPLOT;

print "\n\nTesting multiple ways of passing things...\n\n";

# Create 138x128 image - note must use transpose as 
# perl is column major like C (see docs)

$k=0;
for($i=0; $i<128; $i++) { for($j=0; $j<138; $j++) {
   $$img2D[$i][$j] = sqrt($i*$j) / 128; 
   $img1D[$k]      = $$img2D[$i][$j];  $k++;  # For 1D test
}}

$imgchar = pack("f*",@img1D);

print "PGPLOT module version $PGPLOT::VERSION\n\n";

pgqinf("VERSION",$val,$len);
print "PGPLOT $val library\n\n";

$dev = "?" unless defined $dev; # "?" will prompt for device

pgbegin(0,$dev,2,2);     # Open plot device 

print "Plotting...\n";

@tr=(0,1,0,0,0,1);

@x=(10,20,30,40,50,60,70,80,90,100,110);
@y=(30,35,40,45,50,55,60,65,70,75, 80);

print "--------------------------------------\n";

nextplot('Points: scalars passed one by one','Image: packed char string');

pggray($imgchar,138,128,1,138,1,128,1,0,\@tr);
for($i=0; $i<11; $i++){ pgpt(1,$x[$i],$y[$i],17) }

nextplot('Points: 1D array passed by glob','Image: 1D array passed by glob');

pggray(*img1D,138,128,1,138,1,128,1,0,\@tr);
pgpt(11,*x,*y,17);

nextplot('Points: 1D array passed by reference','Image: 1D array passed by reference');

pggray(\@img1D,138,128,1,138,1,128,1,0,\@tr);
pgpt(11,\@x,\@y,17);

nextplot('Line: 1D cross-section of 2D array','Image: 2D array passed by reference');

pggray($img2D,138,128,1,138,1,128,1,0,\@tr);
pgwindow(0,128,0,1);
pgline(128, [0..127], $$img2D[127]);

$len=1; # -w fudge

pgend;

sub nextplot {
  print $_[0],"\n";
  print $_[1],"\n";
print "--------------------------------------\n";
  pgpage; pgwnad(0,128,0,128); pgsci(3); pgsch(1.3);
  pgbox("BCST",0,0,"BCST",0,0); 
  pgmtext('T',1.0,0.2,0,$_[0]);
  pgmtext('T',2.4,0.2,0,$_[1]);
  pgsci(4);
}
