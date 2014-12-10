#!/usr/local/bin/perl

use PGPLOT;

print "\n\nTesting pghi2d routine...\n\n";

print "PGPLOT module version $PGPLOT::VERSION\n\n";

pgqinf("VERSION",$val,$len);
print "PGPLOT $val library\n\n";

# Read in image (int*2)

$img="";
open(IMG,"test.img") || die "Data file test.img not found";
if($^O =~ /mswin32/i) {binmode(IMG)}
read(IMG, $img, 32768);
close(IMG) or die "Can't close test.img: $!";

print length($img)," bytes read\n";

@image = unpack("n*",$img);

print $#image+1," element image stored\n";

$dev = "?" unless defined $dev; # "?" will prompt for device

pgbegin(0,$dev,1,1);

print "Plotting...\n";

pgenv(0,256,0,65000,0,0);

pgsci(5);

@xvals = (1..128);

@work = (1..128); 

pghi2d(\@image, 128, 128, 1,128,1,128, \@xvals, 1, 200, 1, \@work);

$len=1; # -w fudge

pgend;

