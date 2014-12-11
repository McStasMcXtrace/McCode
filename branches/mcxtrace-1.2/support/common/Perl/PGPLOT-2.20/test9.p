#!/usr/local/bin/perl

use PGPLOT;

print "\n\nTesting PGPLOT 5.0 colour image routines...\n\n";

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

pgbegin(0,$dev,1,1);     # Open plot device 

print "Plotting...\n";

pgsci(3);
pgwnad(12000,13000,13000,12000);

@tr=(12000,8,0,12000,0,8);

pgimag(\@image,128,128,1,128,1,128,0,5000,\@tr);
pglabel("\\ga","\\gd","Galaxy");
pgtbox("ZYHBCNST",0,0,"ZYDBCNST",0,0);

# Note: pgimag() usually defaults to a grey scale unless you explicitly set
# a colour ramp look-up table with pgctab(). Because it is a look
# up table it can be set after drawing the image. It is best to set an
# explicit LUT as a grey scale default can not be guaranteed on all devices.

# Set PHIL2 colour table

@l=(0,0.004,0.502,0.941,1); @r=(0,0,1,1,1); 
@g=(0,0,0.2,1,1); @b=(0,0.2,0,0.1,1);

pgctab(\@l,\@r,\@g,\@b,5,1,0.5);

pgsci(4); pgsls(1);
@cont = (-1,1000,2000,3000,4000,5000);
pgcons(\@image, 128, 128, 1,128,1,128, \@cont, 6, \@tr);

for(@cont){
   pgconl(\@image, 128, 128, 1,128,1,128, $_, \@tr, $_,200,100);
}

pgsci(4); pgscf(2); 
pgqtxt(12125,12100,45,0.5,'PGPLOT...',\@xbox,\@ybox);
pgpoly(4,\@xbox, \@ybox);
pgsci(7); 
pgptxt(12125,12100,45,0.5,'PGPLOT...');

$x=0; $y=0; # Get past -w

pgqinf("CURSOR",$ans,$l);
if ($ans eq "YES") { for($mode=0; $mode<8; $mode++){

   print "Entering interactive PGBAND test MODE=$mode, hit any key, Q to exit early...\n";

   pgsci($mode+1);
   pgband($mode,0,12500,12500,$x,$y,$ch);
   last if $ch eq "q" || $ch eq "Q";
   pgqtxt($x,$y,45,0.5,'PGPLOT...',\@xbox,\@ybox);
   pgpoly(4,\@xbox, \@ybox);
   pgsci($mode+2);
   pgptxt($x,$y,45,0.5,'PGPLOT...');
   
}}

$len=1; # -w fudge

pgend;
