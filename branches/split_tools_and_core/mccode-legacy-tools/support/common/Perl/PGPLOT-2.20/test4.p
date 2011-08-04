#!/usr/local/bin/perl

use PGPLOT;

print "\n\nTesting greyscale, contour and vector routines...\n\n";

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

$dev = "?" unless defined $dev;

pgbegin(0,$dev,1,1);

print "Plotting...\n";

pgsci(3);
pgwnad(12000,13000,13000,12000);

@tr=(12000,8,0,12000,0,8);
pggray(\@image,128,128,1,128,1,128,5000,0,\@tr);
pglabel("\\ga","\\gd","Galaxy");
pgtbox("ZYHBCNST",0,0,"ZYDBCNST",0,0);

pgwedg('R', 2, 5, 5000, 0, 'Counts');

pgsci(4); pgsls(1);
@cont = (-1,1000,2000,3000,4000,5000);
pgcons(\@image, 128, 128, 1,128,1,128, \@cont, 6, \@tr);
pgwnad(0,1000,0,1000);

@a = (1..100);
@b = (1..100);
for(@a) {$_=30};
for(@b) {$_=50};

@tr=(0,100,0,0,0,100);
pgsah(1,30,0.5);
pgsci(2);
pgvect(\@a, \@b, 10, 10, 1,9,1,9, 1, 1, \@tr, -10000);

$len=1; # -w fudge

pgend;

