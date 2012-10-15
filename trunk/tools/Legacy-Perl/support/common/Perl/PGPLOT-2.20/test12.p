#!/usr/local/bin/perl

use PGPLOT;

print "\n\nTesting some new PGPLOT 5.2.0 routines...\n\n";

print "PGPLOT module version $PGPLOT::VERSION\n\n";

pgqinf("VERSION",$val,$len);
print "PGPLOT $val library\n\n";
$val =~ s/\.//g; $val =~ s/v//; 
die "PGPLOT version must be > 5.2.0 for this test $val\n" if $val<520;

# Read in image (int*2)

$img="";
open(IMG,"test.img") || die "Data file test.img not found";
if($^O =~ /mswin32/i) {binmode(IMG)}
read(IMG, $img, 32768);
close(IMG) or die "can't close test.img: $!";

print length($img)," bytes read\n";

@image = unpack("n*",$img);

print $#image+1," element image stored\n";

$dev = "?" unless defined $dev;

pgbegin(0,$dev,1,1);

print "Plotting...\n";

pgsci(3);
pgwnad(12000,13000,13000,12000);

@tr=(12000,8,0,12000,0,8);
pglabel("\\ga","\\gd","Galaxy");
pgtbox("ZYHBCNST",0,0,"ZYDBCNST",0,0);

pgsci(4); pgconf(\@image, 128, 128, 1,128,1,128, 1000,2000, \@tr);
pgsci(2); pgconf(\@image, 128, 128, 1,128,1,128, 2000,3000, \@tr);

@cont = (-1,1000,2000,3000,4000,5000);
pgsci(7);
pgcons(\@image, 128, 128, 1,128,1,128, \@cont, 6, \@tr);

pgsci(1);
pgaxis('LN2',12500,12800,12900,12100,1,4,0,0, 1,2,0.5, -2,30);

pgtick(12500,12800,12900,12100, 0.35, 3,5, 6,90,'pgperl!');

pgqndt($ndrivers);

print "Testing pgqdt() - $ndrivers drivers found...\n";
for $n (1..$ndrivers) {
  pgqdt($n,$type,$tlen,$descr,$dlen,$inter);
  print "$n:  $type $tlen $descr $dlen $in\n";
}

pgend;

