#!/usr/local/bin/perl

use PGPLOT;

print "\n\nTesting perl function passing to pgconx...\n\n";

print "PGPLOT module version $PGPLOT::VERSION\n\n";

pgqinf("VERSION",$val,$len);
print "PGPLOT $val library\n\n";

# Read in image (int*2 raw byte array)

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
pgwnad(1,128,1,128);
pgbox("BCNST",0,0,"BCNST",0,0);

pglabel("X","Y","Dropped Galaxy");

pgsci(5); pgsls(1);
@cont = (-1,1000,2000,3000,4000,5000);

pgsci(5);

pgconx(\@image, 128, 128, 1,128,1,128, \@cont, 6, "squashplot");
pgwnad(0,1000,0,1000);

$len=1; # -w fudge

pgend;

sub squashplot {
    my ($visible,$x,$y,$z) = @_;

    $xworld = $x*$x/128;
    $yworld = $y*$y/128;
    if ($visible) {
       pgdraw($xworld,$yworld);
    }else{
       pgmove($xworld,$yworld);
    }
}
