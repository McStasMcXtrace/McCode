#!/usr/local/bin/perl -w

use strict;
use Tk;
use FindBin;

BEGIN {
    if (!eval q{
	use Test::More;
	1;
    }) {
	print "1..0 # skip: no Test::More module\n";
	exit;
    }
}

plan tests => 5;

use_ok("Tk::JPEG");

my $file = (@ARGV) ? shift : "$FindBin::RealBin/../jpeg/testimg.jpg";

my $mw = MainWindow->new;
$mw->geometry('+10+10');

{
    my $image = $mw->Photo('-format' => 'jpeg', -file => $file);
    ok($image, "Loaded JPEG image from $file");
    $mw->Label(-image => $image)->pack;
    $mw->update;
}

my $image_data = do {
    open my $fh, $file or die "Cannot load $file: $!";
    binmode $fh;
    local $/;
    <$fh>;
};

SKIP: {
    skip("Needs MIME::Base64 for -data options", 1)
	if !eval { require MIME::Base64; 1 };

    my $image = $mw->Photo('-format' => 'jpeg', -data => MIME::Base64::encode_base64($image_data));
    ok($image, "Loaded JPEG image from base64 encoded data");
    $mw->Label(-image => $image)->pack;
    $mw->update;

    my $out_image_data = $image->data(-format => 'jpeg');
    my $image2 = $mw->Photo('-format' => 'jpeg', -data => $out_image_data);
    ok($image2, "Roundtrip image data");
    $mw->Label(-image => $image2)->pack;
    $mw->update;
}

SKIP: {
    skip("Loading JPEG image from binary data NYI *TODO*", 1);

    my $image = $mw->Photo('-format' => 'jpeg', -data => $image_data);
    ok($image, "Loaded JPEG image from binary data");
    $mw->Label(-image => $image)->pack;
    $mw->update;
}

$mw->after(500,[destroy => $mw]);
MainLoop;

