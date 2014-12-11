#!/usr/local/bin/perl -w
use strict;
use Test;
use Tk;
use Tk::Photo;

my @writeopt = ([],[-grayscale],[-progressive],[-quality => 13],[-smooth => 12]);


plan tests => 7*@writeopt+6;

eval { require Tk::JPEG };
ok($@,'',"Cannot load Tk::JPEG");

my $file = (@ARGV) ? shift : 'jpeg/testimg.jpg';

my $mw = MainWindow->new;
$mw->geometry('+10+10');
my $image;
eval {$image = $mw->Photo('-format' => 'jpeg', -file => $file)};
ok($@,'',"Error $@");
ok($image->width,227,"Wrong width");
ok($image->height,149,"Wrong height");
my $l = $mw->Label(-image => $image, -bd => 0, -padx => 0, -pady => 0)->pack;
$mw->update;
ok($l->width,227,"Wrong width");
ok($l->height,149,"Wrong height");

my $image2;

foreach  my $opt (@writeopt)
 {
  unlink("testout.jpg") if -f "testout.jpg";
  eval { $image->write("testout.jpg", -format => ['jpeg',@$opt]) };
  ok($@,'',"Error $@");
  my $ok = (-s "testout.jpg") ? 1 : 0;
  ok($ok,1,"File has no size");

  eval {$image2 = $mw->Photo('-format' => 'jpeg', -file => "testout.jpg")};
  ok($@,'',"Error $@");
  ok($image2->width,227,"Wrong width");
  ok($image2->height,149,"Wrong height");

  $l->configure(-image => $image2);
  $mw->update;
  ok($l->width,227,"Wrong width");
  ok($l->height,149,"Wrong height");
 }


$mw->after(1000,[destroy => $mw]);
MainLoop;

END
{
 unlink "testout.jpg" if -f "testout.jpg";
}
