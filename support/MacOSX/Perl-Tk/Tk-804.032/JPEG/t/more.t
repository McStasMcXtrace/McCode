#!/usr/bin/perl -w

use strict;
use FindBin;
use File::Temp qw(tempfile);
use Test::More;

use Tk;        
use Tk::Photo;    

my @writeopt = ([],[-grayscale],[-progressive],[-quality => 13],[-smooth => 12]);

my $mw = eval { Tk::MainWindow->new() };
if (!Tk::Exists($mw)) {
    plan skip_all => "Cannot create MainWindow: $@";
    CORE::exit(0);
}

plan tests => 7*@writeopt+6;

eval { require Tk::JPEG };
is $@, '', "loading Tk::JPEG";

my $file = (@ARGV) ? shift : "$FindBin::RealBin/../jpeg/testimg.jpg";

$mw->geometry('+10+10');

my $image = eval { $mw->Photo('-format' => 'jpeg', -file => $file) };
is $@, '', "loading jpeg photo from file";
is $image->width, 227, "width check";
is $image->height, 149, "height height";
my $l = $mw->Label(-image => $image, -bd => 0, -padx => 0, -pady => 0)->pack;
$mw->update;
is $l->width, 227, "width check of label";
is $l->height, 149, "height check of label";

my $image2;

foreach  my $opt (@writeopt)
 {
  my($tmpfh,$tmpfile) = tempfile(SUFFIX => '.jpg', UNLINK => 1)
      or die "Can't create temporary file: $!";
  eval { $image->write($tmpfile, -format => ['jpeg',@$opt]) };
  is $@, '', 'writing jpeg';
  my $ok = (-s $tmpfile) ? 1 : 0;
  ok $ok, "File has non-zero size";

  eval {$image2 = $mw->Photo('-format' => 'jpeg', -file => $tmpfile)};
  is $@, '', "loading jpeg";
  is $image2->width, 227, "expected width";
  is $image2->height, 149, "expected height";
                                    
  $l->configure(-image => $image2); 
  $mw->update;                      
  is $l->width, 227, "expected label width";
  is $l->height, 149, "expected label height";
 }


$mw->after(500,[destroy => $mw]);
MainLoop;
