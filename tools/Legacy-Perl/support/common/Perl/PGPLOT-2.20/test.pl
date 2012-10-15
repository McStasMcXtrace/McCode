#!/usr/local/bin/perl

use Config;

# Run and check all the tests

$| = 1; # Unbuffer STDOUT

# Stop f77-linking causing spurious undefined symbols (alpha)

$ENV{'PERL_DL_NONLAZY'}=0 if $Config{'osname'} eq "dec_osf"; 

if($^O =~ /mswin32/i) {
  $note = "
NOTE - Win32 only: If /XSERVE doesn't work properly then
try /PNG (assuming libpng.dll is in your path). Then view
the output '.png' files at the conclusion of the tests. ";
}

else {$note = ''}

if ($ENV{'PGPLOT_DEV'}) {
    $dev = $ENV{'PGPLOT_DEV'};
} else {
print "Default Device for plot tests [recommend /XSERVE] ? $note ";
$dev = <STDIN>; chomp $dev;
$dev = "/XSERVE" unless $dev=~/\w/;
}

if($dev eq '/PNG' && $^O =~ /mswin32/i) {system "del /F /Q *.png"};

$ENV{PGPLOT_XW_WIDTH}=0.3;

foreach $jjj (1..12) {

   print "============== Running test$jjj.p ==============\n";
   %@ = ();       # Clear error status
   do "test$jjj.p";
   warn $@ if $@; # Report any error detected
   if($dev eq '/PNG' && $^O =~ /mswin32/i) {
     system("ren pgplot.png pgplot_$jjj.png");
     }
   sleep 2;
}

