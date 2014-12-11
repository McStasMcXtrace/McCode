#!/usr/local/bin/perl -w

# Simple test to just load the F77.pm module

use strict;
use vars qw/$loaded/;

BEGIN { $| = 1; print "1..2\n"; }
END {print "not ok 1\n" unless $loaded;}
use ExtUtils::F77;
$loaded = 1;
print "ok 1\n";

# try compiling ?

open(FH,">hello.f");
print FH "
      subroutine hello_fortran
      print *, 'Hello from the wonderful world of fortran'
c     $ExtUtils::F77::Compiler $ExtUtils::F77::Cflags $ExtUtils::F77::Runtime
      return
      end
";

close FH;
unlink "hello.o" if(-e "hello.o");
my $compile_command = "$ExtUtils::F77::Compiler  $ExtUtils::F77::Cflags -c hello.f ";
my $rc = system($compile_command);
$rc = 0xffff & $rc;
if($rc){
  if($rc == 0xff00){
	 print "2  ERROR: $compile_command failed: $!";
  }elsif ($rc > 0x80) {
	 $rc >>= 8;
	 print "2  WARNING: $compile_command returned non-zero exit status $rc\n";
  }else{
	 if($rc & 0x80){
		print "2  $compile_command coredumped from signal $rc";
	 }else{
		print "2  $compile_command returned signal $rc";
	 }
  }  
}else{
  unlink "hello.f","hello.o";
  print "ok 2\n" 
}

# how about linking - too complicated? 




