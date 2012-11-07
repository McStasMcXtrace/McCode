#!/bin/sh
# Clear out build and src dirs
rm -rf ../build/*
make clean

# What version of Tk are we building?
TKVER=`basename $PWD`

# On which OS X and which PERL
OSXVER=`sw_vers -productVersion|cut -f -2 -d.`
PERLVER=`perl -v |grep v5|cut -f2 -d\(|cut -f1 -d\)|cut -b2-|cut -f1-2 -d.`

# Assemble build-dir path
BUILDDIR=${TKVER}_MacOSX_${OSXVER}
# Configure with system default perl
perl Makefile.PL PREFIX=../build/$BUILDDIR/Library/Perl/$PERLVER LIB=../build/$BUILDDIR/Library/Perl/$PERLVER

# Build
make

# Install
make install

# Go to build directory and package a .pkg using PackageMaker
cd ../build
/Applications/PackageMaker.app/Contents/MacOS/PackageMaker --root $BUILDDIR -i dk.dtu.fysik.tk.perl -o $BUILDDIR.pkg
