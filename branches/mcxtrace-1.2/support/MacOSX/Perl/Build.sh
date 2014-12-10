#!/bin/sh
# Script for building PGPLOT, PDL and Extutils::F77
#

ls *tar.gz | xargs -n1 tar xzf 

# Assume /opt/mccode/pgplot as pgplot location, and /opt/mccode/gcc/bin as gcc/gfortran location
export PGPLOT_DIR=/opt/mccode/pgplot
export PATH=/opt/mccode/gcc/bin:$PATH
export DYLD_LIBRARY_PATH=/opt/mccode/gcc/lib

# What package are we building?
PACKAGE=PGPLOT_PDL

cd ExtUtils-F77-1.17
# On which OS X and which PERL
OSXVER=`sw_vers -productVersion|cut -f -2 -d.|sed -e 's/\./\_/g'`
PERLVER=`perl -v |grep v5|cut -f2 -d\(|cut -f1 -d\)|cut -b2-|cut -f1-2 -d.`
PERLVER_=`echo $PERLVER|sed -e 's/\./\_/g'`

# Assemble build-dir path
BUILDDIR=${PACKAGE}_MacOSX_${OSXVER}_Perl_${PERLVER_}
# Configure with system default perl
perl Makefile.PL PREFIX=../build/$BUILDDIR/Library/Perl/$PERLVER LIB=../build/$BUILDDIR/Library/Perl/$PERLVER

# Build
make static

# Install
make install

cd ../PGPLOT-2.21

# Assemble build-dir path
BUILDDIR=${PACKAGE}_MacOSX_${OSXVER}_Perl_${PERLVER_}
# Configure with system default perl
perl Makefile.PL PREFIX=../build/$BUILDDIR/Library/Perl/$PERLVER LIB=../build/$BUILDDIR/Library/Perl/$PERLVER

# Build
make static

# Install
make install

cd ../PDL-2.007
# Assemble build-dir path
BUILDDIR=${PACKAGE}_MacOSX_${OSXVER}_Perl_${PERLVER_}
# Configure with system default perl
perl Makefile.PL PREFIX=../build/$BUILDDIR/Library/Perl/$PERLVER LIB=../build/$BUILDDIR/Library/Perl/$PERLVER

# Build
make

# Install
make install


# Go to build directory and package a .pkg using PackageMaker
cd ../build
/Applications/PackageMaker.app/Contents/MacOS/PackageMaker --root $BUILDDIR -i dk.dtu.fysik.perl.PGPLOT_PDL -o $BUILDDIR.pkg
