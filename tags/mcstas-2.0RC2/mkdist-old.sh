#!/bin/sh
#
# Simple script for setting up a mcstas/mcxtrace distribution package.
#
# To be called with one/two input parameter(s) - distribution tail filename, e.g.
#
# ./mkdist version          -> ../mcstas-version.tar.gz
# ./mkdist version mcxtrace -> ../mcxtrace-version.tar.gz
#
# Assumes you have write access to ..
#
# PW, risoe, 20030123
#
#   This file is part of the McStas neutron ray-trace simulation package
#   Copyright (C) 1997-2006, All rights reserved
#   Risoe National Laborartory, Roskilde, Denmark
#   Institut Laue Langevin, Grenoble, France
#
#   This program is free software; you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation; version 2 of the License.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
#
#   You should have received a copy of the GNU General Public License
#   along with this program; if not, write to the Free Software
#   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

# define version and symbols that will be replaced in many places
if [ "y$2" = "ymcxtrace" ]; then
  FLAVOR=mcxtrace
  MCCODE_TARNAME=mcxtrace
  MCCODE_NAME=McXtrace
else
  FLAVOR=mcstas
  MCCODE_TARNAME=mcstas
  MCCODE_NAME=McStas
fi
MCCODE_VERSION=$1
MONTH=`date +"%b"`
DAY=`date +"%d"`
YEAR=`date +"%Y"`
MCCODE_DATE="$MONTH. $DAY, $YEAR"
MCCODE_STRING="$MCCODE_NAME $MCCODE_VERSION - $MONTH. $DAY, $YEAR"

# Create temporary workdir:
PW=`pwd`
TMPDIR=$PW/..
DIST=$TMPDIR/$MCCODE_TARNAME-$MCCODE_VERSION

# Copy current PW checkout to DIST
cp -rp $PW $DIST

# Create the doc's from tex - if a checkout is avail.
if [ -d $TMPDIR/../McStasTeX/trunk ] ;
then
  cd $TMPDIR/../McStasTeX/trunk
  ./build $MCCODE_VERSION

  # copy the doc in DIST/doc. Only use PDF files in distro.
  mv $MCCODE_TARNAME-$MCCODE_VERSION-manual.pdf $DIST/doc/manuals/$MCCODE_TARNAME-manual.pdf
  mv $MCCODE_TARNAME-$MCCODE_VERSION-components.pdf $DIST/doc/manuals/$MCCODE_TARNAME-components.pdf
  cp -r $DIST/doc $DIST/lib
fi

# Go in DIST, clean up CVS/SVN information
cd $DIST
echo "Clean up SVN and git info"
find . -name .svn -exec rm -rf \{\} \; 2> /dev/null
find . -name .git -exec rm -rf \{\} \; 2> /dev/null
# Make sure that all comps and instrs have unix linefeeds
echo "Apply dos2unix"
find . -name \*.comp  -exec dos2unix \{\} \;
find . -name \*.instr -exec dos2unix \{\} \;

# Put in new version info:
# MCCODE_STRING="MCCODE_NAME MCCODE_VERSION - MCCODE_DATE"
echo "Set version $MCCODE_VERSION and date"
find . -wholename './setversion' -prune -o -type d -prune -o -path ./src/McStasTest -exec ./setversion \{\} $MCCODE_VERSION \;
find . -wholename './setyear'    -prune -o -type d -prune -o -path ./src/McStasTest -exec ./setyear \{\} \;

# we update all package/version strings
echo "Set package $MCCODE_VERSION information"
for iter in 1 2 3; do
    for file in mcstas/{CMakeLists.txt,configure.in} \
        mcxtrace/{CMakeLists.txt,configure.in} \
        tools/Legacy-Perl/{configure.in,lib/tools/perl/mccode_reconfigure.in} ; do
        echo ${file}
	      sed 's/@MCCODE_STRING@/'$MCCODE_NAME'-'$MCCODE_VERSION' - '$MONTH'. '$DAY', '$YEAR'/' $file > $file.tmp
        sed 's/@MCCODE_NAME@/'$MCCODE_NAME'/'                                                 $file.tmp > $file
        sed 's/@MCCODE_TARNAME@/'$MCCODE_TARNAME'/'                                           $file > $file.tmp
        sed 's/@MCCODE_VERSION@/'$MCCODE_VERSION'/'                                           $file.tmp > $file
        sed 's/@MCCODE_DATE@/'$MONTH'. '$DAY', '$YEAR'/'                                      $file > $file.tmp
        mv $file.tmp $file
	  done
done
# chmod a+x support/deb/debcreate

# Create the doc's from tex - if a checkout is avail.
cd $DIST
if [ -d $DIST/doc/install_docs/tex ] ;
then
  # Create ps/pdf/html based install info
  cd $DIST/doc/install_docs/tex
  latex install
  latex install
  whichdvipdf=`which dvipdf`
  if [ -e "$whichdvipdf" ] ;
  then
      echo "Generate install PDF files using dvipdf (better quality than ps2pdf)"
      dvipdf install.dvi install.pdf
  else
      echo "Generate install PDF files using ps2pdf"
      dvips -o install.ps install.dvi
      ps2pdf install.ps install.pdf
  # gzip install.ps
      rm *.ps *.ps.gz
  fi

  mkdir ../html
  latex2html -local_icons -dir ../html install.tex
  # clean up
  rm install.aux
  rm install.log
  cd $DIST/doc/tutorial
  # Update the tutorial files
  ./update
  rm update
  mkdir -p $DIST/lib/doc
  cp -r $DIST/doc/install_docs/ $DIST/lib/doc
  cp -r $DIST/doc/tutorial/ $DIST/lib/doc/
  cp -r $DIST/doc/man/ $DIST/lib/doc/
fi
cd $DIST

# Create configure (which will itself create mccode_config.perl and mccode_reconfigure)
cd $FLAVOR
autoconf

# run lex, yacc (assuming linux - flex -i & bison -v -d)
cd src &&
flex -i instrument.l &&
bison -v -d instrument.y &&
cd ..


# Set the INSTALL environment variable to make the src archive
# default to ./install-sh in the contributed Makefile.
# Note: the -c is needed to preserve mcdoc.fixpl which is run at make install
export INSTALL="./install-sh -c"

# Also, do a ./configure to create Makefile and mccode_config.perl
# These will be dependent on this system, ofcourse...
./configure

cd ..


cd tools/Legacy-Perl/lib/tools/perl
autoconf mccode_reconfigure > ${MCCODE_TARNAME}_reconfigure
chmod a+x ${MCCODE_TARNAME}_reconfigure
cd -


# Select scilab as default plotter
# make pgplot

# Clean up
rm TODO.txt
rm mkdist
rm setversion
rm setyear
rm ${FLAVOR}/header
# Make new runs of configure not dependent of this system...
rm config.*
cd ..

# Create tar archive
tar cfz $TMPDIR/$MCCODE_TARNAME-$MCCODE_VERSION-src.tar.gz $MCCODE_TARNAME-$MCCODE_VERSION

# Do a 'make' for creation of binary version (should apply to 'any' Unix)
cd $DIST/${FLAVOR}

# Default again to Linux' /usr/bin/install -c (or whatever ./configure finds)
export INSTALL=

./configure && make

UNAME=`uname`
MACH=`uname -m`
# Up to now, this is always done on intel...
PROC=Intel
cd $TMPDIR

# Create tar archive
tar cfz $TMP/$MCCODE_TARNAME-$MCCODE_VERSION-$MACH-$PROC-$UNAME.tar.gz $MCCODE_TARNAME-$MCCODE_VERSION

# Remove temporary dir
# rm -rf $TMPDIR

# Create .tar.gz archive for building a binary Windows version
cd $DIST/${FLAVOR}
make clean

cd src
flex -i instrument.l
bison -v -d instrument.y
cd ..

./configure
# build.bat is obsolete?
#sed 's/DOZIP="0"/DOZIP="1"/' build.bat > build.bat.new
#sed 's/DONSIS="0"/DONSIS="1"/' build.bat.new > build.bat
cp support/Win32/install/which.exe .
cd ..
tar cfz $TMPDIR/$MCCODE_TARNAME-$MCCODE_VERSION-Win32-src.tar.gz $MCCODE_TARNAME-$MCCODE_VERSION
echo "  "
echo "========================================================================="
echo "mkdist: Distro creation for $MCCODE_STRING"
if [ `which makensis` ]; then
    cd ${DIST}/${FLAVOR}
    ./configure
    make
    make $MCCODE_TARNAME.win32
    cp support/Win32/install/$MCCODE_TARNAME.ini ..
    cp support/Win32/install/$MCCODE_TARNAME.nsi ..
    cp support/Win32/install/$MCCODE_TARNAME.bmp ..
    cp support/Win32/install/LICENSE_$MCCODE_TARNAME.rtf ..
    cd ${TMPDIR}
    zip -r $MCCODE_TARNAME-$MCCODE_VERSION-i686-Intel-Win32.zip $MCCODE_TARNAME-$MCCODE_VERSION
    cp winsupport/* .
    makensis -X"SetCompressor /FINAL lzma" $MCCODE_TARNAME.nsi
    echo Created Win32 binary packages: `ls *zip *exe`
else
    echo Binary versions for Win32 NOT created
fi
echo
echo
echo Your $MCCODE_STRING dist packages are placed in
echo ../$MCCODE_TARNAME-$MCCODE_VERSION-src.tar.gz
echo ../$MCCODE_TARNAME-$MCCODE_VERSION-$MACH-$PROC-$UNAME.tar.gz
echo ../$MCCODE_TARNAME-$MCCODE_VERSION-Win32-src.tar.gz
echo
echo NOTE: Win32 version must be built using \'build.bat\' of the Win32-src package
echo
if [ -d $TMPDIR/../McStasTeX/trunk ] ;
then
echo Your $MCCODE_STRING doc packages are placed in
echo ../$MCCODE_TARNAME-$MCCODE_VERSION-manual.ps.gz
echo ../$MCCODE_TARNAME-$MCCODE_VERSION-manual.pdf
echo ../$MCCODE_TARNAME-$MCCODE_VERSION-components.ps.gz
echo ../$MCCODE_TARNAME-$MCCODE_VERSION-components.pdf
fi
echo
echo WARNING: Please install to a test system and run the following tools
echo before release:
echo
echo compiletest.sh \(check if all examples compile\)
echo mcrun --test \(validation test of selected instruments\)
echo
echo Follow instructions in ../adm to publish on the web.
echo

echo Build packages from `pwd`
# In case of mkfs.hfsplus available in /sbin we'll create the
# DMG file for Mac OS X
if [ -e /sbin/mkfs.hfsplus ]
then
    # Note: DMG file needs enough space to hold the support tools plus
    # our own code. April 2008 this is about 120 megs...
    dd if=/dev/zero of=./$MCCODE_TARNAME-$MCCODE_VERSION.dmg bs=1M count=130
    /sbin/mkfs.hfsplus -v $MCCODE_NAME ./$MCCODE_TARNAME-$MCCODE_VERSION.dmg
    # Likely needs root level access:
    sudo mkdir /mnt/dmg
    sudo mount -t hfsplus -o loop ./$MCCODE_TARNAME-$MCCODE_VERSION.dmg /mnt/dmg
    # Copy the ./adm/OSX/DMG stuff to the mountpoint
    sudo cp -rp ./adm/OSX/DMG/Applications /mnt/dmg
    sudo cp ./adm/OSX/DMG/README.TXT /mnt/dmg
    # Clear out CVS stuff
    sudo find /mnt/dmg -type d -name CVS -exec rm -rf \{\} \;
    # Copy McStas there
    sudo cp ./$MCCODE_TARNAME-$MCCODE_VERSION-src.tar.gz /mnt/dmg/Applications
    # Unmount
    sudo umount /mnt/dmg
    sudo rm -rf /mnt/dmg
    ls -l ./$MCCODE_TARNAME-$MCCODE_VERSION.dmg
fi


# In case of a debian build host, we also build an .deb
# (.rpm was tried using alien - did not work too well)
if [ -e /etc/debian_version ]
then
  if [ -e build-chroot-i386 ]
  then
    echo Proceeding to i386 .deb build
    # First, clean up in case of previous McStas installs in the chrooot
    sudo rm -f build-chroot-i386/usr/local/bin/*
    sudo rm -rf build-chroot-i386/usr/local/lib/*
    sudo cp $MCCODE_TARNAME-$MCCODE_VERSION/support/deb/debcreate build-chroot-i386/root
    sudo cp $MCCODE_TARNAME-$MCCODE_VERSION/support/deb/sources.list build-chroot-i386/etc/apt/
    sudo cp $MCCODE_TARNAME-$MCCODE_VERSION-src.tar.gz build-chroot-i386/root
    sudo mkdir build-chroot-i386/build
    sudo chroot build-chroot-i386 apt-get update
    sudo chmod u+x build-chroot-i386/root/debcreate
    sudo chroot build-chroot-i386 /root/debcreate $1 i386
    echo
    echo
    echo Build packages created:
    echo
    cp build-chroot-i386/build/*deb .
    ls -lf $MCCODE_TARNAME-$MCCODE_VERSION*.* $MCCODE_TARNAME-$MCCODE_VERSION*.*
    echo
  else
    echo You need a chroot environment based on debootstrap in build-chroot-i386 before building the i386 deb package - create using something like
    echo sudo debootstrap --arch i386 karmic build-chroot
  fi
  if [ -e build-chroot-x86_64 ]
  then
    echo Proceeding to .deb build
    # First, clean up in case of previous McStas installs in the chrooot
    sudo rm -f build-chroot-x86_64/usr/local/bin/*
    sudo rm -rf build-chroot-x86_64/usr/local/lib/*
    sudo cp $MCCODE_TARNAME-$MCCODE_VERSION/support/deb/debcreate build-chroot-x86_64/root
    sudo cp $MCCODE_TARNAME-$MCCODE_VERSION/support/deb/sources.list build-chroot-x86_64/etc/apt/
    sudo cp $MCCODE_TARNAME-$MCCODE_VERSION-src.tar.gz build-chroot-x86_64/root
    sudo mkdir build-chroot-x86_64/build
    sudo chroot build-chroot-x86_64 apt-get update
    sudo chmod u+x build-chroot-x86_64/root/debcreate
    sudo chroot build-chroot-x86_64 /root/debcreate $1 amd64
    echo
    echo
    echo Build packages created:
    echo
    cp build-chroot-x86_64/build/*deb .
    ls -lf $MCCODE_TARNAME-$MCCODE_VERSION*.* $MCCODE_TARNAME-$MCCODE_VERSION*.*
    echo
  else
    echo You need a chroot environment based on debootstrap in build-chroot-x86_64 before building the x86_64 deb package - create using something like
    echo sudo debootstrap --arch amd64 karmic build-chroot-x86_64
  fi
fi
