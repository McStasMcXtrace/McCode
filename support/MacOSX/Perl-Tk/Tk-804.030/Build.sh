# Clear out build dir
rm -rf ../build/*

# What version of Tk are we building?
TKVER=`basename $PWD`

# On which OS X?
OSXVER=`sw_vers -productVersion|cut -f -2 -d.`

# Assemble build-dir path
BUILDDIR=${TKVER}_MacOSX_${OSXVER}
# Configure with system default perl
perl Makefile.PL PREFIX=../build/$BUILDDIR/usr/local

# Build
make

# Install
make install

# Go to build directory and package a .pkg using PackageMaker
cd ../build
/Applications/PackageMaker.app/Contents/MacOS/PackageMaker --root $BUILDDIR -i dk.dtu.fysik.tk.perl -o $BUILDDIR.pkg
