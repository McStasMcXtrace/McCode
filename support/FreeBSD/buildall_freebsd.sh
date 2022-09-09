#!/bin/sh
#
# Naive build script for McCode packages on FreeBSD (no dependency- or error control etc.)
#
# Assumes to run from a dist/ output folder containing
# the src package output from e.g. build_src_bin_mcstas in the trunk
#

# Build and install
for TGZ in `ls -rt *tar.gz` 
do
    PKG=`basename $TGZ .tar.gz`
    tar xzf $TGZ
    cd $PKG
    cmake -DCMAKE_TOOLCHAIN_FILE=cmake/toolchains/freebsd64.cmake -DBUILD_MCSTAS=1
    make
    sudo make install
    cd -
done
sudo sh $PKG/work/support/postinst
echo DONE building and installing.
echo 
echo Remember to rehash if this is csh/tcsh


