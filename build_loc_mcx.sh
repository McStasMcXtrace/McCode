#!/usr/bin/env bash

# rapid-build script for mcxtrace-kernel dev:

git submodule init
git submodule update

WORK=`pwd`

export MCINSTALL_PREFIX=$HOME
export CC=gcc
export FC=gfortran

NUM_CPU=`grep CPU /proc/cpuinfo|wc -l`

VERSION="3.0-dev"
if [ "x$1" != "x" ]
then
        VERSION=$1
fi

if [ -d $MCINSTAL_PREFIX/mcxtrace/$VERSION ]
then
    mv  $MCINSTALL_PREFIX/mcxtrace/$VERSION $MCINSTALL_PREFIX/mcxtrace/${VERSION}.bak
fi

./mkdist mcxtrace 3.0-dev "" "" deb64 "" -- justinst
./mkdist mcxtrace-comps 3.0-dev "" "" deb64 "" -- justinst
# can be out-commented after the first build iteration:
./mkdist mcxtrace-tools-perl-cmdline 3.0-dev tools/Legacy-Perl-cmdline/ "" deb64 "" -- justinst
./mkdist mcxtrace-tools-python-mxrun 3.0-dev tools/Python/mcrun/ "" deb64 "" -- justinst
./mkdist mcxtrace-tools-python-mccodelib 3.0-dev tools/Python/mccodelib/ "" deb64 "" -- justinst
./mkdist mcxtrace-tools-python-mxdisplay-pyqtgraph 3.0-dev tools/Python/mcdisplay/pyqtgraph/ "" deb64 "" -- justinst
./mkdist mcxtrace-tools-python-mxplot-pyqtgraph 3.0-dev tools/Python/mcplot/pyqtgraph/ "" deb64 "" -- justinst
./mkdist mcxtrace-tools-python-mxplot-svg 3.0-dev tools/Python/mcplot/svg/ "" deb64 "" -- justinst
./mkdist mcxtrace-tools-python-mxplot-matplotlib 3.0-dev tools/Python/mcplot/matplotlib/ "" deb64 "" -- justinst
cp tools/other/mcsplit/mcsplit.py $MCINSTALL_PREFIX/mcxtrace/3.0-dev/bin/

# Ensure we are configured for 4 node MPI runs
sed -i s/\'${NUM_CPU}\'/\'10\'/g $MCINSTALL_PREFIX/mcxtrace/3.0-dev/tools/Python/mccodelib/mccode_config.py
