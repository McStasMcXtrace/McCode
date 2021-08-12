#!/usr/bin/env bash

# rapid-build script for mcxtrace-kernel dev:

git submodule init
git submodule update

WORK=`pwd`

export MCINSTALL_PREFIX=$HOME/McXtrace
export CC=gcc
export FC=gfortran

NUM_CPU=1
#`grep CPU /proc/cpuinfo|wc -l`

VERSION="3.0-dev"
if [ "x$1" != "x" ]
then
        VERSION=$1
fi


if [ -d ${MCINSTALL_PREFIX}/mcxtrace/${VERSION} ]
then
    mv  ${MCINSTALL_PREFIX}/mcxtrace/${VERSION} $MCINSTALL_PREFIX/mcxtrace/${VERSION}.bak
fi

./mkdist mcxtrace ${VERSION} "" "" deb64 "" -- justinst
./mkdist mcxtrace-comps ${VERSION} "" "" deb64 "" -- justinst
# can be out-commented after the first build iteration:
./mkdist mcxtrace-tools-perl-cmdline ${VERSION} tools/Legacy-Perl-cmdline/ "" deb64 "" -- justinst
./mkdist mcxtrace-tools-perl ${VERSION} tools/Legacy-Perl/ "" deb64 "" -- justinst
./mkdist mcxtrace-tools-python-mxrun ${VERSION} tools/Python/mcrun/ "" deb64 "" -- justinst
./mkdist mcxtrace-tools-python-mccodelib ${VERSION} tools/Python/mccodelib/ "" deb64 "" -- justinst
./mkdist mcxtrace-tools-python-mxdisplay-pyqtgraph ${VERSION} tools/Python/mcdisplay/pyqtgraph/ "" deb64 "" -- justinst
./mkdist mcxtrace-tools-python-mxdisplay-webgl ${VERSION} tools/Python/mcdisplay/webgl/ "" deb64 "" -- justinst
./mkdist mcxtrace-tools-python-mxplot-pyqtgraph ${VERSION} tools/Python/mcplot/pyqtgraph/ "" deb64 "" -- justinst
./mkdist mcxtrace-tools-python-mxplot-svg ${VERSION} tools/Python/mcplot/svg/ "" deb64 "" -- justinst
./mkdist mcxtrace-tools-python-mxgui ${VERSION} tools/Python/mcgui/ "" deb64 "" -- justinst
./mkdist mcxtrace-tools-python-mxdoc ${VERSION} tools/Python/mcdoc/ "" deb64 "" -- justinst
./mkdist mcxtrace-tools-matlab-mxplot ${VERSION} tools/matlab/mcplot/ "" deb64 "" -- justinst
./mkdist mcxtrace-tools-python-mxplot-matplotlib ${VERSION} tools/Python/mcplot/matplotlib/ "" deb64 "" -- justinst
#./mkdist mcxtrace-tools-python-mxdisplay-matplotlib ${VERSION} tools/Python/mcdisplay/matplotlib/ "" deb64 "" -- justinst
#cp tools/other/mcsplit/mcsplit.py $MCINSTALL_PREFIX/mcstas/${VERSION}/bin/

# Ensure we are configured for w node MPI runs
sed -i s/\'${NUM_CPU}\'/\'2\'/g $MCINSTALL_PREFIX/mcxtrace/${VERSION}/tools/Python/mccodelib/mccode_config.py
