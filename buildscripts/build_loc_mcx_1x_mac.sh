#!/usr/bin/env bash

# rapid-build script for mcxtrace-kernel dev:


./3rdparty/sync.sh

WORK=`pwd`

export MCINSTALL_PREFIX=$HOME/McXtrace

if [[ -d $HOME/McXtrace/mcxtrace/1.x-dev ]]
then
    rm -rf $HOME/McXtrace/mcxtrace/1.x-dev/*
fi

./mkdist mcxtrace 1.x-dev "" "" mac "" -- justinst
./mkdist mcxtrace-comps 1.x-dev "" "" mac "" -- justinst
# can be out-commented after the first build iteration:
./mkdist mcxtrace-tools-perl-cmdline 1.x-dev tools/Legacy-Perl-cmdline/ "" mac "" -- justinst
./mkdist mcxtrace-tools-python-mxrun 1.x-dev tools/Python/mcrun/ "" mac "" -- justinst
./mkdist mcxtrace-tools-python-mxdoc 1.x-dev tools/Python/mcdoc/ "" mac "" -- justinst
./mkdist mcxtrace-tools-python-mxgui 1.x-dev tools/Python/mcgui/ "" mac "" -- justinst
./mkdist mcxtrace-tools-python-mccodelib 1.x-dev tools/Python/mccodelib/ "" mac "" -- justinst
./mkdist mcxtrace-tools-python-mxdisplay-pyqtgraph 1.x-dev tools/Python/mcdisplay/pyqtgraph/ "" mac "" -- justinst
./mkdist mcxtrace-tools-python-mxplot-pyqtgraph 1.x-dev tools/Python/mcplot/pyqtgraph/ "" mac "" -- justinst
./mkdist mcxtrace-tools-python-mxplot-svg 1.x-dev tools/Python/mcplot/svg/ "" mac "" -- justinst
./mkdist mcxtrace-tools-python-mxplot-matplotlib 1.x-dev tools/Python/mcplot/matplotlib/ "" mac "" -- justinst
cp tools/other/mcsplit/mcsplit.py $MCINSTALL_PREFIX/mcxtrace/1.x-dev/bin/

# Ensure we are configured for 10 node MPI runs
sed -i s/\'4\'/\'10\'/g $MCINSTALL_PREFIX/mcxtrace/1.x-dev/tools/Python/mccodelib/mccode_config.py
