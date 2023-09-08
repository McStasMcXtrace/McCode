#!/usr/bin/env bash

# rapid-build script for mcxtrace-kernel dev:


./3rdparty/sync.sh

WORK=`pwd`

export MCINSTALL_PREFIX=$HOME/McXtrace

if [[ -d $HOME/McXtrace/mcxtrace/2.x-dev ]]
then
    rm -rf $HOME/McXtrace/mcxtrace/2.x-dev/*
fi

./mkdist mcxtrace 2.x-dev "" "" mac "" -- justinst
./mkdist mcxtrace-comps 2.x-dev "" "" mac "" -- justinst
./mkdist mcxtrace-tools-python-mxrun 2.x-dev tools/Python/mcrun/ "" mac "" -- justinst
./mkdist mcxtrace-tools-python-mxdoc 2.x-dev tools/Python/mcdoc/ "" mac "" -- justinst
./mkdist mcxtrace-tools-python-mxgui 2.x-dev tools/Python/mcgui/ "" mac "" -- justinst
./mkdist mcxtrace-tools-python-mccodelib 2.x-dev tools/Python/mccodelib/ "" mac "" -- justinst
./mkdist mcxtrace-tools-python-mxdisplay-pyqtgraph 2.x-dev tools/Python/mcdisplay/pyqtgraph/ "" mac "" -- justinst
./mkdist mcxtrace-tools-python-mxplot-pyqtgraph 2.x-dev tools/Python/mcplot/pyqtgraph/ "" mac "" -- justinst
./mkdist mcxtrace-tools-python-mxplot-svg 2.x-dev tools/Python/mcplot/svg/ "" mac "" -- justinst
./mkdist mcxtrace-tools-python-mxplot-matplotlib 2.x-dev tools/Python/mcplot/matplotlib/ "" mac "" -- justinst
cp tools/other/mcsplit/mcsplit.py $MCINSTALL_PREFIX/mcxtrace/2.x-dev/bin/

# Ensure we are configured for 10 node MPI runs
sed -i s/\'4\'/\'10\'/g $MCINSTALL_PREFIX/mcxtrace/2.x-dev/tools/Python/mccodelib/mccode_config.py
