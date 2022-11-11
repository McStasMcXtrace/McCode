#!/bin/bash

# rapid-build script for mcstas-kernel dev:


#git checkout mcstas-3.0
#git pull

# Ensure our 3rd party modules are in place and updated
THIRDPARTY=`ls 3rdparty | grep -v patches`
cd 3rdparty
echo $THIRDPARTY | xargs -n1 rm -r
cd -
git submodule init
git submodule update


for MODULE in `echo $THIRDPARTY`
do
    if [ -d "3rdparty/patches/${MODULE}" ]; then
	echo Making McCode cmake modules available for $MODULE
        rsync -avz cmake/ 3rdparty/${MODULE}/cmake
	echo Applying McCode patches to 3rdparty/${MODULE}
	cp -rp 3rdparty/patches/${MODULE}/* 3rdparty/${MODULE}/
    fi
done

WORK=`pwd`

export MCINSTALL_PREFIX=$HOME/McStas
export CC=gcc
export CXX=c++
export FC=gfortran

if [[ -d $HOME/McStas/mcstas/3.0-dev ]]
then
    rm -rf $HOME/McStas/mcstas/3.0-dev/*
fi
./mkdist mcstas 3.0-dev "" "" deb64 "" -- justinst
./mkdist mcstas-comps 3.0-dev "" "" deb64 "" -- justinst
# can be out-commented after the first build iteration:
./mkdist mcstas-tools-perl-cmdline 3.0-dev tools/Legacy-Perl-cmdline/ "" deb64 "" -- justinst
#./mkdist mcstas-tools-perl 3.0-dev tools/Legacy-Perl/ "" deb64 "" -- justinst
#./mkdist mcstas-tools-python-mcgui 3.0-dev tools/Python/mcgui/ "" deb64 "" -- justinst
./mkdist mcstas-tools-python-mcrun 3.0-dev tools/Python/mcrun/ "" deb64 "" -- justinst
./mkdist mcstas-tools-python-mccodelib 3.0-dev tools/Python/mccodelib/ "" deb64 "" -- justinst
#./mkdist mcstas-tools-python-mcdisplay-pyqtgraph 3.0-dev tools/Python/mcdisplay/pyqtgraph/ "" deb64 "" -- justinst
#./mkdist mcstas-tools-python-mcplot-pyqtgraph 3.0-dev tools/Python/mcplot/pyqtgraph/ "" deb64 "" -- justinst
#./mkdist mcstas-tools-python-mcplot-svg 3.0-dev tools/Python/mcplot/svg/ "" deb64 "" -- justinst
#./mkdist mcstas-tools-python-mcplot-matplotlib 3.0-dev tools/Python/mcplot/matplotlib/ "" deb64 "" -- justinst
cp tools/other/mcsplit/mcsplit.py $MCINSTALL_PREFIX/mcstas/3.0-dev/bin/

export MCINSTALL_PREFIX=$HOME/McStas/mcstas/3.0-dev/
#./mkdist mcstas-ncrystal 3.0-dev 3rdparty/ncrystal-patches "" deb64 "" -- justinst

# Ensure we are configured for 10 node MPI runs
sed -i s/\'4\'/\'10\'/g $HOME/McStas/mcstas/3.0-dev/tools/Python/mccodelib/mccode_config.py
