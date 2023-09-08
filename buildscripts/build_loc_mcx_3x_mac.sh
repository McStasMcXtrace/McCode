#!/bin/bash
export CMAKE_BUILD_PARALLEL_LEVEL=${CMAKE_BUILD_PARALLEL_LEVEL:-$(python3 -c "import os;print(os.cpu_count())")}

# rapid-build script for mcxtrace-kernel dev:


git checkout main
git pull

# Ensure our 3rd party modules are in place and updated
THIRDPARTY=`ls 3rdparty | grep -v patches`

./3rdparty/sync.sh

WORK=`pwd`

export MCINSTALL_PREFIX=$HOME/McXtrace
export CONDALOCATION=$MCINSTALL_PREFIX/mcxtrace/3.x-dev/miniconda3

if [[ -d $HOME/McXtrace/mcxtrace/3.x-dev ]]
then
    cd $HOME/McXtrace/mcxtrace/3.x-dev
    ls | grep -v miniconda3 | xargs -n1 rm -rf 
    cd -
fi

if [ ! -f Mambaforge-$(uname)-$(uname -m).sh ]; then
    # Download and embed a miniconda
    echo
    echo "*********************************************************"
    echo "* Downloading a miniconda3 for your app                 *"
    echo "*********************************************************"
    echo
    sleep 3
    curl -L -O "https://github.com/conda-forge/miniforge/releases/latest/download/Mambaforge-$(uname)-$(uname -m).sh"
fi

if [[ !  -d $CONDALOCATION ]]; then
    echo "*********************************************************"
    echo "* Embedding miniconda3 in your app                      *"
    echo "*********************************************************"
    chmod a+x Mambaforge-$(uname)-$(uname -m).sh
    ./Mambaforge-$(uname)-$(uname -m).sh -b -p $CONDALOCATION
    # Run conda to install the dependencies
    echo
    echo "*********************************************************"
    echo "* Downloading python dependencies to embedded miniconda *"
    echo "*********************************************************"
    echo
    export PATH=$CONDALOCATION/bin:$PATH 
    mamba update mamba -y
    mamba install cmake compilers gsl pyaml ply matplotlib numpy tornado scipy pillow pyqtgraph pyqt nomkl qscintilla2 nexusformat nexpy hdf5 openmpi xraylib -y
    mamba clean --all -y
fi

export PATH=$CONDALOCATION/bin:$PATH

./mkdist mcxtrace 3.x-dev "" "" mac "" -- justinst
./mkdist mcxtrace-comps 3.x-dev "" "" mac "" -- justinst
./mkdist mcxtrace-tools-python-mxrun 3.x-dev tools/Python/mcrun/ "" mac "" -- justinst
./mkdist mcxtrace-tools-python-mxdoc 3.x-dev tools/Python/mcdoc/ "" mac "" -- justinst
./mkdist mcxtrace-tools-python-mxgui 3.x-dev tools/Python/mcgui/ "" mac "" -- justinst
./mkdist mcxtrace-tools-python-mccodelib 3.x-dev tools/Python/mccodelib/ "" mac "" -- justinst
./mkdist mcxtrace-tools-python-mxdisplay-pyqtgraph 3.x-dev tools/Python/mcdisplay/pyqtgraph/ "" mac "" -- justinst
./mkdist mcxtrace-tools-python-mxplot-pyqtgraph 3.x-dev tools/Python/mcplot/pyqtgraph/ "" mac "" -- justinst
./mkdist mcxtrace-tools-python-mxplot-svg 3.x-dev tools/Python/mcplot/svg/ "" mac "" -- justinst
./mkdist mcxtrace-tools-python-mxplot-matplotlib 3.x-dev tools/Python/mcplot/matplotlib/ "" mac "" -- justinst
cp tools/other/mcsplit/mcsplit.py $MCINSTALL_PREFIX/mcxtrace/3.x-dev/bin/

export MCINSTALL_PREFIX=$HOME/McXtrace/mcxtrace/3.x-dev/
./mkdist mcxtrace-mcpl 3.x-dev 3rdparty/mcpl-package "" mac "" -- justinst
./mkdist mcxtrace-nexus 3.x-dev 3rdparty/nexus-package "" mac "" -- justinst

# Ensure we are configured for 10 node MPI runs
sed -i s/\'4\'/\'10\'/g $MCINSTALL_PREFIX/mcxtrace/3.x-dev/tools/Python/mccodelib/mccode_config.py
