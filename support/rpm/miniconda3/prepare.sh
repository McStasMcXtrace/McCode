#!/usr/bin/env bash
#
# Script for bootstrapping a miniconda3 environment for packaging in rpm 
# format

# Download the installer
cd `dirname $0`
wget "https://github.com/conda-forge/miniforge/releases/latest/download/Mambaforge-$(uname)-$(uname -m).sh"
chmod a+x Mambaforge-$(uname)-$(uname -m).sh
./Mambaforge-$(uname)-$(uname -m).sh -b -p $PWD/miniconda3/
export PATHBAK=$PATH
export PATH=$PWD/miniconda3/bin:$PATH
$PWD/miniconda3/bin/mamba install gsl pyaml ply matplotlib numpy tornado scipy pillow pyqtgraph pyqt nomkl qscintilla2 -y
export PATH=$PATHBAK
#pick a flavor
if [ "mcxtrace" == "$2" ]; then
  sed -i.orig 's+${PWD}/miniconda3/bin/python+/usr/local/mcxtrace/${1}/miniconda3/bin/python/+' ${PWD}/miniconda3/bin/*
else
  sed -i.orig 's+${PWD}/miniconda3/bin/python+/usr/local/mcstas/${1}/miniconda3/bin/python/+' ${PWD}/miniconda3/bin/*
fi 
cd -

