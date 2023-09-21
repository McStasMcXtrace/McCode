#!/bin/sh
#
# Script for bootstrapping a miniconda3 environment for packaging in rpm 
# format

# Download the installer
cd `dirname $0`
wget curl -L -O "https://github.com/conda-forge/miniforge/releases/latest/download/Mambaforge-$(uname)-$(uname -m).sh"
chmod a+x Mambaforge-$(uname)-$(uname -m).sh
./Mambaforge-$(uname)-$(uname -m).sh -b -b -p $PWD/miniconda3/
export PATHBAK=$PATH
export PATH=$PWD/miniconda3/bin:$PATH
$PWD/miniconda3/bin/mamba env update base -f ../../../environment.yml
export PATH=$PATHBAK
sed -i.orig 's+${PWD}/miniconda3/bin/python+/usr/local/mcstas/${1}/miniconda3/bin/python/+' ${PWD}/miniconda3/bin/*
cd -

