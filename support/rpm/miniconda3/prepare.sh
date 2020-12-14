#!/bin/sh
#
# Script for bootstrapping a miniconda3 environment for packaging in rpm 
# format

# Download the installer
cd `dirname $0`
wget https://repo.continuum.io/miniconda/Miniconda3-latest-Linux-x86_64.sh
chmod a+x Miniconda3-latest-Linux-x86_64.sh
./Miniconda3-latest-Linux-x86_64.sh -b -p $PWD/miniconda3/
export PATHBAK=$PATH
export PATH=$PWD/miniconda3/bin:$PATH
$PWD/miniconda3/bin/pip install PyQt5 Qscintilla pyqtgraph pyaml ply matplotlib numpy tornado scipy pillow jinja2 mpld3
export PATH=$PATHBAK
sed -i.orig 's+${PWD}/miniconda3/bin/python+/usr/local/mcstas/${1}/miniconda3/bin/python/+' ${PWD}/miniconda3/bin/*
cd -

