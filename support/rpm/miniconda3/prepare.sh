#!/bin/sh
#
# Script for bootstrapping a miniconda3 environment for packaging in rpm 
# format

# Download the installer
curl -O https://repo.continuum.io/miniconda/Miniconda3-latest-Linux-x86_64.sh
chmod a+x Miniconda3-latest-Linux-x86_64.sh
./Miniconda3-latest-Linux-x86_64.sh -b -p $PWD/miniconda3/
export PATHBAK=$PATH
export PATH=$PWD/miniconda3/bin:$PATH
$PWD/miniconda3/bin/pip install PyQt5 Qscintilla pyqtgraph pyaml ply matplotlib numpy tornado jinja2 mpld3
export PATH=$PATHBAK
tar cfz miniconda3.tgz miniconda3
