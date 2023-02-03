#!/usr/bin/env bash
#
#
#

BASEDIR=$PWD

cd 3rdparty

if [ -d mcpl ]; then
    echo Updating existing mcpl clone
    cd mcpl
    git pull
    cd ..
else
    echo Cloning mcpl
    git clone https://github.com/mctools/mcpl.git
fi

if [ -d ncrystal ]; then
    echo Updating existing ncrystal clone
    cd ncrystal
    git pull
    cd ..
else
    echo Cloning ncrystal
    git clone https://github.com/mctools/ncrystal.git
fi

if [ -d nexus-code ]; then
    echo Updating existing NeXus clone
    cd nexus-code
    git pull
    cd ..
else
    echo Cloning NeXus
    git clone https://github.com/nexusformat/code nexus-code
fi

if [ -d xraylib ]; then
    echo Updating existing xraylib clone
    cd xraylib
    git pull
    cd ..
else
    echo Cloning xraylib (release 4.1.3)
    git clone https://github.com/tschoonj/xraylib
    cd xraylib
    git checkout xraylib-4.1.3
    git pull
fi

cd $BASEDIR
