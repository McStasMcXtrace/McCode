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

cd $BASEDIR
