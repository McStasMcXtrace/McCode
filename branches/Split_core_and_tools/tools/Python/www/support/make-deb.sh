#!/bin/sh

if [ ! "$0" = "./`basename $0`" ]; then
    echo 'This script must be run within "support" folder.';
    exit 1;
fi

NAME=mcstas-web.deb
DIST=debian/mcstas-web/usr/local/mcstas-web

mkdir -p ${DIST}
(
    # clean out everything but the lib folder
    cd ${DIST}
    rm -rf !lib
)


# copy files
echo '. Copy files and directories..'
cp -P ../*.{py,txt} ${DIST}

# copy dirs
cp -rP ../{bin,scripts,nginx,rplot,static,templates} ${DIST}

# init empty dirs
mkdir -p ${DIST}/{data,logs,out,sim/src}

# pre-download library sources
echo ''
echo '. Get dependencies..'
(
    cd ${DIST}
    ./bin/get-dependencies.sh --no-build
)

# generate new debian package
sudo dh_md5sums
sudo dh_builddeb
