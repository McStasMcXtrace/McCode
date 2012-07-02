#!/bin/sh

if [ ! "$0" = "./`basename $0`" ]; then
    echo 'This script must be run within "support/deb" folder.';
    exit 1;
fi

NAME=mcstas-web.deb
DIST=data/usr/local/mcstas-web

mkdir -p ${DIST}
(
    # clean out everything but the lib folder
    cd ${DIST}
    rm -rf !lib
)

echo '2.0' > debian-binary

echo '. Clean old archives..'
rm -f ${NAME}
rm -f {control,data}/*.tar.gz


# copy files
echo '. Copy files and directories..'
cp -P ../../*.{py,txt} ${DIST}

# copy dirs
cp -rP ../../{bin,nginx,rplot,static,templates} ${DIST}

# init empty dirs
mkdir -p ${DIST}/{data,logs,out,sim/src}

# pre-download library sources
echo ''
echo '. Get dependencies..'
(
    cd ${DIST}
    ./bin/get-dependencies.sh --no-build
)

# initialise new debian package
echo ''
echo ". Goal: ${NAME}"
ar -r ${NAME} debian-binary;

# generate md5 listing
echo ''
echo '. Generate: control/md5sums..'
cd data;
md5sum `find . -type f` > ../control/md5sums;
cd ..;

# pack 'control' and 'data'
echo '. Pack files:'
DIRS="control data"
for d in ${DIRS}; do
    echo "> ${d}"
    cd ${d};
    tar czf ${d}.tar.gz *;
    cd ..;
    ar -r ${NAME} ${d}/${d}.tar.gz;
done

echo ''
echo "${NAME} is ready."
