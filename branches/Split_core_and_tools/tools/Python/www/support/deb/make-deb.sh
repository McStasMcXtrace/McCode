#!/bin/sh

NAME=mcstas-web.deb

echo '2.0' > debian-binary

echo '. Clean old archives..'
rm -vf ${NAME}
rm -vf {control,data}/*.tar.gz

# copy files
# TODO

# initialise new debian package
echo ''
echo ". Goal: ${NAME}"
ar -r ${NAME} debian-binary;

# generate md5 listing
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
