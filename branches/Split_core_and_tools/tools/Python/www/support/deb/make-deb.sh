#!/bin/sh

NAME=mcstas-web.deb

echo '2.0' > debian-binary

# clean up
rm -f ${NAME}
DIRS="control data"
for d in ${DIRS}; do
    rm -f ${d}/${d}.tar.gz;
done

# initialise new debian package
rm -f ${NAME};
ar -r ${NAME} debian-binary;

# generate md5 listing
cd data;
md5sum `find . -type f` > ../control/md5sums;
cd ..;

# pack 'control' and 'data'
DIRS="control data"
for d in ${DIRS}; do
    cd ${d};
    tar czf ${d}.tar.gz *;
    cd ..;
    ar -r ${NAME} ${d}/${d}.tar.gz;
done
