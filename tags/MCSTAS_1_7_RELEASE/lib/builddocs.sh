#! /bin/sh

export http_proxy="proxy.ill.fr:8888"

SRCLIST="http://neutron.risoe.dk/mcstas/mcdoc/src/risoe/ http://www.ill.fr/tas/mcstas/src/"
DIRLIST="sources optics samples monitors misc obsolete"
MCDOC="http://neutron.risoe.dk/mcstas/mcdoc/src/risoe/mcdoc.pl"

for d in $DIRLIST
do
    rm -Rf $d
    mkdir $d
    for src in $SRCLIST
    do
	(cd $d; wget -nd -L -A comp,cmp,com -r $src/$d/; rm index.html)
    done
done

rm -f mcdoc.pl
wget -nd -L $MCDOC
chmod u+x mcdoc.pl
./mcdoc.pl

rm -Rf gathered
mkdir gathered
cp index.html gathered
for d in $DIRLIST
do
    cp $d/*.comp gathered
    cp $d/*.cmp gathered
    cp $d/*.com gathered
    cp $d/*.html gathered
done
