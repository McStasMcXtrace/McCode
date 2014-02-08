#!/bin/sh
#
# Shellscript snippet for extracting mcdoc info from named components in subdir
#
#

BASEDIR=../../../mcstas-comps/
DIRNAME=`basename $1`
PW=$PWD
for COMP in `cat $DIRNAME/mcdoc_index`
do
    cd $DIRNAME/
    mcdoc -t $COMP.comp | grep -A1000 \#\ Input | grep -B1000 \#\ Output | grep -v \# > $COMP.parms
    cd $PW
done
