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
    cd $BASEDIR/$DIRNAME/
    mcdoc -t ./$COMP.comp > $PW/$DIRNAME/$COMP.txt
    cd $PW
done
