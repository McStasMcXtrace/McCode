#!/bin/sh
#
# Shellscript snippet for extracting mcdoc info from named components in subdir
#
#

BASEDIR=../../../mcstas-comps/
DIRNAME=$1

for COMP in `cat $DIRNAME/mcdoc_index`
do
    mcdoc -t $BASEDIR/$COMP.comp > $DIRNAME/$COMP.txt
done
