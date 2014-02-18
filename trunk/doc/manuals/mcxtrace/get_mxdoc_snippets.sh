#!/bin/sh
#
# Shellscript snippet for extracting mcdoc info from named components in subdir
#
#

BASEDIR=$2/../../../mcxtrace-comps/
DIRNAME=`basename $1`
PW=$PWD
PREFIX=$PWD/COMPprefix
HEADER=$PWD/COMPheader
FOOTER=$PWD/COMPfooter
for COMP in `cat $DIRNAME/mcdoc_index`
do
    cd $DIRNAME/
    sed s/@COMP@/$COMP/g $PREFIX | sed s/@CAT@/$DIRNAME/g > $COMP.parms
    cat $HEADER >> $COMP.parms
    mcdoc -t $COMP.comp | grep -A1000 \#\ Input | grep -B1000 \#\ Output | grep -v \#  >> $COMP.parms
    cat $FOOTER >> $COMP.parms
    cd $PW
done
touch $DIRNAME.done
