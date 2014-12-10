#!/bin/sh
#
# Shellscript snippet for extracting mxdoc info from named components in subdir
#
#

DIRNAME=`basename $1`
PW=$PWD
PREFIX=$PWD/COMPprefix
HEADER=$PWD/COMPheader
FOOTER=$PWD/COMPfooter

#search upwards for comps directory
COMP_SOURCE_DIR=$PW
while [ ! -d $COMP_SOURCE_DIR/mcxtrace-comps ] && [ "x$COMP_SOURCE_DIR" != "x" ]
do
  COMP_SOURCE_DIR=${COMP_SOURCE_DIR%/*}
done

for COMP in `cat $DIRNAME/mxdoc_index`
do
    sed s/@COMP@/$COMP/g $PREFIX | sed s/@CAT@/$DIRNAME/g > $DIRNAME/$COMP.parms
    cat $HEADER >> $DIRNAME/$COMP.parms
    cd $COMP_SOURCE_DIR/mcxtrace-comps
    mxdoc -t $COMP.comp | grep -A1000 \#\ Input | grep -B1000 \#\ Output | grep -ve ^\ *$ | grep -v \# >> $PW/$DIRNAME/$COMP.parms
    cd $PW
    cat $FOOTER >> $DIRNAME/$COMP.parms
done
touch $DIRNAME.done
