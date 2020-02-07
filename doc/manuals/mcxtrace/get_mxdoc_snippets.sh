#!/bin/sh
#
# Shellscript snippet for extracting mxdoc info from named components in subdir
#
#

echo "$0 $1 $2 $3"
pwd
ls $1*
DIRNAME=`basename $1`
PW=$PWD
SRCDIR=$2
echo "PW=$PWD, DN=$DIRNAME, SD=$SRCDIR"
PREFIX=$SRCDIR/COMPprefix
HEADER=$SRCDIR/COMPheader
FOOTER=$SRCDIR/COMPfooter

#search upwards for comps directory
COMP_SOURCE_DIR=$PW
while [ ! -d $COMP_SOURCE_DIR/mcxtrace-comps ] && [ "x$COMP_SOURCE_DIR" != "x" ]
do
  COMP_SOURCE_DIR=${COMP_SOURCE_DIR%/*}
done

for COMP in `cat $SRCDIR/$DIRNAME/mxdoc_index`
do
    echo "get_mxdoc_snippets: generating $DIRNAME/${COMP}.parms"
    sed s/@COMP@/$COMP/g $PREFIX | sed s/@CAT@/$DIRNAME/g > $DIRNAME/$COMP.parms
    cat $HEADER >> $DIRNAME/$COMP.parms
    cd $COMP_SOURCE_DIR/mcxtrace-comps
    mxdoc.pl --exact -t $COMP.comp | grep -A1000 \#\ Input | grep -B1000 \#\ Output | grep -ve ^\ *$ | grep -v \# >> $PW/$DIRNAME/$COMP.parms
    cd $PW
    cat $FOOTER >> $DIRNAME/$COMP.parms
done
touch $DIRNAME.done
