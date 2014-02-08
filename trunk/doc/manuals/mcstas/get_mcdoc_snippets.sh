#!/bin/sh
#
# Shellscript snippet for extracting mcdoc info from named components in subdir
#
#

BASEDIR=../../../mcstas-comps/
DIRNAME=`basename $1`
PW=$PWD
HEADER=$PWD/COMPheader
FOOTER=$PWD/COMPfooter
for COMP in `cat $DIRNAME/mcdoc_index`
do
    cd $DIRNAME/
    cp $HEADER $COMP.parms
    mcdoc -t $COMP.comp | grep -A1000 \#\ Input | grep -B1000 \#\ Output | grep -v \# | sed "s/_/\\\\_/g" | sed "s/\</\\\\\\\\\\\hline\ /g" | sed "s/\>\:/\\ \&\\ /g" | sed "s/\]/\]\$\&\ /g" | sed "s/\[/\$\[/g" >> $COMP.parms
    cat $FOOTER >> $COMP.parms
    cd $PW
done
