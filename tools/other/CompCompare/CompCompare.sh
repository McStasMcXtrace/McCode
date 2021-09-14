#!/usr/bin/env bash
#
# Check the main comp cathegories against each other in
# two McStas installations, with path $1 and $2
#

COMPDIRS="sources optics samples monitors misc contrib contrib/union obsolete"

for DIR in `echo $COMPDIRS`
do
    DIR1COMPS=`cd ${1}/${DIR}/ && ls *.comp`
    NUM1COMPS=`echo ${DIR1COMPS} | wc -w`
    DIR2COMPS=`cd ${2}/${DIR}/ && ls *.comp`
    NUM2COMPS=`echo ${DIR2COMPS} | wc -w`
    V1=`$1/bin/mcstas -v | head -1 | cut -f3 -d\  `
    V2=`$2/bin/mcstas -v | head -1 | cut -f3 -d\  `
    DIRfn=`echo $DIR | sed s+/+_+g`
    echo
    echo "-------------------------------------------------------------------------------"
    echo Category $DIR:
    echo "-------------------------------------------------------------------------------"
    echo "-------------------------------------------------------------------------------"
    echo $V1 $DIR has $NUM1COMPS components
    `echo $DIR1COMPS | xargs -n1 > mcstas_${V1}_${DIRfn}.txt`
    echo $V2 $DIR has $NUM2COMPS components
    `echo $DIR2COMPS | xargs -n1 > mcstas_${V2}_${DIRfn}.txt`
    echo "-------------------------------------------------------------------------------"
    echo $DIR diff:
    echo "-------------------------------------------------------------------------------"
    diff mcstas_${V1}_${DIRfn}.txt mcstas_${V2}_${DIRfn}.txt
    `diff mcstas_${V1}_${DIRfn}.txt mcstas_${V2}_${DIRfn}.txt > ${DIRfn}.diff`
    DIFFCNT=`wc -l ${DIRfn}.diff | cut -f1 -d\t `
    echo diff count is $DIFFCNT
    echo $DIFFCNT
    mkdir -p $DIR
    for COMP in `echo $DIR1COMPS`
    do
	if [ -e ${1}/${DIR}/$COMP ];
	then
	    if [ -e ${2}/${DIR}/$COMP ];
	    then
		`grep -B1000 DEFINE ${1}/${DIR}/${COMP} > ${DIR}/${V1}_${COMP}.header`
		`grep -B1000 DEFINE ${2}/${DIR}/${COMP} > ${DIR}/${V2}_${COMP}.header`
		`diff ${DIR}/${V1}_${COMP}.header ${DIR}/${V2}_${COMP}.header > ${DIR}/${COMP}.diff`
	    fi
	fi
    done
done
