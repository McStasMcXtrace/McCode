#!/bin/bash
# Locate candidates for comps needing simplification / fixes wrt. DECLARE block

for COMP in `find mcstas-comps -name \*.comp`
do
    NUM_COMMA=`grep -A1000 DECLARE $COMP | grep -B 1000 INITIALIZE | grep -v INITIALIZE |grep , | wc -l`
    if [ $NUM_COMMA = 0 ]; then
#	echo $NUM_COMMA
#	echo $COMP looks good
	echo 
    else
	echo Looks like $COMP needs fixing in the DECLARE struct:
	grep -A1000 DECLARE $COMP | grep -B 1000 INITIALIZE | grep -v INITIALIZE | grep ,
	echo ----
    fi
       
       
done
