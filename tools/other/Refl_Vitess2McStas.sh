#!/bin/sh
#
# Script to convert Vitess reflectivity file to McStas-compatible format
#

if [ "x$1" = "x" ]; then
    # No arguments
    echo $0 error:
    echo Please provide one argument,e.g : 
    echo
    echo unix\: \~ \>  $0 mirr1a.dat \> mirr1a_McStas.dat
    echo
    exit 1;
fi

if [ -f $1 ]; then
    
    NUMREF=`cat $1 | grep -v \# | xargs -n1 | wc -l`
    REFLECTIVITIES=`cat $1 | grep -v \# | xargs -n1 echo > $1.refs`
    QC="0.0219"
    echo 'for(j=1;j<='${NUMREF}';j++) {
              0.01*j*'${QC}';
          }' | bc > $1.ks
    echo \# Converted Vitess reflectivity file $1 for use with McStas
    echo \# $NUMREF reflectivity entries
    echo \#
    grep \# $1 
    paste $1.ks $1.refs
    echo \# Reformat done.
    #rm $1.ks $1.refs
else
    echo $0 error:
    echo Vitess relfectivity file $1 not found!
    exit 1;
fi