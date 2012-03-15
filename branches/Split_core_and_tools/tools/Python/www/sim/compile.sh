#!/bin/bash

BASEDIR=$(dirname $0)
cd $BASEDIR

echo "Translating and compiling (sim/src -> sim/bin)"

for i in $( ls src/*.instr ); do
    echo "> $i"
    b=`basename $i .instr`
    if [ -f src/$b.c ]; then
        echo "* c file exists";
    else
        echo "* generating c file..";
        mcstas -o src/$b.c $i &> /dev/null;
    fi
    if [ -x bin/$b ]; then
        echo "* executable exists";
    else
        echo "* compiling..";
        gcc -lm -O3 -o bin/$b -O2 src/$b.c
    fi
done
