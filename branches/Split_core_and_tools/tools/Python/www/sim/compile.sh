#!/bin/sh

BASEDIR=$(dirname $0)
cd $BASEDIR

echo "Translating and compiling (from sim/src)"

for i in $( ls src/*.instr ); do
    echo "> $i"
    b=`basename $i .instr`
    if [ -f src/$b.c ]; then
        echo "* c file exists";
    else
        echo "* generating c file..";
        mcstas -o src/$b.c $i &> /dev/null;
    fi
    if [ -x src/$b.out ]; then
        echo "* executable exists";
    else
        echo "* compiling..";
        gcc -lm -O3 -o src/$b.out -O2 src/$b.c
    fi
done
