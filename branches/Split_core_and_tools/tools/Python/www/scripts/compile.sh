#!/bin/sh

echo "Translating and compiling (from sim/)"

cd sim

for i in $( ls *.instr ); do
    echo "> $i"
    b=`basename $i .instr`
    if [ -f $b.c ]; then
        echo "* c file exists";
    else
        echo "* generating c file..";
        mcstas --trace -O3 -o $b.c $i > /dev/null 1>&2;
    fi
    if [ -x $b.out ]; then
        echo "* executable exists";
    else
        echo "* compiling..";
        gcc -lm -O3 -o $b.out -O2 $b.c
    fi
done
