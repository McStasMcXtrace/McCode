#!/bin/sh

echo "* Translating and compiling (from sim/)"


# read config value for MPI_NP
MPI=`python -c "from config import MPI_NP; print MPI_NP"`

MCRUN_FLAGS=""
if [ $MPI -gt 0 ]; then
    MCRUN_FLAGS="--mpi=${MPI}";
fi


cd sim

for i in $( ls *.instr ); do
    echo "> $i"
    b=`basename $i .instr`
    mcrun ${MCRUN_FLAGS} --trace -n0 $i >/dev/null 2>&1;
done
