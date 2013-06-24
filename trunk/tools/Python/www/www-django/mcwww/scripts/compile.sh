#!/bin/sh

echo "* Translating and compiling (from sim/)"


# read config value for MPI_NP
MPI=`python -c "from mcwww.settings import MPI_NP; print MPI_NP"`

MCRUN_FLAGS=""
if [ $MPI -gt 0 ]; then
    MCRUN_FLAGS="--mpi=${MPI}";
fi


cd sim

for dir in $( ls -d * | grep -v datafiles ); do
  cd $dir
  echo "Instrument group $dir..."
    for i in $( ls *.instr ); do
      echo "> $i"
      b=`basename $i .instr`
      mcrun ${MCRUN_FLAGS} --trace --info $i &>/dev/null;
      cd -
    done
done
