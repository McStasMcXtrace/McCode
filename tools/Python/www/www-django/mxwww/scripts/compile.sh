#!/bin/sh

echo "* Translating and compiling (from sim/)"


# read config value for MPI_NP
MPI=`python -c "from mxwww.settings import MPI_NP; print MPI_NP"`

MXRUN_FLAGS=""
if [ $MPI -gt 0 ]; then
    MXRUN_FLAGS="--mpi=${MPI}";
fi


cd sim

for dir in $( ls -d * | grep -v datafiles ); do
  cd $dir
  echo "Instrument group $dir..."
  mxdoc -t .
    for i in $( ls *.instr ); do
      echo "> $i"
      b=`basename $i .instr`
      mxrun ${MXRUN_FLAGS} --trace --info $i &>/dev/null;
      
    done
  mv *.html ..
  cd -
done
