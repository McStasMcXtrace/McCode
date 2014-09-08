#!/bin/sh

echo "* Translating and compiling (from sim/)"


# read config value for MPI_NP
MPI=`python -c "from mcwww.settings import MPI_NP; print MPI_NP"`

MCRUN_FLAGS=""
if [ $MPI -gt 0 ]; then
    MCRUN_FLAGS="--mpi=${MPI}";
fi


cd sim

for dir in $( find . -maxdepth 1 -type d | cut -b3- | grep -v datafiles | grep -v svn); do
  cd $dir
  echo "Instrument group $dir..."
  mcdoc -t .
    # Compile McStas instruments
    echo "McStas based in instruments..."
    for i in $( grep -i mcstas *.instr | cut -f1 -d: | uniq ); do
      echo "> $i"
      b=`basename $i .instr`
      
      mcrun ${MCRUN_FLAGS} --trace --info $i &>/dev/null;      
    done
    # Compile McXtrace instruments
    echo "McXtrace based in instruments..."
    for i in $( grep -i mcxtrace *.instr | cut -f1 -d: |uniq ); do
      echo "> $i"
      b=`basename $i .instr`

      mxrun ${MCRUN_FLAGS} --trace --info $i &>/dev/null;
    done
  mv *.html ..
  cd -
done
