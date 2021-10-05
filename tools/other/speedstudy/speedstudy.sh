#!/usr/bin/env bash

# First, check if $1 is a valid instrument file
NUMMPI=8
INSTR=$1
PARMS=${*:2}
DIR=`basename -s.instr $INSTR`
INSTR=`echo ${DIR}.instr`

if [ -f $MCSTAS/examples/$INSTR ];
then
    DIR=`basename -s.instr $INSTR`
    
    echo Found $INSTR in $MCSTAS, copying to $DIR
    mkdir -p $DIR
    
    cp $MCSTAS/examples/$INSTR $DIR
    cd $DIR
      
    EXAMPLE=`grep %Example $INSTR | cut -f2 -d: | sed s/Detector//g`
    echo Instrument example lines are $EXAMPLE
    echo
    echo Will be tested with parameters $PARMS
    
    if [ -d one_cpu ];
    then
	rm -rf one_cpu
    fi
    mkdir one_cpu
    cd one_cpu && ln -s ../$INSTR .
    (time mcrun -c $INSTR -n0 ) &> compile.log
    echo one_cpu compile done
    cd - 
    
    if [ -d mpi_x_${NUMMPI} ];
    then
	rm -rf mpi_x_${NUMMPI}
    fi
    mkdir mpi_x_${NUMMPI}
    cd mpi_x_${NUMMPI} && ln -s ../$INSTR .
    (time mcrun --mpi=1 -c $INSTR -n0 ) &> compile.log
    echo mpi compile done
    cd -

    if [ -d openacc ];
    then
	rm -rf openacc
    fi
    mkdir openacc
    cd openacc && ln -s ../$INSTR .
    (time mcrun --openacc -c $INSTR -n0 ) &> compile.log
    echo openacc compile done
    cd -
    
    if [ -d openacc_mpi_x_${NUMMPI} ];
    then
	rm -rf openacc_mpi_x_${NUMMPI}
    fi
    mkdir openacc_mpi_x_${NUMMPI}
    cd openacc_mpi_x_${NUMMPI} && ln -s ../$INSTR .
    (time mcrun --openacc --mpi=1 -c $INSTR -n0 ) &> compile.log
    echo openacc_mpi compile done
    cd -
    
    cd one_cpu
    for ncount in `echo 1e4 1e5 1e6 1e7`
    do
	(time -p mcrun $INSTR -n $ncount $PARMS -d $ncount) &>> results.log
	echo $ncount done on one_cpu
	TIME=`tail -3 results.log | grep real | sed s/real//g `
	echo $ncount $TIME >> results.dat
    done
    cd -
    
    cd mpi_x_${NUMMPI}
    for ncount in `echo 1e4 1e5 1e6 1e7 1e8`
    do
	(time -p mcrun --mpi=$NUMMPI $INSTR -n $ncount $PARMS -d $ncount) &>> results.log
	echo $ncount done with mpi	
	TIME=`tail -3 results.log | grep real | sed s/real//g `
	echo $ncount $TIME >> results.dat
    done
    cd -

    cd openacc
    for ncount in `echo 1e4 1e5 1e6 1e7 1e8 1e9`
    do
	(time -p mcrun $INSTR -n $ncount $PARMS -d $ncount) &>> results.log
	echo $ncount done using openacc
	TIME=`tail -3 results.log | grep real | sed s/real//g `
	echo $ncount $TIME >> results.dat
    done
    cd -

    cd openacc_mpi_x_${NUMMPI}
    for ncount in `echo 1e4 1e5 1e6 1e7 1e8 1e9 1e10`
    do
	(time -p mcrun --mpi=$NUMMPI $INSTR -n $ncount $PARMS -d $ncount) &>> results.log
	echo $ncount done using openacc_mpi
	TIME=`tail -3 results.log | grep real | sed s/real//g `
	echo $ncount $TIME >> results.dat
    done
    cd -

    
    
else
    echo Instrument $INSTR \(\$1\) was not found!
fi
