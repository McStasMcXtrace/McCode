#!/bin/sh


if [ "${1}x" == "x" ]; then
    echo Please provide me with at least an instrument filename in '.'
else
    if [ -f "$1" ]; then
	echo "$1 exists, proceeding..."
	BASE=`basename $1 .instr`
	echo $BASE is the base instrument, making _cpu and _gpu clone variants
	# First, copy instr code to mpi variant shadow-copy
	ln -sf ${BASE}.instr ${BASE}_cpu.instr
	ln -sf ${BASE}.instr ${BASE}_gpu.instr
	
	echo Spawning requested run in xterms and waiting here... 
	echo mcrun --openacc ${BASE}_gpu.instr $2 $3 $4 $5 $6 $7 $8 $9 ${10}  --autoplot -s 1000
	echo mcrun ${BASE}_cpu.instr $2 $3 $4 $5 $6 $7 $8 $9 ${10}  --autoplot -s 1000
	xterm -T "GPU run..." -e mcrun --openacc ${BASE}_gpu.instr $2 $3 $4 $5 $6 $7 $8 $9 ${10}  --autoplot -s 1000 &
	xterm -T "CPU run..." -e mcrun ${BASE}_cpu.instr $2 $3 $4 $5 $6 $7 $8 $9 ${10}  --autoplot -s 1000 
	echo  Done.
    else
	echo "$1 does not exist, sorry..."
    fi
fi
