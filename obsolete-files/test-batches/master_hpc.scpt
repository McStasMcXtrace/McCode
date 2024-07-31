#!/bin/sh
### General options
### –- specify queue --
#BSUB -q hpc
### -- set the job Name --
#BSUB -J McStas_test_job
### -- ask for number of cores (default: 1) --
#BSUB -n 10
#BSUB -R "span[block=1]"
### -- Select the resources: 1 gpu in exclusive process mode --
#BSUB -W 10:00
# request 10GB of system-memory
#BSUB -R "rusage[mem=10GB]"
### -- set the email address --
# please uncomment the following line and put in your e-mail address,
# if you want to receive e-mail notifications on a non-default address
#BSUB -u pkwi@fysik.dtu.dk
### -- send notification at start --
#BSUB -B
### -- send notification at completion--
#BSUB -N
### -- Specify the output and error file. %J is the job-id --
### -- -o and -e mean append, -oo and -eo mean overwrite --
#BSUB -o mcstas-master-%J.out
#BSUB -e mcstas-master_%J.err
# -- end of LSF options --

# Ensure we run with our own miniconda3
PATH=${HOME}/miniconda3/bin:$PATH

DATE=`date +%F`
mkdir -p $HOME/TESTS/
mkdir -p $HOME/TESTS/${DATE}

cd $HOME/TESTS/${DATE}

$HOME/McCode/tools/Python/mctest/mctest.py --ncount=5e7 --mpi=auto --configs --mccoderoot $HOME/McStas/mcstas --verbose --testdir $HOME/TESTS/${DATE} --config=McStas-2.x_CPU_MPICC

cd $HOME
$HOME/do_plots.sh


