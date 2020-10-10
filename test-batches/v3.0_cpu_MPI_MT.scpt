#!/bin/sh
### General options
### –- specify queue --
#BSUB -q hpc
### -- set the job Name --
#BSUB -J McStas_test_job
### -- ask for number of cores (default: 1) --
#BSUB -n 8
#BSUB -R "span[block=1]"
#BSUB -W 10:00
# request 5GB of system-memory
#BSUB -R "rusage[mem=5GB]"
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
#BSUB -o cpu-%J.out
#BSUB -e cpu_%J.err
# -- end of LSF options --

#module load mpi/3.0.0-gcc-6.4.0
module list
which mpicc

DATE=`date +%F`
mkdir -p $HOME/TESTS/
mkdir -p $HOME/TESTS/${DATE}

cd $HOME/TESTS/${DATE}

$HOME/McCode/tools/Python/mctest/mctest.py --ncount=5e7 --mpi=auto --configs --mccoderoot $HOME/McStas/mcstas --verbose --testdir $HOME/TESTS/${DATE} --config=McStas_CPU_MPICC_MT

cd $HOME
echo done on CPU/MPI, submitting next job
bsub < $HOME/McCode/test-batches/plots_cpu_MPI_MT.scpt
