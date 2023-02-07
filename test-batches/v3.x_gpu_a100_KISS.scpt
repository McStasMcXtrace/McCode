#!/bin/sh
### General options
### –- specify queue --
#BSUB -q gpua100
#BSUB -R "select[test]"
### -- set the job Name --
#BSUB -J McStas_test_job
### -- ask for number of cores (default: 1) --
#BSUB -n 1
### -- Select the resources: 1 gpu in exclusive process mode --
#BSUB -gpu "num=1:mode=exclusive_process"
### -- set walltime limit: hh:mm --  maximum 24 hours for GPU-queues right now
#BSUB -W 20:00
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
#BSUB -o gpu-%J.out
#BSUB -e gpu_%J.err
# -- end of LSF options --

# Ensure we run with our own miniconda3
PATH=${HOME}/McStas/mcstas/3.x-dev/miniconda3/bin:$PATH

module load cmake/3.18.2 
module load nvhpc/22.7-nompi
module load cuda/11.7.1
module load mpi/4.1.1-gcc-10.3.0-binutils-2.36.1
module load hdf5/1.12.1-gcc-10.3.0


DATE=`date +%F`
mkdir -p $HOME/TESTS/
mkdir -p $HOME/TESTS/${DATE}

cd $HOME/TESTS/${DATE}

$HOME/McCode/tools/Python/mctest/mctest.py --ncount=5e7 --configs --mccoderoot $HOME/McStas/mcstas --verbose --testdir $HOME/TESTS/${DATE} --openacc --config=McStas_GPU_A100_PGCC_TESLA_KISS

cd $HOME

echo done on GPU with split

bsub < $HOME/McCode/test-batches/plots_gpu_a100.scpt 
