#!/bin/sh
### General options
### –- specify queue --
#BSUB -q gpuv100
### -- set the job Name --
#BSUB -J McXtrace_test_job
### -- ask for number of cores (default: 1) --
#BSUB -n 2
### -- Select the resources: 1 gpu in exclusive process mode --
#BSUB -gpu "num=2:mode=exclusive_process"
### -- set walltime limit: hh:mm --  maximum 24 hours for GPU-queues right now
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
#BSUB -o gpu-%J.out
#BSUB -e gpu_%J.err
# -- end of LSF options --

# Ensure we run with our own miniconda3
PATH=${HOME}/miniconda3/bin:$PATH

DATE=`date +%F`
mkdir -p $HOME/xTESTS/
mkdir -p $HOME/xTESTS/${DATE}

cd $HOME/xTESTS/${DATE}
export LD_LIBRARY_PATH=/appl/gcc/8.3.0/openmpi/4.0.1-lsf10/lib:$LD_LIBRARY_PATH
$HOME/mxtest/mctest/mctest.py --ncount=5e7 --configs --mccoderoot $HOME/McXtrace/mcxtrace --verbose --testdir $HOME/xTESTS/${DATE} --config=McXtrace_multiGPU_PGCC_TESLA_KISS --mpi=2

cd $HOME

echo done on multi-GPU 
# 10-core MPI run 
#bsub < $HOME/McCode/test-batches/plots_multigpu.scpt 
