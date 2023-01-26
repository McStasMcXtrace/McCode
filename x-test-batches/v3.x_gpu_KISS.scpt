#!/bin/sh
### General options
### –- specify queue --
#BSUB -q gpuv100
#BSUB -R "select[test]"
#BSUB -R "select[gpu32gb]"
### -- set the job Name --
#BSUB -J McXtrace_test_job
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
#BSUB -o mcxtrace_v3x_gpu-%J.out
#BSUB -e mcxtrace_v3x_gpu_%J.err
# -- end of LSF options --

# Ensure we run with our own miniconda3
PATH=${HOME}/McXtrace/mcxtrace/3.x-dev-dev/miniconda3/bin:$PATH

DATE=`date +%F`
mkdir -p $HOME/xTESTS/
mkdir -p $HOME/xTESTS/${DATE}

cd $HOME/xTESTS/${DATE}

$HOME/mxtest/mctest/mctest.py --ncount=5e7 --configs --mccoderoot $HOME/McXtrace/mcxtrace --verbose --testdir $HOME/xTESTS/${DATE} --openacc --config=McXtrace_GPU_PGCC_TESLA_KISS

cd $HOME

echo done on GPU with split
#bsub < $HOME/McCode/test-batches/plots_gpu.scpt 
