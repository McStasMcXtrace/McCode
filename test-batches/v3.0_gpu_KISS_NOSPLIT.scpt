#!/bin/sh
### General options
### –- specify queue --
#BSUB -q gpuv100
### -- set the job Name --
#BSUB -J McStas_test_job
### -- ask for number of cores (default: 1) --
#BSUB -n 1
### -- Select the resources: 1 gpu in exclusive process mode --
#BSUB -gpu "num=1:mode=exclusive_process"
### -- set walltime limit: hh:mm --  maximum 24 hours for GPU-queues right now
#BSUB -W 7:00
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

DATE=`date +%F`
mkdir -p $HOME/TESTS/
mkdir -p $HOME/TESTS/${DATE}

cd $HOME/TESTS/${DATE}

$HOME/McCode/tools/Python/mctest/mctest.py --ncount=1e9 --configs --mccoderoot $HOME/McStas/mcstas --verbose --testdir $HOME/TESTS/${DATE} --config=McStas_GPU_PGCC_TESLA_KISS_NOSPLIT

cd $HOME

echo done on GPU, submitting 1st MPI job and plots
# 10-core MPI run 
#bsub < $HOME/McCode/test-batches/v3.0_cpu_MPI_KISS.scpt
bsub < $HOME/McCode/test-batches/plots_gpu_nosplit.scpt 
rsync -avz elearn1.fysik.dtu.dk:TESTS/${DATE}/ TESTS/${DATE} --exclude='*.mcpl' --exclude='*.mcpl.gz' --exclude='*_list*' --exclude='*.out'
bsub < $HOME/McCode/test-batches/plots_cpu_MPI_KISS.scpt
bsub < $HOME/McCode/test-batches/plots_cpu_multicore.scpt
