!/bin/sh
### General options
### –- specify queue --
#BSUB -q hpc
### -- set the job Name --
#BSUB -J Plots_test_job
### -- ask for number of cores (default: 1) --
#BSUB -n 1
### -- Select the resources: 1 gpu in exclusive process mode --
#BSUB -W 12:00
# request 5GB of system-memory
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
#BSUB -o plots-%J.out
#BSUB -e plots_%J.err
# -- end of LSF options --

DATE=`date +%F`
REF="McXtrace-1.x_CPU_MPICC"
TARGET="McXtrace_CPU_GCC_MT"
cd $HOME/xTESTS/

export MATLABPATH=${HOME}/McCode/generate_testplots/

matlab -r "generate_testplots('$DATE','$REF','$TARGET');"

$HOME/go5.sh

