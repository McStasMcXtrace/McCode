#!/bin/sh

source  /lsf/conf/profile.lsf

cd $HOME

find TESTS/ -name Detector\*_Sqw_\* -exec rm \{\} \;
find TESTS/ -name \*_list.p.n.U1.E.x.y.z.vx.vy.vz.t -exec rm \{\} \;
  bsub < McCode/test-batches/plots_cpu_MPI_KISS.scpt
  bsub < McCode/test-batches/plots_cpu_KISS.scpt
  bsub < McCode/test-batches/plots_cpu_multicore.scpt
  bsub < McCode/test-batches/plots_gpu.scpt
  bsub < McCode/test-batches/plots_gpu_nosplit.scpt
  bsub < McCode/test-batches/plots_8gpu.scpt
  
sleep 30

  bsub < McCode/test-batches/plots_cpu_MPI_KISS.scpt
  bsub < McCode/test-batches/plots_cpu_KISS.scpt
  bsub < McCode/test-batches/plots_cpu_multicore.scpt
  bsub < McCode/test-batches/plots_gpu.scpt
  bsub < McCode/test-batches/plots_gpu_nosplit.scpt
  bsub < McCode/test-batches/plots_8gpu.scpt
