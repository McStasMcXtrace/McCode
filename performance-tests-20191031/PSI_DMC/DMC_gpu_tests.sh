#!/bin/bash
(time ./PSI_DMC_simple_gpu.out lambda=2.566 -n 1e4 -dPSI_DMC_simple_gpu_1e4 -s1000) &> PSI_DMC_simple_gpu_log
echo 1e5
(time ./PSI_DMC_simple_gpu.out lambda=2.566 -n 1e5 -dPSI_DMC_simple_gpu_1e5 -s1000) &>> PSI_DMC_simple_gpu_log
echo 1e6
(time ./PSI_DMC_simple_gpu.out lambda=2.566 -n 1e6 -dPSI_DMC_simple_gpu_1e6 -s1000) &>> PSI_DMC_simple_gpu_log
echo 1e7
(time ./PSI_DMC_simple_gpu.out lambda=2.566 -n 1e7 -dPSI_DMC_simple_gpu_1e7 -s1000) &>> PSI_DMC_simple_gpu_log
echo 1e8
(time ./PSI_DMC_simple_gpu.out lambda=2.566 -n 1e8 -dPSI_DMC_simple_gpu_1e8 -s1000) &>> PSI_DMC_simple_gpu_log
echo 1e9
(time ./PSI_DMC_simple_gpu.out lambda=2.566 -n 1e9 -dPSI_DMC_simple_gpu_1e9 -s1000) &>> PSI_DMC_simple_gpu_log
echo done
