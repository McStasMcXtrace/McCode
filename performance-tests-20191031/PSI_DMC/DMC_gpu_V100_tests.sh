#!/bin/bash
(time ./PSI_DMC_simple_gpu_V100.out lambda=2.566 -n 1e4 -dPSI_DMC_simple_gpu_V100_1e4 -s1000) &> PSI_DMC_simple_gpu_V100_log
echo 1e5
(time ./PSI_DMC_simple_gpu_V100.out lambda=2.566 -n 1e5 -dPSI_DMC_simple_gpu_V100_1e5 -s1000) &>> PSI_DMC_simple_gpu_V100_log
echo 1e6
(time ./PSI_DMC_simple_gpu_V100.out lambda=2.566 -n 1e6 -dPSI_DMC_simple_gpu_V100_1e6 -s1000) &>> PSI_DMC_simple_gpu_V100_log
echo 1e7
(time ./PSI_DMC_simple_gpu_V100.out lambda=2.566 -n 1e7 -dPSI_DMC_simple_gpu_V100_1e7 -s1000) &>> PSI_DMC_simple_gpu_V100_log
echo 1e8
(time ./PSI_DMC_simple_gpu_V100.out lambda=2.566 -n 1e8 -dPSI_DMC_simple_gpu_V100_1e8 -s1000) &>> PSI_DMC_simple_gpu_V100_log
echo 1e9
(time ./PSI_DMC_simple_gpu_V100.out lambda=2.566 -n 1e9 -dPSI_DMC_simple_gpu_V100_1e9 -s1000) &>> PSI_DMC_simple_gpu_V100_log
echo 1e10
(time ./PSI_DMC_simple_gpu_V100.out lambda=2.566 -n 1e10 -dPSI_DMC_simple_gpu_V100_1e10 -s1000) &>> PSI_DMC_simple_gpu_V100_log
echo 1e11
(time ./PSI_DMC_simple_gpu_V100.out lambda=2.566 -n 1e11 -dPSI_DMC_simple_gpu_V100_1e11 -s1000) &>> PSI_DMC_simple_gpu_V100_log
echo 1e12
(time ./PSI_DMC_simple_gpu_V100.out lambda=2.566 -n 1e12 -dPSI_DMC_simple_gpu_V100_1e12 -s1000) &>> PSI_DMC_simple_gpu_V100_log
echo done
