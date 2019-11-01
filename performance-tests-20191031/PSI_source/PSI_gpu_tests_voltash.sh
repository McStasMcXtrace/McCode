#!/bin/bash
(time ./PSI_source_gpu.out lambda=2.566 -n 1e4 -dPSI_source_gpu_1e4_volta -s1000) &> PSI_source_gpu_log_volta
echo 1e5
(time ./PSI_source_gpu.out lambda=2.566 -n 1e5 -dPSI_source_gpu_1e5_volta -s1000) &>> PSI_source_gpu_log_volta
echo 1e6
(time ./PSI_source_gpu.out lambda=2.566 -n 1e6 -dPSI_source_gpu_1e6_volta -s1000) &>> PSI_source_gpu_log_volta
echo 1e7
(time ./PSI_source_gpu.out lambda=2.566 -n 1e7 -dPSI_source_gpu_1e7_volta -s1000) &>> PSI_source_gpu_log_volta
echo 1e8
(time ./PSI_source_gpu.out lambda=2.566 -n 1e8 -dPSI_source_gpu_1e8_volta -s1000) &>> PSI_source_gpu_log_volta
echo 1e9
(time ./PSI_source_gpu.out lambda=2.566 -n 1e9 -dPSI_source_gpu_1e9_volta -s1000) &>> PSI_source_gpu_log_volta
echo 1e10
(time ./PSI_source_gpu.out lambda=2.566 -n 1e10 -dPSI_source_gpu_1e10_volta -s1000) &>> PSI_source_gpu_log_volta
echo 1e11
(time ./PSI_source_gpu.out lambda=2.566 -n 1e11 -dPSI_source_gpu_1e11_volta -s1000) &>> PSI_source_gpu_log_volta
echo 1e12
(time ./PSI_source_gpu.out lambda=2.566 -n 1e12 -dPSI_source_gpu_1e12_volta -s1000) &>> PSI_source_gpu_log_volta
echo done
