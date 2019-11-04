#!/bin/bash
(time ./PSI_source_gpu.out lambda=2.566 -n 1e4 -dPSI_source_gpu_1e4_h -s1000) &> PSI_source_gpu_log_h
echo 1e5
(time ./PSI_source_gpu.out lambda=2.566 -n 1e5 -dPSI_source_gpu_1e5_h -s1000) &>> PSI_source_gpu_log_h
echo 1e6
(time ./PSI_source_gpu.out lambda=2.566 -n 1e6 -dPSI_source_gpu_1e6_h -s1000) &>> PSI_source_gpu_log_h
echo 1e7
(time ./PSI_source_gpu.out lambda=2.566 -n 1e7 -dPSI_source_gpu_1e7_h -s1000) &>> PSI_source_gpu_log_h
echo 1e8
(time ./PSI_source_gpu.out lambda=2.566 -n 1e8 -dPSI_source_gpu_1e8_h -s1000) &>> PSI_source_gpu_log_h
echo 1e9
(time ./PSI_source_gpu.out lambda=2.566 -n 1e9 -dPSI_source_gpu_1e9_h -s1000) &>> PSI_source_gpu_log_h
echo 1e10
(time ./PSI_source_gpu.out lambda=2.566 -n 1e10 -dPSI_source_gpu_1e10_h -s1000) &>> PSI_source_gpu_log_h
echo done
