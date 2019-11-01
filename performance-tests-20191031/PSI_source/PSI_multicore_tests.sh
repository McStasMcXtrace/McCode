#!/bin/bash
(time ./PSI_source_multicore.out lambda=2.566 -n 1e4 -dPSI_source_multicore_1e4 -s1000) &> PSI_source_multicore_log
echo 1e5
(time ./PSI_source_multicore.out lambda=2.566 -n 1e5 -dPSI_source_multicore_1e5 -s1000) &>> PSI_source_multicore_log
echo 1e6
(time ./PSI_source_multicore.out lambda=2.566 -n 1e6 -dPSI_source_multicore_1e6 -s1000) &>> PSI_source_multicore_log
echo 1e7
(time ./PSI_source_multicore.out lambda=2.566 -n 1e7 -dPSI_source_multicore_1e7 -s1000) &>> PSI_source_multicore_log
echo 1e8
(time ./PSI_source_multicore.out lambda=2.566 -n 1e8 -dPSI_source_multicore_1e8 -s1000) &>> PSI_source_multicore_log
echo 1e9
(time ./PSI_source_multicore.out lambda=2.566 -n 1e9 -dPSI_source_multicore_1e9 -s1000) &>> PSI_source_multicore_log
echo 1e10
(time ./PSI_source_multicore.out lambda=2.566 -n 1e10 -dPSI_source_multicore_1e10 -s1000) &>> PSI_source_multicore_log
echo done
