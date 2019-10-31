#!/bin/bash
(time ./mini_gpu.out -n 1e4 -dmini_gpu_1e4 -s1000) &> mini_gpu_log
echo 1e5
(time ./mini_gpu.out -n 1e5 -dmini_gpu_1e5 -s1000) &>> mini_gpu_log
echo 1e6
(time ./mini_gpu.out -n 1e6 -dmini_gpu_1e6 -s1000) &>> mini_gpu_log
echo 1e7
(time ./mini_gpu.out -n 1e7 -dmini_gpu_1e7 -s1000) &>> mini_gpu_log
echo 1e8
(time ./mini_gpu.out -n 1e8 -dmini_gpu_1e8 -s1000) &>> mini_gpu_log
echo 1e9
(time ./mini_gpu.out -n 1e9 -dmini_gpu_1e9 -s1000) &>> mini_gpu_log
echo 1e10
(time ./mini_gpu.out -n 1e10 -dmini_gpu_1e10 -s1000) &>> mini_gpu_log
echo done
