#!/bin/bash
(time ./BNL_H8_simple_gpu_float.out lambda=2.36 -n 1e4 -dBNL_H8_simple_gpu_float_1e4 ) &> BNL_H8_simple_gpu_float_log
echo 1e5
(time ./BNL_H8_simple_gpu_float.out lambda=2.36 -n 1e5 -dBNL_H8_simple_gpu_float_1e5 ) &>> BNL_H8_simple_gpu_float_log
echo 1e6
(time ./BNL_H8_simple_gpu_float.out lambda=2.36 -n 1e6 -dBNL_H8_simple_gpu_float_1e6 ) &>> BNL_H8_simple_gpu_float_log
echo 1e7
(time ./BNL_H8_simple_gpu_float.out lambda=2.36 -n 1e7 -dBNL_H8_simple_gpu_float_1e7 ) &>> BNL_H8_simple_gpu_float_log
echo 1e8
(time ./BNL_H8_simple_gpu_float.out lambda=2.36 -n 1e8 -dBNL_H8_simple_gpu_float_1e8 ) &>> BNL_H8_simple_gpu_float_log
echo 1e9
(time ./BNL_H8_simple_gpu_float.out lambda=2.36 -n 1e9 -dBNL_H8_simple_gpu_float_1e9 ) &>> BNL_H8_simple_gpu_float_log
echo done
