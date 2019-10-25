#!/bin/bash

# Processor info
cat /proc/cpuinfo >  cpu.txt
nvidia-smi > gpu.txt


# Generate the binaries

# 1 standard serial gcc-compilation, Mersenne Twister
(time gcc BNL_H8_simple.c -DRNG_ALG=1 -lm -o BNL_H8_simple_gcc_mt.out) &> compile_gcc_mt.txt

# 2 standard serial gcc-compilation, KISS
(time gcc BNL_H8_simple.c -DRNG_ALG=2 -lm -o BNL_H8_simple_gcc_KISS.out) &> compile_gcc_KISS.txt

# 3 MPICC with Mersenne Twister
(time mpicc BNL_H8_simple.c -DRNG_ALG=1 -lm -DUSE_MPI -lmpi -o BNL_H8_simple_mpicc_mt.out) &> compile_mpicc_mt.txt

# 4 MPICC with KISS
(time mpicc BNL_H8_simple.c -DRNG_ALG=2 -lm -DUSE_MPI -lmpi -o BNL_H8_simple_mpicc_KISS.out) &> compile_mpicc_KISS.txt

# 5 pgcc and -ta:multicore
(time pgcc -ta=multicore -Minfo=accel BNL_H8_simple.c -DRNG_ALG=2 -DUSE_PGI -o BNL_H8_simple_multicore_KISS.out) &> compile_multicore_KISS.txt

# 5 pgcc and -ta=tesla...
(time pgcc -ta=tesla:cc70,managed,deepcopy -Minfo=accel -DRNG_ALG=2 -DUSE_PGI -DNOSIGNALS BNL_H8_simple.c -o BNL_H8_simple_GPU_KISS.out) &> compile_GPU_KISS.txt

# Generate simulation datasets - 1e6
echo tesla...
(time ./BNL_H8_simple_GPU_KISS.out -n1e6 lambda=2.36 -d BNL_H8_simple_GPU_KISS_1e6_dir) &> run_GPU_KISS_1e6.txt
echo single core runs...
(time ./BNL_H8_simple_gcc_mt.out -n1e6 lambda=2.36 -d BNL_H8_simple_gcc_mt_1e6_dir) &> run_gcc_mt_1e6.txt
(time ./BNL_H8_simple_gcc_KISS.out -n1e6 lambda=2.36 -d BNL_H8_simple_gcc_KISS_1e6_dir) &> run_gcc_KISS_1e6.txt
echo mpi runs...
(time mpirun ./BNL_H8_simple_mpicc_mt.out -n1e6 lambda=2.36 -d BNL_H8_simple_mpicc_mt_1e6_dir) &> run_mpicc_mt_1e6.txt
(time mpirun ./BNL_H8_simple_mpicc_KISS.out -n1e6 lambda=2.36 -d BNL_H8_simple_mpicc_KISS_1e6_dir) &> run_mpicc_KISS_1e6.txt
echo multicore...
(time ./BNL_H8_simple_multicore_KISS.out -n1e6 lambda=2.36 -d BNL_H8_simple_multicore_KISS_1e6_dir) &> run_multicore_KISS_1e6.txt

echo tesla...
(time ./BNL_H8_simple_GPU_KISS.out -n1e7 lambda=2.36 -d BNL_H8_simple_GPU_KISS_1e7_dir) &> run_GPU_KISS_1e7.txt
echo single core runs...
(time ./BNL_H8_simple_gcc_mt.out -n1e7 lambda=2.36 -d BNL_H8_simple_gcc_mt_1e7_dir) &> run_gcc_mt_1e7.txt
(time ./BNL_H8_simple_gcc_KISS.out -n1e7 lambda=2.36 -d BNL_H8_simple_gcc_KISS_1e7_dir) &> run_gcc_KISS_1e7.txt
echo mpi runs...
(time mpirun ./BNL_H8_simple_mpicc_mt.out -n1e7 lambda=2.36 -d BNL_H8_simple_mpicc_mt_1e7_dir) &> run_mpicc_mt_1e7.txt
(time mpirun ./BNL_H8_simple_mpicc_KISS.out -n1e7 lambda=2.36 -d BNL_H8_simple_mpicc_KISS_1e7_dir) &> run_mpicc_KISS_1e7.txt
echo multicore...
(time ./BNL_H8_simple_multicore_KISS.out -n1e7 lambda=2.36 -d BNL_H8_simple_multicore_KISS_1e7_dir) &> run_multicore_KISS_1e7.txt

echo tesla...
(time ./BNL_H8_simple_GPU_KISS.out -n1e8 lambda=2.36 -d BNL_H8_simple_GPU_KISS_1e8_dir) &> run_GPU_KISS_1e8.txt
echo single core runs...
(time ./BNL_H8_simple_gcc_mt.out -n1e8 lambda=2.36 -d BNL_H8_simple_gcc_mt_1e8_dir) &> run_gcc_mt_1e8.txt
(time ./BNL_H8_simple_gcc_KISS.out -n1e8 lambda=2.36 -d BNL_H8_simple_gcc_KISS_1e8_dir) &> run_gcc_KISS_1e8.txt
echo mpi runs...
(time mpirun ./BNL_H8_simple_mpicc_mt.out -n1e8 lambda=2.36 -d BNL_H8_simple_mpicc_mt_1e8_dir) &> run_mpicc_mt_1e8.txt
(time mpirun ./BNL_H8_simple_mpicc_KISS.out -n1e8 lambda=2.36 -d BNL_H8_simple_mpicc_KISS_1e8_dir) &> run_mpicc_KISS_1e8.txt
echo multicore...
(time ./BNL_H8_simple_multicore_KISS.out -n1e8 lambda=2.36 -d BNL_H8_simple_multicore_KISS_1e8_dir) &> run_multicore_KISS_1e8.txt
