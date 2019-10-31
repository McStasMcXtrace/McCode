#!/bin/bash
(time mpirun ./mini_mpi.out -n 1e4 -dmini_mpi_1e4 -s1000) &> mini_mpi_log
echo 1e5
(time mpirun ./mini_mpi.out -n 1e5 -dmini_mpi_1e5 -s1000) &>> mini_mpi_log
echo 1e6
(time mpirun ./mini_mpi.out -n 1e6 -dmini_mpi_1e6 -s1000) &>> mini_mpi_log
echo 1e7
(time mpirun ./mini_mpi.out -n 1e7 -dmini_mpi_1e7 -s1000) &>> mini_mpi_log
echo 1e8
(time mpirun ./mini_mpi.out -n 1e8 -dmini_mpi_1e8 -s1000) &>> mini_mpi_log
echo 1e9
(time mpirun ./mini_mpi.out -n 1e9 -dmini_mpi_1e9 -s1000) &>> mini_mpi_log
echo 1e10
(time mpirun ./mini_mpi.out -n 1e10 -dmini_mpi_1e10 -s1000) &>> mini_mpi_log
echo done
