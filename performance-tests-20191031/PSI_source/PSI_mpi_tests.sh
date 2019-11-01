#!/bin/bash
(time mpirun ./PSI_source_mpi.out lambda=2.566 -n 1e4 -dPSI_source_mpi_1e4 -s1000) &> PSI_source_mpi_log
echo 1e5
(time mpirun ./PSI_source_mpi.out lambda=2.566 -n 1e5 -dPSI_source_mpi_1e5 -s1000) &>> PSI_source_mpi_log
echo 1e6
(time mpirun ./PSI_source_mpi.out lambda=2.566 -n 1e6 -dPSI_source_mpi_1e6 -s1000) &>> PSI_source_mpi_log
echo 1e7
(time mpirun ./PSI_source_mpi.out lambda=2.566 -n 1e7 -dPSI_source_mpi_1e7 -s1000) &>> PSI_source_mpi_log
echo 1e8
(time mpirun ./PSI_source_mpi.out lambda=2.566 -n 1e8 -dPSI_source_mpi_1e8 -s1000) &>> PSI_source_mpi_log
echo 1e9
(time mpirun ./PSI_source_mpi.out lambda=2.566 -n 1e9 -dPSI_source_mpi_1e9 -s1000) &>> PSI_source_mpi_log
echo 1e10
(time mpirun ./PSI_source_mpi.out lambda=2.566 -n 1e10 -dPSI_source_mpi_1e10 -s1000) &>> PSI_source_mpi_log
echo done
