#!/bin/bash
(time mpirun ./templateLaue_mpi.out reflections=C60.lau -n 1e4 -dtemplateLaue_mpi_1e4 -s1000) &> templateLaue_mpi_log
echo 1e5
(time mpirun ./templateLaue_mpi.out reflections=C60.lau -n 1e5 -dtemplateLaue_mpi_1e5 -s1000) &>> templateLaue_mpi_log
echo 1e6
(time mpirun ./templateLaue_mpi.out reflections=C60.lau -n 1e6 -dtemplateLaue_mpi_1e6 -s1000) &>> templateLaue_mpi_log
echo 1e7
(time mpirun ./templateLaue_mpi.out reflections=C60.lau -n 1e7 -dtemplateLaue_mpi_1e7 -s1000) &>> templateLaue_mpi_log
echo 1e8
(time mpirun ./templateLaue_mpi.out reflections=C60.lau -n 1e8 -dtemplateLaue_mpi_1e8 -s1000) &>> templateLaue_mpi_log
echo done
