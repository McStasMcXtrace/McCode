set(0, 'defaultTextFontSize',20);
xes=[1e4 1e5 1e6 1e7 1e8 1e9 1e10]';
load gpu.dat
load gpu_float.dat
load cpu.dat
load gcc.dat
load mpi.dat
load multicore.dat

hold on
plot(xes,gpu,'-o')
plot(xes,gpu_float,'-x')
plot(xes,multicore,'-+')
plot(xes(1:6),cpu,'-s')
plot(xes(1:6),gcc,'-d')
plot(xes,mpi,'-*')
  legend('PGCC Quadro T2000 doubles -O0','PGCC Quadro T2000 floats -O0','PGCC multicore -O2, 8 cores','PGCC -O2 cpu serial','gcc -O2 cpu serial','PGCC -O2 mpi, 8 cores')

  xlabel('Problem size / neutrons')
  ylabel('Wall-clock time / s')
  title('Performance in the PSI_source case','interpreter','none')

  figure

  perf_gpu=gcc./gpu(1:6);
  perf_gpu_float=gcc./gpu_float(1:6);
  perf_multicore=gcc./multicore(1:6);
  perf_mpi=gcc./mpi(1:6);
  perf_cpu=gcc./cpu(1:6);
 perf_gcc=gcc./gcc(1:6);
 hold on
plot(xes(1:6),perf_gpu,'-o')
plot(xes(1:6),perf_gpu_float,'-x')
plot(xes(1:6),perf_multicore,'-+')
plot(xes(1:6),perf_cpu,'-s')
plot(xes(1:6),perf_gcc,'-d')
plot(xes(1:6),perf_mpi,'-*')
  xlabel('Problem size / neutrons')
  ylabel('speed relative to gcc serial')
   title('Performance in the PSI_source case','interpreter','none')
  legend('PGCC Quadro T2000 doubles -O0','PGCC Quadro T2000 floats -O0','PGCC multicore -O2, 8 cores','PGCC -O2 cpu serial','gcc -O2 cpu serial','PGCC -O2 mpi, 8 cores')
