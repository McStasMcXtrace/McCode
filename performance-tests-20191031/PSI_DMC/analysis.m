set(0, 'defaultTextFontSize',20);
xes=[1e4 1e5 1e6 1e7 1e8 1e9]';
xes_V100=[1e4 1e5 1e6 1e7 1e8 1e9 1e10]';
load gpu.dat
load V100.dat
load cpu.dat
load gcc.dat
load mpi.dat
load multicore.dat

gcc(7) = 10*gcc(6);
hold on
plot(xes(1:6),gpu,'-o')
plot(xes_V100,V100,'-x')
plot(xes,multicore,'-+')
plot(xes(1:6),cpu,'-s')
plot(xes(1:6),gcc(1:6),'-d')
plot(xes(1:6),mpi,'-*')
  legend('PGCC Quadro T2000 doubles -O0','PGCC V100 doubles -O0','PGCC multicore -O2, 8 cores','PGCC -O2 cpu serial','gcc -O2 cpu serial','PGCC -O2 mpi, 8 cores')

  xlabel('Problem size / neutrons')
  ylabel('Wall-clock time / s')
  title('Performance in the PSI_DMC case','interpreter','none')

  figure

  perf_gpu=gcc(1:6)./gpu;
  perf_V100=gcc./V100;
  perf_multicore=gcc(1:6)./multicore(1:6);
  perf_mpi=gcc(1:6)./mpi;
  perf_cpu=gcc(1:6)./cpu;
 perf_gcc=gcc./gcc;
 hold on
plot(xes(1:6),perf_gpu,'-o')
plot(xes_V100,perf_V100,'->')
plot(xes(1:6),perf_multicore,'-+')
plot(xes(1:6),perf_cpu,'-s')
plot(xes_V100,perf_gcc,'-d')
plot(xes(1:6),perf_mpi,'-*')
  xlabel('Problem size / neutrons')
  ylabel('speed relative to gcc serial')
   title('Performance in the PSI_DMC case','interpreter','none')
  legend('PGCC Quadro T2000 doubles -O0','PGCC V100 doubles -O0','PGCC multicore -O2, 8 cores','PGCC -O2 cpu serial','gcc -O2 cpu serial','PGCC -O2 mpi, 8 cores')
