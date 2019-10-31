set(0, 'defaultTextFontSize',20);
xes=[1e4 1e5 1e6 1e7 1e8]';
load gpu.dat
load cpu.dat
load gcc.dat
load mpi.dat
load multicore.dat

hold on
plot(xes(1:3),gpu,'-o')
plot(xes,multicore,'-+')
plot(xes,cpu,'-s')
plot(xes,gcc,'-d')
plot(xes,mpi,'-*')
  legend('PGCC Quadro T2000 doubles -O0','PGCC multicore -O2, 8 cores','PGCC -O2 cpu serial','gcc -O2 cpu serial','PGCC -O2 mpi, 8 cores')

  xlabel('Problem size / neutrons')
  ylabel('Wall-clock time / s')
  title('Performance in the Laue/C60 case','interpreter','none')

  figure

  perf_gpu=gcc(1:3)./gpu;
  perf_multicore=gcc./multicore;
  perf_mpi=gcc./mpi;
  perf_cpu=gcc./cpu;
 perf_gcc=gcc./gcc;
 hold on
 plot(xes(1:3),perf_gpu,'-o')
plot(xes,perf_multicore,'-+')
plot(xes,perf_cpu,'-s')
plot(xes,perf_gcc,'-d')
plot(xes,perf_mpi,'-*')
  xlabel('Problem size / neutrons')
  ylabel('speed relative to gcc serial')
   title('Performance in the Laue/C60 case','interpreter','none')
  legend('PGCC Quadro T2000 doubles -O0','PGCC multicore -O2, 8 cores','PGCC -O2 cpu serial','gcc -O2 cpu serial','PGCC -O2 mpi, 8 cores')
