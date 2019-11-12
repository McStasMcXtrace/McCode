set(0, 'defaultTextFontSize',20);
xes=[1e4 1e5 1e6 1e7 1e8 1e9 1e10]';
xes_volta=[1e4 1e5 1e6 1e7 1e8 1e9 1e10 1e11 1e12]';
load gpu.dat
load gpu_hypatia.dat
load gpu_float.dat
load gpu_float_hypatia.dat
load volta_v100.dat
load cpu.dat
load gcc.dat
load mpi.dat
load multicore.dat

gcc(7)=10*gcc(6);
cpu(7)=10*cpu(6);
gcc(8)=100*gcc(6);
cpu(8)=100*cpu(6);
gcc(9)=1000*gcc(6);
cpu(9)=1000*cpu(6);

hold on
plot(xes,gpu,'-o')
plot(xes,gpu_hypatia,'-p')
plot(xes,gpu_float,'-x')
plot(xes,gpu_float_hypatia,'-h')
plot(xes_volta,volta_v100,'-^')
plot(xes,multicore,'-+')
plot(xes(1:6),cpu(1:6),'-s')
plot(xes(1:6),gcc(1:6),'-d')
plot(xes,mpi,'-*')
plot(xes(7),gcc(7),'->')
plot(xes(7),cpu(7),'-<')

 legend('PGCC Quadro T2000 doubles -O0','PGCC GeForce GTX 1080 -O0','PGCC Quadro T2000 floats -O0','PGCC GeForce GTX 1080 floats -O0','PGCC Volta V100 -O0','PGCC multicore -O2, 8 cores','PGCC -O2 cpu serial','gcc -O2 cpu serial','PGCC -O2 mpi, 8 cores','extrapolation gcc cpu ','extrapolation,PGCC cpu')

  xlabel('Problem size / neutrons')
  ylabel('Wall-clock time / s')
  title('Performance in the PSI_source case - execution time','interpreter','none')
  set(gca,'xscale','log','yscale','log')
  
  figure

  perf_gpu=gcc(1:7)./gpu;
  perf_gpu_hypatia=gcc(1:7)./gpu_hypatia;
  perf_gpu_float=gcc(1:7)./gpu_float;
  perf_gpu_float_hypatia=gcc(1:7)./gpu_float_hypatia;
  perf_volta_v100=gcc./volta_v100;
  perf_multicore=gcc(1:7)./multicore;
  perf_mpi=gcc(1:7)./mpi;
  perf_cpu=gcc(1:7)./cpu(1:7);
 perf_gcc=gcc(1:7)./gcc(1:7);
 hold on
plot(xes,perf_gpu,'-o')
plot(xes,perf_gpu_hypatia,'-p')
plot(xes,perf_gpu_float,'-x')
plot(xes,perf_gpu_float_hypatia,'-h')
plot(xes_volta,perf_volta_v100,'-^')
plot(xes,perf_multicore,'-+')
plot(xes,perf_cpu,'-s')
plot(xes,perf_gcc,'-d')
plot(xes,perf_mpi,'-*')
  xlabel('Problem size / neutrons')
  ylabel('speed relative to gcc serial')
   title('Performance in the PSI_source case - relative speedup','interpreter','none')
  legend('PGCC Quadro T2000 doubles -O0','PGCC GeForce GTX 1080 -O0','PGCC Quadro T2000 floats -O0','PGCC GeForce GTX 1080 floats -O0','PGCC Volta V100 -O0','PGCC multicore -O2, 8 cores','PGCC -O2 cpu serial','gcc -O2 cpu serial','PGCC -O2 mpi, 8 cores')
  set(gca,'xscale','log','yscale','log')