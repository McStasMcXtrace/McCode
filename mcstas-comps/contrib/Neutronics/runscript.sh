rm ptrak
rm wssa 
gfortran -c neutronics-subs.f
mcstas -t test_SSR_SSW.instr &&  
gcc -o runme.out test_SSR_SSW.c neutronics-subs.o -lm -lgfortran --trace &&  ./runme.out
