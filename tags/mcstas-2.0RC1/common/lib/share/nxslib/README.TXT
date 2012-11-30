NOTE: We ought to have a proper Makefile here!!

NXS Crystallography library provided by Mirko Boin, HZB.

To build the shared library:

gcc -fPIC -c *.c 
gcc -shared -Wl -o libnxs.so *.o

Then use

MCSTAS_CFLAGS="-g -O2 -L/install/path -I/install/path-lm -lnxs"
LD_LIBRARY_PATH=/install/path

when compiling your instrument that includes Sample_nxs.comp
