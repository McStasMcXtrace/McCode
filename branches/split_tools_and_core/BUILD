These are build/install instructions for McStas / McXtrace + tool layer packages

(* Please note that distribution packages are currently built using the mkdist script - the config/install method at hand is currently "for developers only" *)

a) A McStas or McXtrace core package

b) Related 'tools', i.e. automation scripts, GUI, plotting implemented in either
   b.1) Perl + Perl::Tk + ((pgplot + pgperl + PDL) || (Matlab) || Scilab || Gnuplot)) (mccode-legacy-tools)
   b.2) Python + ... + ... (mccode-tools)

0) Start by configuring for a McStas or McXtrace - centered build, either

   0.a)   ./configure --enable-mcstas
   0.b)	  ./configure --enable-mcxtrace

   (This will configure both the mccode-core and mccode-*tools portions to build for either McStas or McXtrace)

1) Build the core package:
   cd mccode-core
   make && make install
   cd ..

2) Build the legacy-tools:
   cd mccode-legacy-tools
   make && make install

3) Nothing so far for the new tool layer...

IMPORTANT: To build the "other" package, please follow these steps:
  make distclean
  ./configure --enable-theotherpackage
   
   