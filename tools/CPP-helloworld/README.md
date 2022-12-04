Proof of concept solution for calling a C++ library from McStas/McXtrace:

1) The instrument Test_CppWrap.instr uses a component called CppWrap.comp, and passes a string 
   input variable to that comp

2) CppWrap.comp has a DEPENDENCY "-L. -lhellowrap"

3) The library hellowrap.cpp is a combined c/c++ wrapper depending on an 'inner' c++ function 
   hello() from the library hello.cpp/hello.h. The wrapper defines the function sayHi() which
   is called from the component code.

4) hello.cpp has a single function hello() that prints the input string via cout 

5) Compilation steps:

   a) Compile libhello.so via
      g++ -fpic -shared hello.cpp -o libhello.so
   
   b) Compile libhellowrap.so via
      g++ -fpic -shared hellowrap.cpp -L. -lhello -o libhellowrap.so
      
   c) You may need to add . to your linker path
      export LD_LIBRARY_PATH=.

   d) Compile the instrument via (e.g.)
      mcrun.pl -c Test_CppWrap.instr


Output:

mcrun.pl -c Test_CppWrap.instr
/usr/local/bin/mcrun.pl: reading local mcstas configuration from /Users/pkwi/.mcstas/mccode_config.perl
*** No directory given - placing data in Test_CppWrap_20171120_113035 ***
Translating instrument definition 'Test_CppWrap.instr' into C ...
mcstas -t -o Test_CppWrap.c Test_CppWrap.instr
Warning: Component Wrap=CppWrap(string something) definition parameter
         may be changed into a setting parameter to avoid
         warnings at compile time.
CFLAGS= -L. -lhellowrap
Compiling C source 'Test_CppWrap.c' ...
gcc -o Test_CppWrap.out Test_CppWrap.c -lm -g -O2 -lm -L. -lhellowrap
Running simulation 'Test_CppWrap.out' ...
Test_CppWrap.out --dir=Test_CppWrap_20171120_113035 --format=PGPLOT --ncount=1000000
Instrument parameters for Test_CppWrap (Test_CppWrap.instr)
Set value of instrument parameter saythis (string) [default='HelloWorld']:
John was here
[Test_CppWrap] Initialize
output: John was here

Save [Test_CppWrap]

Finally [Test_CppWrap: Test_CppWrap_20171120_113035]. Time: 0 [s] 
