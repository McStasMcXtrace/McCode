
Tk800.013 has been built by the author using ActivePerl
from ActiveState's APi509e.exe.

You need Visual C++ (Mine is version 6.0 - Professional Edition).

When you install ActivePerl, it provides patched C runtime as PerlCRT.dll
which it installs in the "system32" directory.
This needs "administrator" rights on NT.

It also provides the import library PerlCRT.lib, but this is installed
in an odd location e.g. C:\ActivePerl\lib\CORE\PerlCRT.lib
where it is not found by MakeMaker or VC++.
I copied it to C:\VisualStudio\VC98\lib\PerlCRT.lib
(Your paths may vary dependinh where you installed ActivePerl and VC++.)

Once that is done:

perl Makefile.PL
nmake
nmake test
nmake install_perl

Works as expected.

With prior verions of ActivePerl and Tk it has been necessary with
some versions of VC++ to downgrade "optimization"; from -O2 that
ActivePerl suggests, to -Od.  This does not _seem_ to be required this
time.



