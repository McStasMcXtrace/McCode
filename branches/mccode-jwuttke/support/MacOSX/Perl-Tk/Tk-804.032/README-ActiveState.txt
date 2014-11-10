Building using ActivePerl and gcc (MinGW)

ActivePerl provides a stripped down MinGW ppm package which can be
installed using:

    ppm install MinGW

Unfortunately, some binaries required to build Perl/Tk are missing,
namely: dllwrap, ranlib, and strip (see also
http://community.activestate.com/node/10487). These binaries have to
be fetched from elsewhere and copied to C:\Perl\site\bin, where the
other MinGW binaries reside (gcc, make, ar etc.).

Once done, Perl/Tk can be installed normally using:

    perl Makefile.PL
    dmake
    dmake test
    dmake install

(Tested with ActivePerl 5.16.3 and Tk 804.031_501 using various
Windows versions)

ActivePerl may also automatically detect MinGW which comes with
StrawberryPerl. Only $Config{libpth} seems to be wrong and needs to be
fixed by setting an environment variable:

    set LIBRARY_PATH=C:\strawberry\c\i686-w64-mingw32\lib

After that, Perl/Tk may be built using:

    perl Makefile.PL
    dmake
    dmake test
    dmake install

----------------------------------------------------------------------
Older notes about building using ActivePerl with Visual C++

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

