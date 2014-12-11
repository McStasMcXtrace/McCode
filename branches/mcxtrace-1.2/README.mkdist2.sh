* Notes for building packages on Debian/Ubuntu:



* Notes for building packages on Mac OS X:

Prerequisites:
- Xcode including commandline tools
- Auxiullary Tools for Xcode (developer.apple.com)
- cmake

NB: Builds seems to be incompatible between Mountain Lion and Lion - build independently for these platforms!

mcstas mcstas-tools and mcstas-comps:
In trunk, issue these commands:
./mkdist2.sh mctools 2.0beta tools/Legacy-Perl/ -- OSXpkg
./mkdist2.sh mcstas 2.0beta "" -- OSXpkg
./mkdist2.sh mcstas-comps 2.0beta "" -- OSXpkg

find dist -name \*2.0beta\*.pkg -exec cp -rp \{\} dist \;

Perl-Tk:
cd support/MacOSX/Perl-Tk/Tk-804.030
./Build.sh
cd -
cp -rp support/MacOSX/Perl-Tk/build/*.pkg dist

SciPDL:
cd dist
wget http://sourceforge.net/projects/pdl/files/PDL/2.4.10/SciPDL-v2.4.10-Lion.pkg.zip
open SciPDL-v2.4.10-Lion.pkg.zip

Now use PackageMaker (sorry, GUI only) and generate a 'meta' package from the generated pkg's in dist... 
 !! Consider adding .app bundle for /Applications
 !! We ought to include proper README, license etc. info in the individual/Meta package...


* Notes for building packages on Windows 32/64 bit:
