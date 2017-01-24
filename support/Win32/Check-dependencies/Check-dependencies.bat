@reg query "hkcu\software\Python\ContinuumAnalytics"
@if ERRORLEVEL 1 GOTO NOPYTHON  
@goto :HASPYTHON  

:NOPYTHON  
@  echo Please download and install Anaconda Python 3 - spawning browser
@  timeout 2
@  start https://www.continuum.io/downloads#windows
@  timeout 2
@  exit
@

:HASPYTHON
@  echo Seems you have Anaconda Python, great!
@
@perl -e1 2>NUL
@if ERRORLEVEL 1 GOTO NOPERL 
@goto :HASPERL 

:NOPERL
@  echo Please download and install Strawberry Perl from mcstas.org - spawning browser
@  timeout 2
@  start http://download.mcstas.org/current/windows/
@  timeout 2
@  exit

:HASPERL
@  echo Seems you have a Perl
@timeout 5

