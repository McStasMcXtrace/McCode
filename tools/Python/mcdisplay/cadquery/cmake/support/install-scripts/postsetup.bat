@REM Script for final installation of McStas/McXtrace-related Perl modules
@REM
@REM If conda dependencies should be reintroduced here, what worked for McStas 2.4 is
@REM conda python=3.4 pyqt=4.10.4 pyqtgraph pyaml ply numpy -y
@REM conda install -c cogsci qscintilla2=2.9.3
@REM  -> is what is present in http://support.mccode.org/windows/miniconda3.zip
@REM
@if exist c:\\strawberry\\perl\\bin
  @set PATH=c:\\strawberry\\perl\\bin;PATH=c:\\strawberry\\c\\bin;%PATH%
  @echo Installing Perl modules PDL, PGPLOT,Tk
  @call c:\\strawberry\\perl\\bin\\ppm.bat install PDL PGPLOT Tk
  @echo Building Tk-CodeText locally
  @unzip Tk-CodeText-0.3.4.zip
  @cd Tk-CodeText-0.3.4
  @perl Makefile.PL
  @gmake
  @gmake install
  @echo Done installing all Perl modules!
  pause
@else
  @echo No Strawberry Perl installation on system, exiting

