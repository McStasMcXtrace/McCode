@REM Script for final installation of McStas/McXtrace-related Perl modules
@REM
@REM If conda dependencies should be reintroduced here, what worked for McStas 2.4 is
@REM conda python=3.4 pyqt=4.10.4 pyqtgraph pyaml ply numpy -y
@REM conda install -c cogsci qscintilla2=2.9.3
@REM  -> is what is present in http://support.mccode.org/windows/miniconda3.zip
@REM
@set PATH=c:\\strawberry\\perl\\bin;%PATH%
@call c:\\strawberry\\perl\\bin\\ppm.bat install http://ppds.mccode.org/Astro-FITS-Header.ppd
@call c:\\strawberry\\perl\\bin\\ppm.bat install http://ppds.mccode.org/Convert-UU.ppd
@call c:\\strawberry\\perl\\bin\\ppm.bat install http://ppds.mccode.org/Tk.ppd
@call c:\\strawberry\\perl\\bin\\ppm.bat install http://ppds.mccode.org/Tk-CodeText.ppd
@call c:\\strawberry\\perl\\bin\\ppm.bat install http://ppds.mccode.org/Math-Amoeba.ppd
@call c:\\strawberry\\perl\\bin\\ppm.bat install http://ppds.mccode.org/OpenGL.ppd
@call c:\\strawberry\\perl\\bin\\ppm.bat install http://ppds.mccode.org/PDL.ppd
@call c:\\strawberry\\perl\\bin\\ppm.bat install http://ppds.mccode.org/PGPLOT.ppd
