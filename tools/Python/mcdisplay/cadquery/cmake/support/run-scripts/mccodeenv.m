% Matlab scriptfile for setting up iFit for use with @CPACK_PACKAGE_NAME@
%
% First, set up various environment variables
%
% Path related:
setenv('PATH', ['@CPACK_NSIS_INSTALL_ROOT@\\bin;@CPACK_NSIS_INSTALL_ROOT@\\miniconda3;@CPACK_NSIS_INSTALL_ROOT@\\miniconda3\\Scripts\\;@CPACK_NSIS_INSTALL_ROOT@\\miniconda3\\Library\\bin;@CPACK_NSIS_INSTALL_ROOT@\\miniconda3\\Library\\mingw-w64\\bin;c:\\strawberry\\perl\\bin;c:\\Microsoft MPI\\Bin;' getenv('PATH')]);
%
% McStas related:
setenv('@FLAVOR_UPPER@', '@CPACK_NSIS_INSTALL_ROOT@\\lib');
setenv('@FLAVOR_UPPER@_TOOLS', '@CPACK_NSIS_INSTALL_ROOT@\\lib\\tools\\Perl\\');
setenv('@FLAVOR_UPPER@_CC', 'gcc');
setenv('@FLAVOR_UPPER@_FORMAT', '@PLOTTER@');
%
% PGPLOT configuration
setenv('PGPLOT_DIR', 'c:\\strawberry\\perl\\site\\lib\\PGPLOT\\pgplot_supp');
setenv('PGPLOT_FONT', 'c:\\strawberry\\perl\\site\\lib\\PGPLOT\\pgplot_supp\\grfont.dat');
setenv('PGPLOT_DEV', '/gw');
% 
% Issue a last config help:
display('********************************************************************************');
display(['Your Matlab/iFit should now be aware of your @FLAVOR@ ' ...
         'installation in @CPACK_NSIS_INSTALL_ROOT@.']);
display(['For safety, please also set options.mcrun="@CPACK_NSIS_INSTALL_ROOT@\\bin\\@MCCODE_PREFIX@run in any iFit mcstas() ' ...
         'run, as explained in http://ifit.mccode.org/McStas.html'])
display('********************************************************************************');