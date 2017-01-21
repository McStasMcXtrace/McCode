******************************************
README for McStas and McXtrace on Mac OS X
******************************************

Contents of this folder:
1) README.TXT: This file

2) McCode application bundle named McStas-version.app or McXtrace-version.app

3) Check-PY-dependencies.command: Script to check for / install Python tool dependencies
   !! MANDATORY to run before attempting to run the .app !!
   - Will check if Xcode is installed, otherwise help you install it
   - Will check if Anaconda Python 3 is installed, otherwise help you install it
   - Will make sure needed Python modules are installed
   - Will ask you to take ownership of /usr/local

4) Check-PL-dependencies.command: Script to check for / install Perl tool dependencies
   !! Optional - we encourage you to use the Python variants instead !!
   - Will check if Xcode is installed, otherwise help you install it
   - Will help you install needed Perl modules from the McCode GitHub page
   - Will allow you to reconfigure the .app bundle to run the legacy Perl tool set
   - Will ask you to take ownership of /usr/local

5) A link to /Applications - after running the relevant dependency script(s) above you
   may use it to install McCode in the default location (drag the .app)

6) Optional script Install-extras.command:
   - Helps in downloading/installing gcc from hpc.sourceforge.net (installed in /usr/local)
   - Helps in downloading/building openmpi (c-bindings only, installed in /usr/local)

Once the app has been installed on your harddrive, optionally use the gui to make your McCode
the default mcstas/mcxtrace on your machine:
    * mcgui    (python) - Use "File -> Set as default"
    * mcgui.pl (perl)   - Use "Tools -> Set this McCode as sys default"
    * Furhter menu points in the same place allows to configure the app to run the perl or python UI

In case of issues installing / using this app bundle, please contact either
   mcstas-users@mcstas.org
   mcxtrace-users@mcxtrace.org





