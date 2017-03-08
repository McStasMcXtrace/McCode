******************************************
README and install instructions for 
McStas and McXtrace on Mac OS X
******************************************

Contents of this folder:
1) README.TXT: This file - please read fully!

2) McCode application bundle named McStas-version.app or McXtrace-version.app

3) A link to /Applications - dragg the McCode app there to install!

4) Run the Check-PY-dependencies.command script to check for / install Python tool dependencies
   !! MANDATORY to run before attempting to run the .app that you moved to /Applications !!
   - Will check if Xcode is installed, otherwise help you install it
   - Will ask you to take ownership of /usr/local
   - Will ask you to configure the firewall to allow simple usage of openmpi

4) Optionally run the Check-PL-dependencies.command script to check for / install Perl tool dependencies
   !! OPTIONAL only - we encourage you to use the Python variants instead !!
   - Will check if Xcode is installed, otherwise help you install it
   - Will help you install needed Perl modules from the McCode GitHub page
   - Will allow you to reconfigure the .app bundle to run the legacy Perl tool set
   - Will ask you to take ownership of /usr/local
   - Will ask you to configure the firewall to allow simple usage of openmpi

6) A link to an "environment script" (suffix .command) for placement in /Applications. Runs
   the version at hand in a terminal (for easy access to terminal commands)

Once the app and dependencies have been installed on your harddrive, optionally use the gui to make your McCode
the default mcstas/mcxtrace on your machine:
    * mcgui    (python) - Use "File -> Set as default"
    * mcgui.pl (perl)   - Use "Tools -> Set this McCode as sys default"
    * Furhter menu points in the same place allows to configure the app to run the perl or python UI

In case of issues installing / using this app bundle, please contact either
   mcstas-users@mcstas.org
   mcxtrace-users@mcxtrace.org





