******************************************
README and install instructions for 
McStas and McXtrace on Mac OS X.

OS X versions 11.0 and 10.9-10.15 fully supported,
earlier ones only limited support...
******************************************

Contents of this folder:
1) README.TXT: This file - please read fully!

2) McCode application bundle named McStas-version.app or McXtrace-version.app

3) A link to /Applications - please drag both of the below items there to install!
   - The McCode app
   - The "environment script" (suffix .command)

4) Run the Check-PY-dependencies.command script to check for / install Python tool dependencies
   !! MANDATORY to run before attempting to run the .app that you moved to /Applications !!
   - Will check if Xcode is installed, otherwise help you install it (THIS MAY FAIL on old versions
     e.g. Mountain Lion and older  - please manually install Xcode and Xcode commandline tools on
     such systems!!)
   - Will ask you to take ownership of /usr/local
   - Will ask you to configure the firewall to allow simple usage of openmpi

5) Optionally run the Check-PL-dependencies.command script to check for / install Perl tool dependencies
   !! OPTIONAL only - we encourage you to use the Python variants instead !!
   - Will check if Xcode is installed, otherwise help you install it
   - Will help you install needed Perl modules from the McCode GitHub page
   - Will allow you to reconfigure the .app bundle to run the legacy Perl tool set
   - Will ask you to take ownership of /usr/local
   - Will ask you to configure the firewall to allow simple usage of openmpi

6) If you need support for NeXus output, we propose to install this via homebrew (brew install nexusformat).
   To allow compilation with the NeXus libraries, please redefine your CFLAGS through the mcgui/mxgui preferences:
   -g -lm -O2 -DUSE_NEXUS -lNeXus -I/usr/local/include/nexus

7) Now open either the newly installed app or environment script

Once the app and dependencies have been installed on your harddrive, optionally use the gui to make your McCode
the default mcstas/mcxtrace on your machine:
    * mcgui    (python) - Use "File -> Set as default"
    * mcgui.pl (perl)   - Use "Tools -> Set this McCode as sys default"
    * Furhter menu points in the same place allows to configure the app to run the perl or python UI

In case of issues installing / using this app bundle, please contact either
   mcstas-users@mcstas.org
   mcxtrace-users@mcxtrace.org





