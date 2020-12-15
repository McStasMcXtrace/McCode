# Installation of McStas 3.0 on macOS 

## Supported macOS releases
* macOS 11.0 (Big Sur, Fully supported python tool set, partially
  supported perl tool set)

* macOS 10.9-10.15 (Maverics, Yosemite, El Capitan, Sierra, High
  Sierra, Mojave, Catalina) Fully supported, both perl and python tool sets

* Mac OS X 10.8 (Partially supported, perl tool set works and python might...)

## Steps to perform

* Download
  [McStas 3.0 for macOS](http://download.mcstas.org/current/mac/mcstas-3.0.tgz)
  and unpack it (e.g. double-clicking should work)

* Open the resulting folder

* Drag the McStas-3.0.app and the McStas-3.0-environment.command to
/Applications

* Run the Check-PY-dependencies.command script to check for / install
  basic compiler support and  Python tool dependencies (right-click and "open"). Please follow
  on-screen instructions.

* Optionally run the Check-PL-dependencies.command script to check for
  / install basic compiler support  Perl tool dependencies (right-click and "open"). Please follow
  on-screen instructions.

* Once the app and dependencies have been installed on your harddrive, optionally use the gui to make your McCode
the default mcstas/mcxtrace on your machine:
 * mcgui    (python) - Use "File -> Set as default"
 * mcgui.pl (perl)   - Use "Tools -> Set this McCode as sys default"
 * Furhter menu points in the same place allows to configure the app to run the perl or python UI

* If you need support for NeXus output, please follow the instructions
  at https://github.com/McStasMcXtrace/McCode/wiki/McStas-and-Mantid#install-nexus

* In case of issues installing / using this app bundle, please contact mcstas-users@mcstas.org
