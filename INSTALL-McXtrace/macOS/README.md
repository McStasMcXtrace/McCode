# Installation of McXtrace 1.7 on macOS 

## Supported macOS releases
* macOS 11.0 (Big Sur, Fully supported python tool set, partially
  supported perl tool set)

* macOS 10.9-10.15 (Maverics, Yosemite, El Capitan, Sierra, High
  Sierra, Mojave, Catalina) Fully supported, both perl and python tool sets

* Mac OS X 10.8 (Partially supported, perl tool set works and python might...)

## Steps to perform

* Download the relevant verison (arm64 for M1/Silicon processor,
x86_64 for Intel processor) from 
[McXtrace 1.7 for macOS](https://download.mcxtrace.org/mcxtrace-1.7/mac/)
  and unpack it (e.g. double-clicking should work)

* Open the resulting folder and

* Drag the McXtrace-1.7.app and the McXtrace-1.7-environment.command to
/Applications

* Run the Check-PY-dependencies.command script to check for / install
  basic compiler support and  Python tool dependencies (right-click and "open"). Please follow
  on-screen instructions. (If you are on an M1 machine you will be
  prompted to install 'homebrew' during the process.)

* Optionally run the Check-PL-dependencies.command script to check for
  / install basic compiler support  Perl tool dependencies (right-click and "open"). Please follow
  on-screen instructions.

* Once the app and dependencies have been installed on your harddrive, optionally use the gui to make your McCode
the default mcstas/mcxtrace on your machine:
 * mcgui    (python) - Use "File -> Set as default"

* In case of issues installing / using this app bundle, please contact mcxtrace-users@mcxtrace.org
