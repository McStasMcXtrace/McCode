# Installation of McXtrace 1.7.1 on macOS 

## Supported macOS releases
* macOS 11-13 (Big Sur and later), Fully supported python tool set, partially
  supported perl tool set. Supported on both Intel and Apple Silicon,
  via separate installation bundles.

## Steps to perform

* Download the relevant verison (arm64 for M-series / Silicon processor,
x86_64 for Intel processor) from 
[McXtrace 1.7.1 for macOS](https://download.mcxtrace.org/mcxtrace-1.7.1/mac/)
  and unpack it (e.g. double-clicking should work)

* Open the resulting folder and

* Drag the McXtrace-1.7.1.app and the McXtrace-1.7.1-environment.command to
/Applications

* Run the Check-PY-dependencies.command script to check for / install
  basic compiler support and  Python tool dependencies (right-click and "open"). Please follow
  on-screen instructions. 

* In case you have trouble accessing instrument files in certain areas
  of your disk, please give the McXtrace bundle "Full Disk Access"
  through your macOS Settings app

* Once the app and dependencies have been installed on your harddrive, optionally use the gui to make your McCode
the default mcstas/mcxtrace on your machine:
 * mxgui    (python) - Use "File -> Set as default"

* In case of issues installing / using this app bundle, please contact mcxtrace-users@mcxtrace.org
