# Installation of McStas 2.7.2 on macOS 

## Supported macOS releases
* macOS 11-13 (Big Sur and later, Fully supported python tool set, partially
  supported perl tool set). Supported on both Intel and Apple Silicon,
  via separate installation bundles.

## Steps to perform

* Download the Intel package:
  [McStas 2.7.2 for macOS on Intel](https://download.mcstas.org/mcstas-2.7.2/mac/x86_64/mcstas-2.7.2_x86_64.tgz)
  or Silicon / M-series package:
  [McStas 2.7.2 for macOS on Apple Silicon / M-series](https://download.mcstas.org/mcstas-2.7.2/mac/arm64/mcstas-2.7.2_arm64.tgz)
 and unpack it (e.g. double-clicking should work)

* Open the resulting folder

* Drag the McStas-2.7.2.app and the McStas-2.7.2-environment.command to
/Applications

* Run the Check-PY-dependencies.command script to check for / install
  basic compiler support and  Python tool dependencies (right-click and "open"). Please follow
  on-screen instructions. 

* In case you have trouble accessing instrument files in certain areas
  of your disk, please give the McStas bundle "Full Disk Access"
  through your macOS Settings app

* Once the app and dependencies have been installed on your harddrive, optionally use the gui to make your McCode
the default mcstas/mcxtrace on your machine:
 * mcgui    (python) - Use "File -> Set as default"

* If you need support for NeXus output, please follow the instructions
  at https://github.com/McStasMcXtrace/McCode/wiki/McStas-and-Mantid#install-nexus

* In case of issues installing / using this app bundle, please contact mcstas-users@mcstas.org
