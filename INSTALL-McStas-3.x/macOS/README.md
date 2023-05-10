# Installation of McStas 3.3 on macOS 

## Supported macOS releases
* macOS 11-13 (Big Sur and later, Fully supported python tool set, partially
  supported perl tool set). Supported on both Intel and Apple Silicon,
  via separate installation bundles.

## Steps to perform

* Download the Intel package:
  [McStas 3.3 for macOS on Intel](https://download.mcstas.org/mcstas-3.3/mac/x86_64/mcstas-3.3_x86_64.tgz)
  or M1 package:
  [McStas 3.3 for macOS on Apple Silicon / M1](https://download.mcstas.org/mcstas-3.3/mac/arm64/mcstas-3.3_arm64.tgz)
 and unpack it (e.g. double-clicking should work)

* Open the resulting folder

* Drag the McStas-3.3.app and the McStas-3.3-environment.command to
/Applications

* Run the Check-PY-dependencies.command script to check for / install
  basic compiler support and  Python tool dependencies (right-click and "open"). Please follow
  on-screen instructions. The Silicon / M1 version requires the Arm
  version of Homebrew installed in /opt.

* In case you have trouble accessing instrument files in certain areas
  of your disk, please give the McStas bundle "Full Disk Access"
  through your macOS Settings app

* Optionally run the Check-PL-dependencies.command script to check for
  / install basic compiler support  Perl tool dependencies (right-click and "open"). Please follow
  on-screen instructions.

* Once the app and dependencies have been installed on your harddrive, optionally use the gui to make your McCode
the default mcstas/mcxtrace on your machine:
 * mcgui    (python) - Use "File -> Set as default"
 * Further menu points in the same place allows to configure the app to run the perl or python UI

* If you need support for NeXus output, please follow the instructions
  at https://github.com/McStasMcXtrace/McCode/wiki/McStas-and-Mantid#install-nexus

* In case of issues installing / using this app bundle, please contact mcstas-users@mcstas.org
