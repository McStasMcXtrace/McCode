## Install McXtrace 1.7 On Debian class systems (including Ubuntu, mint etc.):

# Add the McCode repository
After following the below steps your package manager should now be aware of mcxtrace
```bash
cd /etc/apt/sources.list.d
sudo wget http://packages.mccode.org/debian/mccode.list
sudo apt-get update
```

# Debian only:
On Debian you will further have to install the non-free repository to have access to all McXtrace tool parts. See https://wiki.debian.org/SourcesList

# The easiest way:
The meta-packages *mcxtrace-suite-perl* and *mcxtrace-suite-python* allow you to install mcxtrace with one or both sets of tools (mcrun/mcplot etc.) by simple apt-get commands like
```bash
sudo apt-get install mcxtrace-suite-python
```
For the legacy perl tools just replace python with perl.

# Look for McXtrace packages to install
```bash
mcxtrace@debian:~$ apt-cache search mcxtrace | grep 1.7
mcxtrace-1.7 - mcxtrace built using CMake
mcxtrace-comps-1.7 - mcxtrace-comps built using CMake
mcxtrace-manuals-1.7 - mcxtrace_manuals built using CMake
mcxtrace-suite - A metapackage for McStas + perl and python tools
mcxtrace-suite-perl - A metapackage for McStas + perl tools
mcxtrace-suite-python - A metapackage for McStas + python tools
mcxtrace-tools-matlab-mxplot-1.7 - matlab-tools-mcplot built using CMake
mcxtrace-tools-perl-1.7 - legacy-tools built using CMake
mcxtrace-tools-perl-cmdline-1.7 - legacy-tools-cmdline built using CMake
mcxtrace-tools-python-mxcodelib-1.7 - python-tools-mccodelib built using CMake
mcxtrace-tools-python-mxdisplay-pyqtgraph-1.7 - python-tools-mxdisplay-pyqtgraph built using CMake
mcxtrace-tools-python-mxdisplay-webgl-1.7 - python-tools-mxdisplay-webgl built using CMake
mcxtrace-tools-python-mxgui-1.7 - python-tools-mxgui built using CMake
mcxtrace-tools-python-mxplot-pyqtgraph-1.7 - python-tools-mxplot-pyqtgraph built using CMake
mcxtrace-tools-python-mxrun-1.7 - python-tools-mxrun built using CMake
```
# Optionals
Optionally install iFit to visualize results using a Matlab environment (for free, no license needed).
Optionally install a VRML/X3D plotter such as Freewrl or InstantReality.
Optionally, you can install the NeXus format libraries to be able to export data files in HDF5.
Please report any trouble with the repository to [mcxtrace-users](mailto:mcxtrace-users@mcxtrace.org)

# Installing without adding the repo
If you want to attempt installing our debian packages manually via
dpkg, the packages are available for download at http://download.mcxtrace.org/current/linux/mcxtrace-1.5-deb64/

## In case of issues
Please report any trouble with the repository to [mcxtrace-users](mailto:mcxtrace-users@mcxtrace.org)
