## Install McXtrace 3.0 On Debian class systems (including Ubuntu, mint etc.):

# Add the McCode repository
After following the below steps your package manager should now be aware of mcxtrace
```bash
cd /etc/apt/sources.list.d
sudo wget http://packages.mccode.org/debian/mccode.list
sudo apt-get update
```

# Debian only:
On Debian you will further have to install the non-free repository to have access to all McXtrace tool parts. See https://wiki.debian.org/SourcesList

# Look for McXtrace packages to install
```bash
mcxtrace@debian:~$ apt-cache search mcxtrace | grep -v 2.
mcxtrace-3.0 - mcxtrace built using CMake
mcxtrace-comps-3.0 - mcxtrace-comps built using CMake
mcxtrace-manuals-3.0 - mcxtrace_manuals built using CMake
mcxtrace-suite - A metapackage for McXtrace + perl and python tools
mcxtrace-suite-perl-ng - A metapackage for McXtrace + perl tools
mcxtrace-suite-python-ng - A metapackage for McXtrace + python tools
mcxtrace-tools-matlab-mcplot-3.0 - matlab-tools-mcplot built using CMake
mcxtrace-tools-perl-3.0 - legacy-tools built using CMake
mcxtrace-tools-perl-cmdline-3.0 - legacy-tools-cmdline built using CMake
mcxtrace-tools-python-mccodelib-3.0 - python-tools-mccodelib built using CMake
mcxtrace-tools-python-mcdisplay-mantid-3.0 - python-tools-mcdisplay-mantid built using CMake
mcxtrace-tools-python-mcdisplay-pyqtgraph-3.0 - python-tools-mcdisplay-pyqtgraph built using CMake
mcxtrace-tools-python-mcdisplay-webgl-3.0 - python-tools-mcdisplay-webgl built using CMake
mcxtrace-tools-python-mcgui-3.0 - python-tools-mcgui built using CMake
mcxtrace-tools-python-mcplot-pyqtgraph-3.0 - python-tools-mcplot-pyqtgraph built using CMake
mcxtrace-tools-python-mcrun-3.0 - python-tools-mcrun built using CMake
```
The meta-packages mcxtrace-suite-perl-ng and mcxtrace-suite-pytho-ng
allows you to install mcxtrace 3.0 with one or both sets of tools (mcrun/mcplot etc.) by simple apt-get commands like
```bash
sudo apt-get install mcxtrace-suite-python-ng
```
The -suite packages without 'ng' in the package name will install the
latest McXtrace 1.x package

# Optionals
Optionally install iFit to visualize results using a Matlab environment (for free, no license needed).
Optionally install a VRML/X3D plotter such as Freewrl or InstantReality.
Optionally, you can install the NeXus format libraries to be able to export data files in HDF5.
Please report any trouble with the repository to [mcxtrace-users](mailto:mcxtrace-users@mcxtrace.org)

# Installing without adding the repo
If you want to attempt installing our debian packages manually via
dpkg, the packages are available for download at http://download.mcxtrace.org/mcxtrace-3.0/linux/mcxtrace-3.0-deb64/

## In case of issues
Please report any trouble with the repository to [mcxtrace-users](mailto:mcxtrace-users@mcxtrace.org), or raise an issue
in our GitHub issue tracker at: https://github.com/McStasMcXtrace/McCode/issues

