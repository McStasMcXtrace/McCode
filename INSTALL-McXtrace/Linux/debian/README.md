## Install McXtrace 3.2 On Debian class systems (including Ubuntu, mint etc.):
The packages have been tested to work correctly on Ubuntu 22.04 and Debian 11.

# Add the McCode repository
After following the below steps your package manager should now be aware of mcxtrace
```bash
cd /etc/apt/sources.list.d
sudo wget https://packages.mccode.org/debian/mccode.list
sudo apt-get update
```

# Debian only:
On Debian you will further have to install the non-free repository to have access to all McXtrace tool parts. See https://wiki.debian.org/SourcesList

# The easiest way:
The meta-package *mcxtrace-suite-python-ng* will allow you to install mcxtrace with the recommended set of python-tools (mcrun/mcplot etc.) by a simple apt-get command like
```bash
sudo apt-get install mcxtrace-suite-python-ng
```
The -suite packages without 'ng' in the package name will install the
latest McXtrace 1.x package

# Look for McXtrace packages to install
```bash
mcxtrace@debian:~$ apt-cache search mcxtrace | grep 3.2
mcxtrace-3.2 - mcxtrace built using CMake
mcxtrace-comps-3.2 - mcxtrace-comps built using CMake
mcxtrace-manuals-3.2 - mcxtrace_manuals built using CMake
mcxtrace-suite-python-ng - A metapackage for McStas + python tools
mcxtrace-tools-matlab-mxplot-3.2 - matlab-tools-mcplot built using CMake
mcxtrace-tools-perl-3.2 - legacy-tools built using CMake
mcxtrace-tools-perl-cmdline-3.2 - legacy-tools-cmdline built using CMake
mcxtrace-tools-python-mxcodelib-3.2 - python-tools-mccodelib built using CMake
mcxtrace-tools-python-mxdisplay-pyqtgraph-3.2 - python-tools-mxdisplay-pyqtgraph built using CMake
mcxtrace-tools-python-mxdisplay-webgl-3.2 - python-tools-mxdisplay-webgl built using CMake
mcxtrace-tools-python-mxgui-3.2 - python-tools-mxgui built using CMake
mcxtrace-tools-python-mxplot-pyqtgraph-3.2 - python-tools-mxplot-pyqtgraph built using CMake
mcxtrace-tools-python-mxrun-3.2 - python-tools-mxrun built using CMake
```
# Using mxdoc on modern Ubuntu systems
Ubuntu is shipping its browsers as "snap" packages, meaning that they
are blocked from accessing e.g. the McStas html snippets in
/usr/share/mcxtrace/3.2/.

To fix this we propose to switch your browser to a proper apt based
installation, in this example firefox:

Remove the snap-based firefox:
```
sudo snap remove firefox
```
Add the official mozilla-built firefox instead:
```
sudo add-apt-repository ppa:mozillateam/ppa
```
Set priorities to always prefer this firefox package:
```
echo '
Package: *
Pin: release o=LP-PPA-mozillateam
Pin-Priority: 1001
' | sudo tee /etc/apt/preferences.d/mozilla-firefox
```
Set up unattended upgrades of firefox
```
echo 'Unattended-Upgrade::Allowed-Origins::
"LP-PPA-mozillateam:${distro_codename}";' | sudo tee
/etc/apt/apt.conf.d/51unattended-upgrades-firefox
```
Finally, install firefox from the mozilla-channels
```
sudo apt install firefox
```

# Optionals
Optionally install iFit to visualize results using a Matlab environment (for free, no license needed).
Optionally install a VRML/X3D plotter such as Freewrl or InstantReality.
Optionally, you can install the NeXus format libraries to be able to export data files in HDF5.
Please report any trouble with the repository to [mcxtrace-users](mailto:mcxtrace-users@mcxtrace.org)

# Installing without adding the repo
If you want to attempt installing our debian packages manually via
dpkg, the packages are available for download at https://download.mcxtrace.org/current/linux/mcxtrace-3.2-deb64/
(available for multiple processor architectures).

## In case of issues
Please report any trouble with the repository to [mcxtrace-users](mailto:mcxtrace-users@mcxtrace.org)
