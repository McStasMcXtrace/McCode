## Install McStas 2.7.2 On Debian class systems (including Ubuntu, mint etc.):

# Add the McCode repository
After following the below steps your package manager should now be aware of mcstas
```bash
cd /etc/apt/sources.list.d
sudo wget https://packages.mccode.org/debian/mccode.list
sudo apt-get update
```

# Debian only:
On Debian you will further have to install the non-free repository to have access to all McStas tool parts. See https://wiki.debian.org/SourcesList

# Look for McStas packages to install
```bash
mcstas@debian:~$ apt-cache search mcstas | grep -v 2.0 |grep -v 2.1 | grep -v 2.2 | grep -v 2.3 | grep -v 2.4 | grep -v 2.5 | grep -v 2.6
mcstas-2.7.2 - mcstas built using CMake
mcstas-comps-2.7.2 - mcstas-comps built using CMake
mcstas-manuals-2.7.2 - mcstas_manuals built using CMake
mcstas-suite - A metapackage for McStas + perl and python tools
mcstas-suite-perl - A metapackage for McStas + perl tools
mcstas-suite-python - A metapackage for McStas + python tools
mcstas-tools-matlab-mcplot-2.7.2 - matlab-tools-mcplot built using CMake
mcstas-tools-perl-2.7.2 - legacy-tools built using CMake
mcstas-tools-perl-cmdline-2.7.2 - legacy-tools-cmdline built using CMake
mcstas-tools-python-mccodelib-2.7.2 - python-tools-mccodelib built using CMake
mcstas-tools-python-mcdisplay-mantid-2.7.2 - python-tools-mcdisplay-mantid built using CMake
mcstas-tools-python-mcdisplay-pyqtgraph-2.7.2 - python-tools-mcdisplay-pyqtgraph built using CMake
mcstas-tools-python-mcdisplay-webgl-2.7.2 - python-tools-mcdisplay-webgl built using CMake
mcstas-tools-python-mcgui-2.7.2 - python-tools-mcgui built using CMake
mcstas-tools-python-mcplot-pyqtgraph-2.7.2 - python-tools-mcplot-pyqtgraph built using CMake
mcstas-tools-python-mcrun-2.7.2 - python-tools-mcrun built using CMake
```
The meta-package mcstas-suite-python
allows you to install mcstas 2.7.2 with one or both sets of tools (mcrun/mcplot etc.) by simple apt-get commands like
```bash
sudo apt-get install mcstas-suite-python
```
The -suite packages including 'ng' in the package name will install the
latest McStas 3.x package

# Using mcdoc on modern Ubuntu systems
Ubuntu is shipping its browsers as "snap" packages, meaning that they
are blocked from accessing e.g. the McStas html snippets in
/usr/share/mcstas/2.7.2/.

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
Please report any trouble with the repository to [mcstas-users](mailto:mcstas-users@mcstas.org)

# Installing without adding the repo
If you want to attempt installing our debian packages manually via
dpkg, the packages are available for download at https://download.mcstas.org/mcstas-2.7.2/linux/debian/
(available for multiple processor architectures).

## In case of issues
Please report any trouble with the repository to [mcstas-users](mailto:mcstas-users@mcstas.org)


