# Installing McStas 3.4 on Fedora 

McStas provides a package repository for use with RedHat-based
distributions, such as Fedora. To allow automatic update of your
mcstas, mcstas-components and tools when new revisions are relased,
follow the below instruction. **PLEASE NOTE** that our repo-based RPM
packages are built on Fedora 37.

An alternative solution for you may be to build yourself using the [repo build instructions](https://github.com/McStasMcXtrace/McCode/wiki/Building-McStas-McXtrace)


## Add the McCode repo to your system
```bash
cd /etc/yum.repos.d
sudo wget https://packages.mccode.org/rpm-fedora/mccode-fedora.repo
sudo yum update
```

## Look for the packages descriptions on your system
After following the above steps your package manager should now be aware of mcstas

```bash
mcstas@redhat~# yum search mcstas | grep -v 2.
Loaded plugins: security
============================= N/S Matched: mcstas ==============================
mcstas-3.4.x86_64 : mcstas built using CMake
mcstas-comps-3.4.x86_64 : mcstas-comps built using CMake
mcstas-manuals-3.4.x86_64 : mcstas_manuals built using CMake
mcstas-suite-python-ng.x86_64 : A metapackage for McStas + python tools built using CMake
mcstas-miniconda3.4.3.x86_64 : miniconda3 built using CMake
mcstas-tools-matlab-mcplot-3.4.x86_64 : matlab-tools-mcplot built using CMake
mcstas-tools-python-mccodelib-3.4.x86_64 : python-tools-mccodelib built using CMake
mcstas-tools-python-mcdisplay-mantid-3.4.x86_64 : python-tools-mcdisplay-mantid built using CMake
mcstas-tools-python-mcdisplay-pyqtgraph-3.4.x86_64 : python-tools-mcdisplay-pyqtgraph built using CMake
mcstas-tools-python-mcdisplay-webgl-3.4.x86_64 : python-tools-mcdisplay-webgl
mcstas-tools-python-mcgui-3.4.x86_64 : python-tools-mcgui built using CMake
mcstas-tools-python-mcplot-pyqtgraph-3.4.x86_64 : python-tools-mcplot-pyqtgraph built using CMake
mcstas-tools-python-mcrun-3.4.x86_64 : python-tools-mcrun built using CMake

  Name and summary matches only, use "search all" for everything.
```
The meta-package mcstas-suite-python-ng allows you to install mcstas 3.4 with one or both sets of tools (mcrun/mcplot etc.) by simple yum commands like

```bash
sudo yum install mcstas-suite-python
```
The -suite packages without 'ng' in the package name will install the
latest McStas 2.x package


**NOTE** that the Python based package should automatically include
all dependencies, and hence is preferred!

## Install without repo use
If you want to attempt installing our RPM packages manually via rpm -i, the packages are available for download at https://download.mcstas.org/mcstas-3.4/linux/fedora/


## In case of issues
Please report any trouble with the repository to [mcstas-users](mailto:mcstas-users@mcstas.org)

