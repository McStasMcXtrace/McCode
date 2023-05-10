# Installing McStas 2.7.2 on Fedora 

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
mcstas@redhat~# yum search mcstas | grep -v 2.0 |grep -v 2.1 | grep -v 2.2 | grep -v 2.3 | grep -v 2.4 | grep -v 2.5 | grep -v 2.6
Loaded plugins: security
============================= N/S Matched: mcstas ==============================
mcstas-2.7.2.x86_64 : mcstas built using CMake
mcstas-comps-2.7.2.x86_64 : mcstas-comps built using CMake
mcstas-manuals-2.7.2.x86_64 : mcstas_manuals built using CMake
mcstas-suite.x86_64 : A metapackage for McStas + perl and python tools built using CMake
mcstas-suite-perl.x86_64 : A metapackage for McStas + perl tools built using CMake
mcstas-suite-python.x86_64 : A metapackage for McStas + python tools built using CMake
mcstas-miniconda3-2.7.2.x86_64 : miniconda3 built using CMake
mcstas-tools-matlab-mcplot-2.7.2.x86_64 : matlab-tools-mcplot built using CMake
mcstas-tools-perl-2.7.2.x86_64 : legacy-tools built using CMake
mcstas-tools-perl-cmdline-2.7.2.x86_64 : legacy-tools-cmdline built using CMake
mcstas-tools-python-mccodelib-2.7.2.x86_64 : python-tools-mccodelib built using CMake
mcstas-tools-python-mcdisplay-mantid-2.7.2.x86_64 : python-tools-mcdisplay-mantid built using CMake
mcstas-tools-python-mcdisplay-pyqtgraph-2.7.2.x86_64 : python-tools-mcdisplay-pyqtgraph built using CMake
mcstas-tools-python-mcdisplay-webgl-2.7.2.x86_64 : python-tools-mcdisplay-webgl
mcstas-tools-python-mcgui-2.7.2.x86_64 : python-tools-mcgui built using CMake
mcstas-tools-python-mcplot-pyqtgraph-2.7.2.x86_64 : python-tools-mcplot-pyqtgraph built using CMake
mcstas-tools-python-mcrun-2.7.2.x86_64 : python-tools-mcrun built using CMake

  Name and summary matches only, use "search all" for everything.
```
The meta-package mcstas-suite-python allows you to install mcstas with one or both sets of tools (mcrun/mcplot etc.) by simple yum commands like

```bash
sudo yum install mcstas-suite-python
```
The -suite packages including 'ng' in the package name will install the
latest McStas 3.x package


**NOTE** that the Python based package should automatically include
all dependencies, and hence is preferred!

## Install without repo use
If you want to attempt installing our RPM packages manually via rpm -i, the packages are available for download at https://download.mcstas.org/mcstas-2.7.2/linux/fedora/


## In case of issues
Please report any trouble with the repository to [mcstas-users](mailto:mcstas-users@mcstas.org)

