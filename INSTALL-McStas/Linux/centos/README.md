# Installing McStas 2.5 on CentOS 

McStas provides a package repository for use with RedHat-based distributions, such as CentOS. To allow automatic update of your mcstas, mcstas-components and tools when new revisions are relased, follow the below instruction. **PLEASE NOTE** that our repo-based RPM packages are built on CentOS 7 and will require glibc-2.14 or newer! Hence, a better solution for you may be to build yourself using the [repo build instructions](https://github.com/McStasMcXtrace/McCode/wiki/Building-McStas-McXtrace)


## Add the McCode repo to your system
cd /etc/yum.repos.d
sudo wget http://packages.mccode.org/rpm/mccode.repo
sudo yum update

## Add the EPEL extensions to your system
On some RPM-oriented systems like RHEL, CentOS or Scientific Linux, you may also need to install the EPEL extensions to resolve some dependencies. See e.g. [this webpage](https://www.cyberciti.biz/faq/installing-rhel-epel-repo-on-centos-redhat-7-x/) for further instructions.


## Look for the packages descriptions on your system
After following the above steps your package manager should now be aware of mcstas

```bash
mcstas@redhat~# yum search mcstas | grep -v 2.0 |grep -v 2.1 | grep -v 2.2 | grep -v 2.3 | grep -v 2.4
Loaded plugins: security
============================= N/S Matched: mcstas ==============================
mcstas-2.4.x86_64 : mcstas built using CMake
mcstas-comps-2.4.x86_64 : mcstas-comps built using CMake
mcstas-manuals-2.4.x86_64 : mcstas_manuals built using CMake
mcstas-suite.x86_64 : A metapackage for McStas + perl and python tools built using CMake
mcstas-suite-perl.x86_64 : A metapackage for McStas + perl tools built using CMake
mcstas-suite-python.x86_64 : A metapackage for McStas + python tools built using CMake
mcstas-miniconda3-2.4.x86_64 : miniconda3 built using CMake
mcstas-tools-matlab-mcplot-2.4.x86_64 : matlab-tools-mcplot built using CMake
mcstas-tools-perl-2.4.x86_64 : legacy-tools built using CMake
mcstas-tools-perl-cmdline-2.4.x86_64 : legacy-tools-cmdline built using CMake
mcstas-tools-python-mccodelib-2.4.x86_64 : python-tools-mccodelib built using CMake
mcstas-tools-python-mcdisplay-mantid-2.4.x86_64 : python-tools-mcdisplay-mantid built using CMake
mcstas-tools-python-mcdisplay-pyqtgraph-2.4.x86_64 : python-tools-mcdisplay-pyqtgraph built using CMake
mcstas-tools-python-mcdisplay-webgl-2.4.x86_64 : python-tools-mcdisplay-webgl
mcstas-tools-python-mcgui-2.4.x86_64 : python-tools-mcgui built using CMake
mcstas-tools-python-mcplot-pyqtgraph-2.4.x86_64 : python-tools-mcplot-pyqtgraph built using CMake
mcstas-tools-python-mcrun-2.4.x86_64 : python-tools-mcrun built using CMake

  Name and summary matches only, use "search all" for everything.
```
The meta-packages mcstas-suite-perl and mcstas-suite-python allows you to install mcstas with one or both sets of tools (mcrun/mcplot etc.) by simple yum commands like

```bash
sudo yum install mcstas-suite-python
```

**NOTE** that the Python based package should automatically include
all dependencies, and hence is preferred!

## Optionals
Optionally install iFit to visualize results using a Matlab environment (for free, no license needed).
Optionally install a VRML/X3D plotter such as Freewrl or InstantReality.
Optionally, you can install the NeXus format libraries to be able to
export data files in HDF5.

## Install without repo use
If you want to attempt installing our RPM packages manually via rpm -i, the packages are available for download at http://download.mcstas.org/current/linux/debian/mcstas-2.5-rpm64

## In case of issues
Please report any trouble with the repository to [mcstas-users](mailto:mcstas-users@mcstas.org)

