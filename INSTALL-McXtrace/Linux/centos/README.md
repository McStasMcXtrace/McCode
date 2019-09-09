# Installing McXtrace 1.5rc1 on CentOS 

McXtrace provides a package repository for use with RedHat-based distributions, such as CentOS. To allow automatic update of your mcxtrace, mcxtrace-components and tools when new revisions are relased, follow the below instruction. **PLEASE NOTE** that our repo-based RPM packages are built on CentOS 7 and will require glibc-2.14 or newer! Hence, a better solution for you may be to build yourself using the [repo build instructions](https://github.com/McStasMcXtrace/McCode/wiki/Building-McStas-McXtrace)


## Add the McCode repo to your system
```bash
cd /etc/yum.repos.d
sudo wget http://packages.mccode.org/rpm/mccode.repo
sudo yum update
```


## Add the EPEL extensions to your system
```bash
sudo yum install epel-release
sudo yum update
```

## Look for the packages descriptions on your system
After following the above steps your package manager should now be aware of mcxtrace

```bash
mcxtrace@redhat~# yum search mcxtrace | grep -v 2.0 |grep -v 2.1 | grep -v 2.2 | grep -v 2.3 | grep -v 2.4
Loaded plugins: security
============================= N/S Matched: mcstas ==============================
mcxtrace-1.5rc1.x86_64 : mcstas built using CMake
mcxtrace-comps-1.5rc1.x86_64 : mcstas-comps built using CMake
mcxtrace-manuals-1.5rc1.x86_64 : mcstas_manuals built using CMake
mcxtrace-suite.x86_64 : A metapackage for McStas + perl and python tools built using CMake
mcxtrace-suite-perl.x86_64 : A metapackage for McStas + perl tools built using CMake
mcxtrace-suite-python.x86_64 : A metapackage for McStas + python tools built using CMake
mcxtrace-miniconda3-1.5rc1.x86_64 : miniconda3 built using CMake
mcxtrace-tools-matlab-mcplot-1.5rc1.x86_64 : matlab-tools-mcplot built using CMake
mcxtrace-tools-perl-1.5rc1.x86_64 : legacy-tools built using CMake
mcxtrace-tools-perl-cmdline-1.5rc1.x86_64 : legacy-tools-cmdline built using CMake
mcxtrace-tools-python-mccodelib-1.5rc1.x86_64 : python-tools-mccodelib built using CMake
mcxtrace-tools-python-mcdisplay-mantid-1.5rc1.x86_64 : python-tools-mcdisplay-mantid built using CMake
mcxtrace-tools-python-mcdisplay-pyqtgraph-1.5rc1.x86_64 : python-tools-mcdisplay-pyqtgraph built using CMake
mcxtrace-tools-python-mcdisplay-webgl-1.5rc1.x86_64 : python-tools-mcdisplay-webgl
mcxtrace-tools-python-mcgui-1.5rc1.x86_64 : python-tools-mcgui built using CMake
mcxtrace-tools-python-mcplot-pyqtgraph-1.5rc1.x86_64 : python-tools-mcplot-pyqtgraph built using CMake
mcxtrace-tools-python-mcrun-1.5rc1.x86_64 : python-tools-mcrun built using CMake

  Name and summary matches only, use "search all" for everything.
```
The meta-packages mcxtrace-suite-perl and mcxtrace-suite-python allows you to install mcstas with one or both sets of tools (mxrun/mxplot etc.) by simple yum commands like

```bash
sudo yum install mcxtrace-suite-python
```

**NOTE** that the Python based package should automatically include
all dependencies, and hence is preferred!

## Install without repo use
If you want to attempt installing our RPM packages manually via rpm -i, the packages are available for download at http://download.mcxtrace.org/current/linux/mcxtrace-1.5rc1-rpm64-CentOS_7/

## In case of issues
Please report any trouble with the repository to [mcstas-users](mailto:mcstas-users@mcstas.org)

