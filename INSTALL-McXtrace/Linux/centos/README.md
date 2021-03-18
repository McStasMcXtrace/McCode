# Installing McXtrace 1.5 on CentOS

McXtrace provides a package repository for use with RedHat-based distributions, such as CentOS. To allow automatic update of your mcxtrace, mcxtrace-components and tools when new revisions are relased, follow the below instruction. **PLEASE NOTE** that our repo-based RPM packages are built on CentOS 7 and will require glibc-2.14 or newer! Hence, a better solution for you may be to build yourself using the [repo build instructions](https://github.com/McXtraceMcXtrace/McCode/wiki/Building-McStas-McXtrace)


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
mcxtrace@redhat~# yum search mcxtrace | grep -v 1.4 |grep -v 1.2
Loaded plugins: security
============================= N/S Matched: mcxtrace ==============================
mcxtrace-1.5.x86_64 : mcxtrace built using CMake
mcxtrace-comps-1.5.x86_64 : mcxtrace-comps built using CMake
mcxtrace-manuals-1.5.x86_64 : mcxtrace_manuals built using CMake
mcxtrace-suite.x86_64 : A metapackage for McXtrace + perl and python tools built using CMake
mcxtrace-suite-perl.x86_64 : A metapackage for McXtrace + perl tools built using CMake
mcxtrace-suite-python.x86_64 : A metapackage for McXtrace + python tools built using CMake
mcxtrace-miniconda3-1.5.x86_64 : miniconda3 built using CMake
mcxtrace-tools-matlab-mxplot-1.5.x86_64 : matlab-tools-mxplot built using CMake
mcxtrace-tools-perl-1.5.x86_64 : legacy-tools built using CMake
mcxtrace-tools-perl-cmdline-1.5.x86_64 : legacy-tools-cmdline built using CMake
mcxtrace-tools-python-mccodelib-1.5.x86_64 : python-tools-mccodelib built using CMake
mcxtrace-tools-python-mxdisplay-mantid-1.5.x86_64 : python-tools-mxdisplay-mantid built using CMake
mcxtrace-tools-python-mxdisplay-pyqtgraph-1.5.x86_64 : python-tools-mxdisplay-pyqtgraph built using CMake
mcxtrace-tools-python-mxdisplay-webgl-1.5.x86_64 : python-tools-mxdisplay-webgl
mcxtrace-tools-python-mxgui-1.5.x86_64 : python-tools-mxgui built using CMake
mcxtrace-tools-python-mxplot-pyqtgraph-1.5.x86_64 : python-tools-mxplot-pyqtgraph built using CMake
mcxtrace-tools-python-mxplot-svg-1.5.x86_64 : python-tools-mxplot-pyqtgraph built using CMake
mcxtrace-tools-python-mxrun-1.5.x86_64 : python-tools-mxrun built using CMake
mcxtrace-tools-python-mxdoc-1.5.x86_64 : python-tools-mxdoc built using CMake

  Name and summary matches only, use "search all" for everything.
```
The meta-packages *mcxtrace-suite-python* and *mcxtrace-suite-perl* allows you to install mcxtrace with one or both sets of tools (mxrun/mxplot etc.) by simple yum commands like

```bash
sudo yum install mcxtrace-suite-python
```

**NOTE** that the Python based package should automatically include
all dependencies, and hence is preferred!

## Install without repo use
If you want to attempt installing our RPM packages manually via rpm -i, the packages are available for download at http://download.mcxtrace.org/current/linux/mcxtrace-1.5rc1-rpm64-CentOS_7/

## In case of issues
Please report any trouble with the repository to [mcxtrace-users](mailto:mcstas-users@mcxtrace.org)

