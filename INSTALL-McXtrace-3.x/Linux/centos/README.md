# Installing McXtrace 3.0 on CentOS 

McXtrace provides a package repository for use with RedHat-based
distributions, such as CentOS. To allow automatic update of your
mcxtrace, mcxtrace-components and tools when new revisions are relased,
follow the below instruction. Our repo-based RPM packages are built on
CentOS 7 and will require glibc-2.14 or newer. In some cases, the best solution for you may be to build yourself using the [repo build instructions](https://github.com/McStasMcXtrace/McCode/wiki/Building-McStas-McXtrace)


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
mcxtrace@redhat~# yum search mcxtrace | grep -v 2.
Loaded plugins: security
============================= N/S Matched: mcxtrace ==============================
mcxtrace-3.0.x86_64 : mcxtrace built using CMake
mcxtrace-comps-3.0.x86_64 : mcxtrace-comps built using CMake
mcxtrace-manuals-3.0.x86_64 : mcxtrace_manuals built using CMake
mcxtrace-suite-perl-ng.x86_64 : A metapackage for McXtrace + perl tools built using CMake
mcxtrace-suite-python-ng.x86_64 : A metapackage for McXtrace + python tools built using CMake
mcxtrace-miniconda3-3.0.x86_64 : miniconda3 built using CMake
mcxtrace-tools-matlab-mcplot-3.0.x86_64 : matlab-tools-mcplot built using CMake
mcxtrace-tools-perl-3.0.x86_64 : legacy-tools built using CMake
mcxtrace-tools-perl-cmdline-3.0.x86_64 : legacy-tools-cmdline built using CMake
mcxtrace-tools-python-mccodelib-3.0.x86_64 : python-tools-mccodelib built using CMake
mcxtrace-tools-python-mcdisplay-mantid-3.0.x86_64 : python-tools-mcdisplay-mantid built using CMake
mcxtrace-tools-python-mcdisplay-pyqtgraph-3.0.x86_64 : python-tools-mcdisplay-pyqtgraph built using CMake
mcxtrace-tools-python-mcdisplay-webgl-3.0.x86_64 : python-tools-mcdisplay-webgl
mcxtrace-tools-python-mcgui-3.0.x86_64 : python-tools-mcgui built using CMake
mcxtrace-tools-python-mcplot-pyqtgraph-3.0.x86_64 : python-tools-mcplot-pyqtgraph built using CMake
mcxtrace-tools-python-mcrun-3.0.x86_64 : python-tools-mcrun built using CMake

  Name and summary matches only, use "search all" for everything.
```
The meta-packages mcxtrace-suite-perl-ng and mcxtrace-suite-python-ng allows you to install mcxtrace 3.0 with one or both sets of tools (mcrun/mcplot etc.) by simple yum commands like

```bash
sudo yum install mcxtrace-suite-python
```
The -suite packages without 'ng' in the package name will install the
latest McXtrace 1.x package


**NOTE** that the Python based package should automatically include
all dependencies, and hence is preferred!

## Install without repo use
If you want to attempt installing our RPM packages manually via rpm -i, the packages are available for download at http://download.mcxtrace.org/mcxtrace-3.0/linux/mcxtrace-3.0-rpm64-CentOS_7/


## In case of issues
Please report any trouble with the repository to [mcxtrace-users](mailto:mcxtrace-users@mcxtrace.org)

