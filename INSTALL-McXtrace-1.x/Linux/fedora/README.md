# Installing McXtrace 1.7.1 on CentOS

McXtrace provides a package repository for use with RedHat-based
distributions, such as Fedora 37 or newer. To allow automatic update
of your mcxtrace, mcxtrace-components and tools when new revisions are
relased, follow the below instruction. **PLEASE NOTE** that our
repo-based RPM packages are built on Fedora 37 and will require a
reasonably new glibc!

An alternative solution for you may be, to build yourself using the [repo build instructions](https://github.com/McXtraceMcXtrace/McCode/wiki/Building-McStas-McXtrace)


## Add the McCode repo to your system
```bash
cd /etc/yum.repos.d
sudo wget https://packages.mccode.org/rpm-fedora/mccode-fedora.repo
sudo yum update
```

## Look for the packages descriptions on your system
After following the above steps your package manager should now be aware of mcxtrace
```bash
yum search mcxtrace* |grep -v 1.5 |grep -v 1.4 |grep -v 1.2 |grep -v 1.1
Last metadata expiration check: 0:00:59 ago on Fri 13 Aug 2021 13:30:41 UTC.
====================== Name & Summary Matched: mcxtrace* =======================
mcxtrace-1.7.1.x86_64 : mcxtrace built using CMake
mcxtrace-comps-1.7.1.x86_64 : mcxtrace-comps built using CMake
mcxtrace-manuals-1.7.1.x86_64 : mcxtrace_manuals built using CMake
=========================== Name Matched: mcxtrace* ============================
mcxtrace-clustertools-1.7.1.x86_64 : perl-cluster-tools built using CMake
mcxtrace-miniconda3-1.7.1.x86_64 : miniconda3 built using CMake
mcxtrace-suite.x86_64 : A metapackage for McXtrace + perl and python tools
mcxtrace-suite-perl.x86_64 : A metapackage for McXtrace + perl tools
mcxtrace-suite-python.x86_64 : A metapackage for McXtrace + python tools
mcxtrace-tools-matlab-mxplot-1.7.1.x86_64 : matlab-tools-mcplot built using CMake
mcxtrace-tools-perl-1.7.1.x86_64 : legacy-tools built using CMake
mcxtrace-tools-perl-cmdline-1.7.1.x86_64 : legacy-tools-cmdline built using CMake
mcxtrace-tools-python-mccodelib-1.7.1.x86_64 : python-tools-mccodelib built using CMake
mcxtrace-tools-python-mxdisplay-pyqtgraph-1.7.1.x86_64 : python-tools-mcdisplay-pyqtgraph built using CMake
mcxtrace-tools-python-mxdisplay-webgl-1.7.1.x86_64 : python-tools-mcdisplay-webgl built using CMake
mcxtrace-tools-python-mxdoc-1.7.1.x86_64 : python-tools-mcdoc built using CMake
mcxtrace-tools-python-mxgui-1.7.1.x86_64 : python-tools-mcgui built using CMake
mcxtrace-tools-python-mxplot-matplotlib-1.7.1.x86_64 : python-tools-mcplot built using CMake
mcxtrace-tools-python-mxplot-pyqtgraph-1.7.1.x86_64 : python-tools-mcplot-pyqtgraph built using CMake
mcxtrace-tools-python-mxplot-svg-1.7.1.x86_64 : python-tools-mcplot-svg built using CMake
mcxtrace-tools-python-mxrun-1.7.1.x86_64 : python-tools-mcrun built using CMake
```
```
The meta-package *mcxtrace-suite-python-n* allows you to install
mcxtrace with the recommended set of python tools (mxrun/mxplot etc.)
by a simple yum command:

```bash
sudo yum install mcxtrace-suite-python
```
Choosing mcxtrace-suite-python-ng  will similarly install the latest 3.x release

## Install without repo use
If you want to attempt installing our RPM packages manually via rpm -i, the packages are available for download at https://download.mcxtrace.org/current/linux/mcxtrace-rpm64_x86_64_fedora

## In case of issues
Please report any trouble with the repository to [mcxtrace-users](mailto:mcstas-users@mcxtrace.org)

