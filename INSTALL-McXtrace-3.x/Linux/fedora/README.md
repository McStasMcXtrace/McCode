# Installing McXtrace 3.0 on Fedora 33

## Add the rpmfusion repo
Follow the instructions at https://rpmfusion.org/Configuration

## Install dependencies
```bash
sudo yum install pgplot perl-PGPLOT perl-Tk perl-PDL perl python2
```

## Install the remaining packages manually
<<<<<<< HEAD
* Download the [all-rpms](http://download.mcxtrace.org/mcxtrace-3.0/linux/fedora/all-rpms.tgz)
=======
* Download the [all-rpms](http://download.mcxtrace.org/current/linux/mcxtrace-3.0-rpm64-Fedora33/all-rpms.tgz)
>>>>>>> b2c46b000... Put in dedicated folder structure
* Unpack the archive
* Install the libtk-codetext-perl-0.3.4-2.noarch.rpm
```bash
sudo rpm -i libtk-codetext-perl-0.3.4-2.noarch.rpm
```
* Afterwards you can install the wanted packages, i.e for McXtrace + python
tools:
```bash
sudo rpm -i mcxtrace-3.0-rpm64.rpm mcxtrace-comps-3.0-rpm64.rpm \
mcxtrace-manuals-3.0-rpm64.rpm mcxtrace-miniconda3-3.0-rpm64.rpm \
mcxtrace-tools-perl-cmdline-3.0-rpm64.rpm \
mcxtrace-tools-python-mccodelib-3.0-rpm64.rpm \
mcxtrace-tools-python-mcdisplay-mantid-3.0-rpm64.rpm \
mcxtrace-tools-python-mcdisplay-pyqtgraph-3.0-rpm64.rpm \
mcxtrace-tools-python-mcdisplay-webgl-3.0-rpm64.rpm \
mcxtrace-tools-python-mcdoc-3.0-rpm64.rpm \
mcxtrace-tools-python-mcgui-3.0-rpm64.rpm \
mcxtrace-tools-python-mcplot-matplotlib-3.0-rpm64.rpm \
mcxtrace-tools-python-mcplot-pyqtgraph-3.0-rpm64.rpm \
mcxtrace-tools-python-mcrun-3.0-rpm64.rpm
```
<<<<<<< HEAD

* Optionally you can install the mcxtrace-tools-perl pakage which provides the **legacy** perl based gui/graphics tools:
```bash
sudo rpm -i mcxtrace-tools-perl-3.0-rpm64.rpm
```
* Dependencies can likely be resolved via https://rmpfusion.org (untested)
=======
* And add this extra package to also install the legacy perl based gui/graphics tools:
```bash
sudo rpm -i mcxtrace-tools-perl-3.0-rpm64.rpm
```
>>>>>>> b2c46b000... Put in dedicated folder structure
* (there are also other packages available that are optional, but
  whose dependencies are not easy to resolve)

## In case of issues
Please report any trouble with the repository to [mcxtrace-users](mailto:mcxtrace-users@mcxtrace.org)

