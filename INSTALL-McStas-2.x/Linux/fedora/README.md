# Installing McStas 2.7 on Fedora 

## Add the rpmfusion repo
Follow the instructions at https://rpmfusion.org/Configuration

## Install dependencies
```bash
sudo yum install pgplot perl-PGPLOT perl-Tk perl-PDL perl python2
```

## Install the remaining packages manually
* Downlooad the [all-rpms](http://download.mcstas.org/mcstas-2.7/linux/mcstas-2.7-rpm64-Fedora29/all-rpms.tgz)
* Unpack the archive
* Install the libtk-codetext-perl-0.3.4-2.noarch.rpm
```bash
sudo rpm -i libtk-codetext-perl-0.3.4-2.noarch.rpm
```
* Afterwards you can install the wanted packages, i.e for McStas + python
tools:
```bash
sudo rpm -i mcstas-2.7-rpm64.rpm mcstas-comps-2.7-rpm64.rpm \
mcstas-manuals-2.7-rpm64.rpm mcstas-miniconda3-2.7-rpm64.rpm \
mcstas-tools-perl-cmdline-2.7-rpm64.rpm \
mcstas-tools-python-mccodelib-2.7-rpm64.rpm \
mcstas-tools-python-mcdisplay-mantid-2.7-rpm64.rpm \
mcstas-tools-python-mcdisplay-pyqtgraph-2.7-rpm64.rpm \
mcstas-tools-python-mcdisplay-webgl-2.7-rpm64.rpm \
mcstas-tools-python-mcdoc-2.7-rpm64.rpm \
mcstas-tools-python-mcgui-2.7-rpm64.rpm \
mcstas-tools-python-mcplot-matplotlib-2.7-rpm64.rpm \
mcstas-tools-python-mcplot-pyqtgraph-2.7-rpm64.rpm \
mcstas-tools-python-mcrun-2.7-rpm64.rpm
```
* And add this extra package to also install the legacy perl based gui/graphics tools:
```bash
sudo rpm -i mcstas-tools-perl-2.7-rpm64.rpm
```
* (there are also other packages available that are optional, but
  whose dependencies are not easy to resolve)

## In case of issues
Please report any trouble with the repository to [mcstas-users](mailto:mcstas-users@mcstas.org)

