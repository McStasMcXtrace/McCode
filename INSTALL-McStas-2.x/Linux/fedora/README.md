# Installing McStas 2.7.2 on Fedora 

## Add the rpmfusion repo
Follow the instructions at https://rpmfusion.org/Configuration

## Install dependencies
```bash
sudo yum install pgplot perl-PGPLOT perl-Tk perl-PDL perl python2
```

## Install the remaining packages manually
* Downlooad the [all-rpms](https://download.mcstas.org/mcstas-2.7.2/linux/fedora/all-rpms.tgz)
* Unpack the archive
* Install the libtk-codetext-perl-0.3.4-2.noarch.rpm
```bash
sudo rpm -i libtk-codetext-perl-0.3.4-2.noarch.rpm
```
* Afterwards you can install the wanted packages, i.e for McStas + python
tools:
```bash
sudo rpm -i mcstas-2.7.2-rpm64.rpm mcstas-comps-2.7.2-rpm64.rpm \
mcstas-manuals-2.7.2-rpm64.rpm mcstas-miniconda3-2.7.2-rpm64.rpm \
mcstas-tools-perl-cmdline-2.7.2-rpm64.rpm \
mcstas-tools-python-mccodelib-2.7.2-rpm64.rpm \
mcstas-tools-python-mcdisplay-mantid-2.7.2-rpm64.rpm \
mcstas-tools-python-mcdisplay-pyqtgraph-2.7.2-rpm64.rpm \
mcstas-tools-python-mcdisplay-webgl-2.7.2-rpm64.rpm \
mcstas-tools-python-mcdoc-2.7.2-rpm64.rpm \
mcstas-tools-python-mcgui-2.7.2-rpm64.rpm \
mcstas-tools-python-mcplot-matplotlib-2.7.2-rpm64.rpm \
mcstas-tools-python-mcplot-pyqtgraph-2.7.2-rpm64.rpm \
mcstas-tools-python-mcrun-2.7.2-rpm64.rpm
```
* Optionally you can install the mcstas-tools-perl pakage which provides the **legacy** perl based gui/graphics tools:
```bash
sudo rpm -i mcstas-tools-perl-2.7.2-rpm64.rpm
```
* Dependencies can likely be resolved via https://rmpfusion.org (untested)
* (there are also other packages available that are optional, but
  whose dependencies are not easy to resolve)

## In case of issues
Please report any trouble with the repository to [mcstas-users](mailto:mcstas-users@mcstas.org)

