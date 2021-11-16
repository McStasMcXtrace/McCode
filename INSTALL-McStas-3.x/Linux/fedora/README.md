# Installing McStas 3.0 on Fedora 33

## Add the rpmfusion repo
Follow the instructions at https://rpmfusion.org/Configuration

## Install dependencies
```bash
sudo yum install pgplot perl-PGPLOT perl-Tk perl-PDL perl python2
```

## Install the remaining packages manually
* Download the [all-rpms](https://download.mcstas.org/mcstas-3.0/linux/fedora/all-rpms.tgz)
* Unpack the archive
* Install the libtk-codetext-perl-0.3.4-2.noarch.rpm
```bash
sudo rpm -i libtk-codetext-perl-0.3.4-2.noarch.rpm
```
* Afterwards you can install the wanted packages, i.e for McStas + python
tools:
```bash
sudo rpm -i mcstas-3.0-rpm64.rpm mcstas-comps-3.0-rpm64.rpm \
mcstas-manuals-3.0-rpm64.rpm mcstas-miniconda3-3.0-rpm64.rpm \
mcstas-tools-perl-cmdline-3.0-rpm64.rpm \
mcstas-tools-python-mccodelib-3.0-rpm64.rpm \
mcstas-tools-python-mcdisplay-mantid-3.0-rpm64.rpm \
mcstas-tools-python-mcdisplay-pyqtgraph-3.0-rpm64.rpm \
mcstas-tools-python-mcdisplay-webgl-3.0-rpm64.rpm \
mcstas-tools-python-mcdoc-3.0-rpm64.rpm \
mcstas-tools-python-mcgui-3.0-rpm64.rpm \
mcstas-tools-python-mcplot-matplotlib-3.0-rpm64.rpm \
mcstas-tools-python-mcplot-pyqtgraph-3.0-rpm64.rpm \
mcstas-tools-python-mcrun-3.0-rpm64.rpm
```

* Optionally you can install the mcstas-tools-perl pakage which provides the **legacy** perl based gui/graphics tools:
```bash
sudo rpm -i mcstas-tools-perl-3.0-rpm64.rpm
```
* Dependencies can likely be resolved via https://rmpfusion.org (untested)
* (there are also other packages available that are optional, but
  whose dependencies are not easy to resolve)

## In case of issues
Please report any trouble with the repository to [mcstas-users](mailto:mcstas-users@mcstas.org)

