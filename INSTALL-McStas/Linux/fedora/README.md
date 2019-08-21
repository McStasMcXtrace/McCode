# Installing McStas 2.5 on Fedorda 

## Add the rpmfusion repo
Follow the instructions at https://rpmfusion.org/Configuration

## Install dependencies
```bash
sudo yum install pgplot perl-PGPLOT perl-Tk perl-PDL perl python2
```

## Install the remaining packages manually
* Downlooad the [all-rpms](http://download.mcstas.org/current/linux/mcstas-2.5-rpm64-Fedora29/all-rpms.tgz)
* Unpack the archive
* Install the libtk-codetext-perl-0.3.4-2.noarch.rpm
```bash
sudo rpm -i libtk-codetext-perl-0.3.4-2.noarch.rpm
```
* Afterwards you can install the wanted packages, i.e for McStas + python
tools:
```bash
sudo rpm -i mcstas-2.5-rpm64.rpm mcstas-comps-2.5-rpm64.rpm \
mcstas-manuals-2.5-rpm64.rpm mcstas-miniconda3-2.5-rpm64.rpm \
mcstas-tools-perl-cmdline-2.5-rpm64.rpm \
mcstas-tools-python-mccodelib-2.5-rpm64.rpm \
mcstas-tools-python-mcdisplay-mantid-2.5-rpm64.rpm \
mcstas-tools-python-mcdisplay-pyqtgraph-2.5-rpm64.rpm \
mcstas-tools-python-mcdisplay-webgl-2.5-rpm64.rpm \
mcstas-tools-python-mcdoc-2.5-rpm64.rpm \
mcstas-tools-python-mcgui-2.5-rpm64.rpm \
mcstas-tools-python-mcplot-matplotlib-2.5-rpm64.rpm \
mcstas-tools-python-mcplot-pyqtgraph-2.5-rpm64.rpm \
mcstas-tools-python-mcrun-2.5-rpm64.rpm
```
* And add this extra package to also install the legacy perl based gui/graphics tools:
```bash
sudo rpm -i mcstas-tools-perl-2.5-rpm64.rpm
```
* (there are also other packages available that are optional, but
  whose dependencies are not easy to resolve)

## Installing and using NCrystal
Unfortunately, the mcstas-suite-\*-2.5 packages did not include the mcstas-ncrystal-2.5 package and hence needs to be installed independently, i.e.
```bash
sudo rpm -i mcstas-ncrystal-2.5-rpm64.rpm
```
which then has the side effect that some symlinks are broken. Hence also please reinstall e.g. mcstas-tools-python-mcrun-2.5:
```bash
sudo rpm -i mcstas-2.5-rpm64.rpm mcstas-tools-python-mcrun-2.5-rpm64.rpm
```
Further the example instrument [NCrystal_example_mcstas.instr](http://download.mcstas.org/current/linux/mcstas-2.5-rpm64-Fedora29/NCrystal_example_mcstas.instr) is provided separately as it is missing the the mcstas-comps package...

All of this mess will be properly resolved in a McStas 2.5.1 during the first months of 2019... :)


## In case of issues
Please report any trouble with the repository to [mcstas-users](mailto:mcstas-users@mcstas.org)

