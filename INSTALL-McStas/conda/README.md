# Install McStas 3.5.1 through conda-forge (macOS, Linux or Windows host)

* We a set of conda-packages for installing McStas 3.5.1.x through conda

## Get yourself a conda / mamba
Due to the complex [licensing situation](https://discuss.scientific-python.org/t/response-to-anaconda-switch-to-paid-plans/1395) with the commercial Anaconda ecosystem, we clearly recommend McStas users to start from an open-source entry-point such as
* [miniforge](https://github.com/conda-forge/miniforge) or
* [micromamba](https://mamba.readthedocs.io/en/latest/user_guide/micromamba.html)

## If you ALREADY have a conda (be it Anaconda, blah-conda or whatever)
* Initially, list mcstas versions available on conda-forge, just to be sure you pick them up:
* ```conda search mcstas --channel conda-forge```, should give you many lines of the form
* ```mcstas                         3.5.8      hce30654_1  conda-forge   ```
* To install McStas with all needed dependencies, run
* ```conda create --name mcstas --channel conda-forge --channel nodefaults mcstas```

## If you DON'T have a conda already:

### Optionally create a dedicated environment and load it
* ```conda create --name mcstas ```
* ```conda activate mcstas```

### Install McStas, compilers, openmpi
* ```mamba install mcstas```

## Note for use on Windows
At the time of release for 3.5.1 MCPL and NCrystal are not yet available for Windows on conda-forge, so related instruments will not compile/function.
We expect these dependencies to become available during the fall/winter of 2024.

## Install McStasScript, jypyterlab ipympl
* ```mamba install pip jupyterlab ipympl```
* ```pip install mcstasscript```

You are welcome to contact us in case of issues, e.g. via
*   [mcstas-users](mailto:mcstas-users@mcstas.org)
* by creating an issue on [the McCode issue tracker](https://github.com/McStasMcXtrace/McCode/issues)
