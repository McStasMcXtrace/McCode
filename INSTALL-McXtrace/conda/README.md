# Install McXtrace 3.5.1 through conda-forge (macOS, Linux or Windows host)

* We a set of conda-packages for installing McXtrace 3.5.1.x through conda

## Get yourself a conda / mamba
Due to the complex [licensing situation](https://discuss.scientific-python.org/t/response-to-anaconda-switch-to-paid-plans/1395) with the commercial Anaconda ecosystem, we clearly recommend McXtrace users to start from an open-source entry-point such as
* [miniforge](https://github.com/conda-forge/miniforge) or
* [micromamba](https://mamba.readthedocs.io/en/latest/user_guide/micromamba.html)

## If you ALREADY have a conda (be it Anaconda, blah-conda or whatever)
* Initially, list mcstas versions available on conda-forge, just to be sure you pick them up:
* ```conda search mcxtrace --channel conda-forge```, should give you many lines of the form
* ```mcxtrace                         3.5.8      hce30654_1  conda-forge   ```
* To install McStrace with all needed dependencies, run
* ```conda create --name mcxtrace --channel conda-forge --channel nodefaults mcxtrace```

## If you DON'T have a conda already:

### Optionally create a dedicated environment and load it
* ```conda create --name mcxtrace ```
* ```conda activate mcxtrace```

### Install McStas, compilers, openmpi
* ```mamba install mcxtrace```
 
## Note for use on Windows
At the time of release for 3.5.1 MCPL is notr yet available for Windows on conda-forge, so related instruments will not compile/function.
We expect this dependency to become available during the fall/winter of 2024.

## Install McXtraceScript, jypyterlab ipympl
* ```mamba install pip jupyterlab ipympl```
* ```pip install mcxtracescript```

You are welcome to contact us in case of issues, e.g. via
*   [mcxtrace-users](mailto:mcxtrace-users@mcxtrace.org)
* by creating an issue on [the McCode issue tracker](https://github.com/McStasMcXtrace/McCode/issues)
