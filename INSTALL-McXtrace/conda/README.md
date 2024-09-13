# Install McXtrace 3.5.0 through conda-forge (macOS, Linux or Windows host)

* We a set of conda-packages for installing McXtrace 3.5.0.x through conda

## Get yourself a conda / mamba
We recommend [miniforge](https://github.com/conda-forge/miniforge)

## Optionally create a dedicated environment and load it
* ```conda create --name mcxtrace ```
* ```conda activate mcxtrace```

## Install McXtrace, compilers, openmpi
* ```mamba install mcxtrace compilers openmpi=4```
  
(From next McXtrace release newer openmpi versions are also OK, but the 3.5.0 version of mcrun has an issue with openmpi 5.x)

## Note for use on Windows
At the time of release for 3.5.0 MCPL and NCrystal are not yet available for Windows on conda-forge, so related instruments will not compile/function.
We expect these dependencies to become available during the fall/winter of 2024.

## Install McXtraceScript, jypyterlab ipympl
* ```mamba install pip jupyterlab ipympl```
* ```pip install mcxtracescript```

You are welcome to contact us in case of issues, e.g. via
*   [mcxtrace-users](mailto:mcxtrace-users@mcxtrace.org)
* by creating an issue on [the McCode issue tracker](https://github.com/McXtraceMcXtrace/McCode/issues)
