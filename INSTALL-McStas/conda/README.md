# Install McStas 3.5.1 through conda-forge (macOS, Linux or Windows host)

* We a set of conda-packages for installing McStas 3.5.1.x through conda

## Get yourself a conda / mamba
We recommend [miniforge](https://github.com/conda-forge/miniforge)

## Optionally create a dedicated environment and load it
* ```conda create --name mcstas ```
* ```conda activate mcstas```

## Install McStas, compilers, openmpi
* ```mamba install mcstas compilers openmpi=4```
  
(From next McStas release newer openmpi versions are also OK, but the 3.5.1 version of mcrun has an issue with openmpi 5.x)

## Note for use on Windows
At the time of release for 3.5.1 MCPL and NCrystal are not yet available for Windows on conda-forge, so related instruments will not compile/function.
We expect these dependencies to become available during the fall/winter of 2024.

## Install McStasScript, jypyterlab ipympl
* ```mamba install pip jupyterlab ipympl```
* ```pip install mcstasscript```

You are welcome to contact us in case of issues, e.g. via
*   [mcstas-users](mailto:mcstas-users@mcstas.org)
* by creating an issue on [the McCode issue tracker](https://github.com/McStasMcXtrace/McCode/issues)
