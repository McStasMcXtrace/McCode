# Install McStas 3.4.x through conda-forge (macOS, Linux host)

**Warning: McStas through conda is considered EXPERIMENTAL and will
for the moment only work on macOS or Linux**

* We a set of conda-packages for installing McStas 3.4.x through conda

## Get yourself a conda / mamba
We recommend [miniforge](https://github.com/conda-forge/miniforge)

## Optionally create a dedicated environment and load it
```conda create --name mcstas ```
```conda activate mcstas```

## Install McStas, compilers, openmpi
```mamba install mcstas compilers openmpi```

## Install McStasScript, jypyterlab ipympl
```mamba install pip jupyterlab ipympl```
```pip install mcstasscript```

You are welcome to contact us in case of issues, e.g. via
*   [mcstas-users](mailto:mcstas-users@mcstas.org)
* by creating an issue on [the McCode issue tracker](https://github.com/McStasMcXtrace/McCode/issues)
