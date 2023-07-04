# Description

The `sasmodels2mcstas.py` contains three main methods that create:

- c files from `models` and `other_models` folder.
- component files from the generated c files.
- test folders from the generated component files.

You may add new models to the `other_models` folder. the kernel_header.c
file is needed as a prefix for all .c files, and all folders in this
directory should keep their names.

For the moment, 70 SANS models for form factors are created, out of which
21 of them have anisotropic descriptions.

# Run 
In mcstas-environment run 

`python sasmodels2mcstas.py .`

By default, it will create a `generated_mcstas_models` folder in which all
c files will be stored. An `indiv_coms` folder with all the individual 
model components and a test folder with one test per model will also be created.

## TODO
For the moment, sasmodels `onion`, `core_multi_shell`, `rpa`, and `spherical_sld`
are not available given that they need a vector input. The models will be created
but are not functional. An implementation of these models is still missing.
