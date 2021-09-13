# Parked components and instruments
In this folder, we store instruments and components that rely on
features not yet available in the mcstas-3.x series

## *Scatter_log* and **shielding** instruments / components
The scatter logger and shielding mechanisms redefine the SCATTER keyword, at
present we do not know how to implement this feature in McStas 3.

## Monitor_Sqw.comp + Test_Monitor_Sqw.instr
The monitor uses Montior_nD uservars in a "dynamic" fashion, currently
not available in McStas 3. Compont-driven USERVARS would likely allow
some or all neede features. For now, please use Sqw_monitor instead.

## Test_Magnetic_Userdefined.instr
For now, user-written magnetic field functions are not available in
McStas 3. Most usecases should however be covered with what is
available in the ```Pol_Bfield ``` and ```Pol_tabled_field```
components.

## Test_shellguides.instr
This instrument and the related components have not been ported to
McStas 3. Ideally they should be rewritten, taking advantage of the
functionalities of McStas 3.

## Test_Single_crystal_inelastic.instr
It should be straightforward to port
```Single_crystal_inelastic.comp``` to McStas 3, following the
modifications done to ```Single_crystal.comp``` and
```Isotropic_Sqw.comp```. ```E_4PI.comp``` is a monitor and should be
easy to fix.

## Test_single_magnetic_crystal.instr
It should be straightforward to port
```Single_magnetic_crystal.comp``` to McStas 3, following the
modifications done to ```Single_crystal.comp```

## FZJ_BenchmarkSfin2.instr and related SANS_benchmark2.comp
The component needs lots of work on variable structure etc. to allow
use with McStas 3. One of the models from component has been ported to
the ```SANS_spheres2.comp``` which works with McStas 3.

## MCPL_merge.instr
The instrument reads / writes from / to MCPL-files within TRACE. This
is not possible on GPU.

