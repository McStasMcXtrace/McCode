    This file is part of the McStas neutron ray-trace simulation package
    Copyright (C) 1997-, All rights reserved
    Technical University of Denmark, plus collaboration including
    Institut Laue Langevin, Grenoble, France
    
    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; version 2 of the License.
    
    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.
    
    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

# McStas component nomenclature
This file describes 'standard' McStas parameter names for components.

## Geometry and Dimensions:
Use combinations of the below identifiers to realise simple
geometrical shapes, such as spheres, boxes and cylinders.

For hollow shapes, combine with ```thickness``` or ```radius_i/radius_o```

* Radial specifications (suffixes _i and _o indicate inner, outer respectively)
  ```radius, radius_i, radius_o```
* Dimensions along x
   ```xwidth, xmin, xmax```
 * Dimensions along y
   ```yheight, ymin, ymax```
* Dimensions along z
  ```zdepth, zlength, zmin, zmax, l, length```
* Material thickness, e.g. for hollow shapes
  ```thickness```
* Width, height and length specifications for guides and other optics
  ```w1, h1``` at guide entry,  ```w2, h2```at guide exit, ```l, length``` for z-length
* Reflectivity parameters for guides and other optics
  ```R0 Qc alpha m W```
* For multichannel optics and collimators, used to specify number of channels
  ```nslit```
* Components that support the OFF surface specification often use
  ```geometry``` to specify a file with vertices and faces

## Spectral definitions for source (often defines the MC sampling range
* Wavelength
  ```Lmin, Lmax``` or ```Lambda0, dLambda``` or ```lambda0, dlambda```
* Energy
  ```Emin, Emax``` or ```E0, dE```

## Physical parameters:
* Scalar cross-sections of absorption, incoherent and corherent
scattering
  ```sigma_abs, sigma_inc, sigma_coh```
* Frequencies
  ```nu, freq```
* Phase-definitions, e.g. for choppers
  ```phase, phi```

## Monte Carlo variance-reduction and tuning-parameters:
* Propability of "interaction", often meaning scattering "including
change of direction" (i.e. 1-p_interact is "transmitted" or "tunneled"
through object)
  ```p_interact```
* Propability of incoherent scattering, transmission
  ```p_incoherent, p_transmit```
* Defining a solid-angle of interest, often used in sources and samples
  ```focus_xw, focus_yh``` for rectangular, ```focus_aw, focus_ah``` for angular , ```focus_r``` for radial specficiation
* to be combined with either
  ```dist``` for distance along z, ```target_index``` for  specification via location of other component object, ```target_x, target_y, target_z``` for specification via free-form vector
* In some samples, the scattered output can be limitied to an angular range
around the scattering plane
  ```d_phi```
* or horizontal range 
  ```d_omega```

## Binning definitions in monitors
Typically done with combination of limits and number of bins, examples
* ```Emin, Emax, nE``` in energy-monitors
* ```Lmin, Lmax, nL``` in wavelength monitors
* ```xwidth, yheight, nx, ny``` in position-monitors
* Also consult ```Monitor_nD``` documentation for the advanced ```options``` string

## Other:
```filename``` specifies a needed file, be it for input (physical parameters, geometry, ...) or output (in monitors and samples)
