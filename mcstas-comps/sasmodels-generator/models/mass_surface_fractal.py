r"""

A number of natural and commercial processes form high-surface area materials
as a result of the vapour-phase aggregation of primary particles.
Examples of such materials include soots, aerosols, and fume or pyrogenic
silicas. These are all characterised by cluster mass distributions (sometimes
also cluster size distributions) and internal surfaces that are fractal in
nature. The scattering from such materials displays two distinct breaks in
log-log representation, corresponding to the radius-of-gyration of the primary
particles, $rg$, and the radius-of-gyration of the clusters (aggregates),
$Rg$. Between these boundaries the scattering follows a power law related to
the mass fractal dimension, $Dm$, whilst above the high-Q boundary the
scattering follows a power law related to the surface fractal dimension of
the primary particles, $Ds$.

Definition
----------

The scattered intensity I(q) is calculated using a modified
Ornstein-Zernicke equation

.. math::

    I(q) &= \text{scale} \times P(q) + \text{background} \\
    P(q) &= \left\{ \left[ 1+(q^2a)\right]^{D_m/2} \times
                   \left[ 1+(q^2b)\right]^{(6-D_s-D_m)/2}
           \right\}^{-1} \\
    a &= R_{g}^2/(3D_m/2) \\
    b &= r_{g}^2/[-3(D_s+D_m-6)/2] \\
    \text{scale} &= \text{scale factor}
        \times NV^2 (\rho_\text{particle} - \rho_\text{solvent})^2

where $R_g$ is the size of the cluster, $r_g$ is the size of the primary
particle, $D_s$ is the surface fractal dimension, $D_m$ is the mass fractal
dimension, $\rho_\text{solvent}$ is the scattering length density of the
solvent, and $\rho_\text{particle}$ is the scattering length density of
particles.

.. note::

    The surface ( $D_s$ ) and mass ( $D_m$ ) fractal dimensions are only
    valid if $0 < surface\_dim < 6$, $0 < mass\_dim < 6$, and
    $(surface\_dim + mass\_dim ) < 6$.
    Older versions of sasview may have the default primary particle radius
    larger than the cluster radius, this was an error, also present in the
    Schmidt review paper below. The primary particle should be the smaller
    as described in the original Hurd, et al., who also point out that
    polydispersity in the primary particle sizes may affect their
    apparent surface fractal dimension.


References
----------

#. P Schmidt, *J Appl. Cryst.*, 24 (1991) 414-435 Equation(19)
#. A J Hurd, D W Schaefer, J E Martin, *Phys. Rev. A*,
   35 (1987) 2361-2364 Equation(2)

Authorship and Verification
----------------------------

* **Author:** Piotr Rozyczko **Date:** Jan 20, 2016
* **Last Modified by:**
* **Last Reviewed by:** Richard Heenan **Date:** May 30, 2018
"""

import numpy as np
from numpy import inf

name = "mass_surface_fractal"
title = "Mass Surface Fractal model"
description = """
        The scattering intensity  I(x) = scale*P(x)*S(x) + background, where
        p(x)= {[1+(x^2*a)]^(Dm/2) * [1+(x^2*b)]^(6-Ds-Dm)/2}^(-1)
        a = Rg^2/(3*Dm/2)
        b = rg^2/(3*(6-Ds-Dm)/2)
        scale        =  scale factor * N*Volume^2*contrast^2
        fractal_dim_mass       =  Dm (mass fractal dimension)
        fractal_dim_surf  =  Ds
        rg_cluster  =  Rg
        rg_primary    =  rg
        background   =  background
        Hurd, Schaefer, Martin, Phys Rev A, eq(2),(1987),35, 2361-2364
        Note that 0 < Ds< 6 and 0 < Dm < 6.
        """
category = "shape-independent"

# pylint: disable=bad-whitespace, line-too-long
#   ["name", "units", default, [lower, upper], "type","description"],
parameters = [
    ["fractal_dim_mass", "",      1.8, [0.0, 6.0], "", "Mass fractal dimension"],
    ["fractal_dim_surf", "",      2.3, [0.0, 6.0], "", "Surface fractal dimension"],
    ["rg_cluster",       "Ang", 4000., [0.0, inf], "", "Cluster radius of gyration"],
    ["rg_primary",       "Ang",  86.7, [0.0, inf], "", "Primary particle radius of gyration"],
]
# pylint: enable=bad-whitespace, line-too-long

source = ["mass_surface_fractal.c"]
valid = "fractal_dim_mass + fractal_dim_surf <= 6.0"

def random():
    """Return a random parameter set for the model."""
    fractal_dim = np.random.uniform(0, 6)
    surface_portion = np.random.uniform(0, 1)
    fractal_dim_surf = fractal_dim*surface_portion
    fractal_dim_mass = fractal_dim - fractal_dim_surf
    rg_cluster = 10**np.random.uniform(1, 5)
    rg_primary = rg_cluster*10**np.random.uniform(-4, -1)
    scale = 10**np.random.uniform(2, 5)
    pars = dict(
        #background=0,
        scale=scale,
        fractal_dim_mass=fractal_dim_mass,
        fractal_dim_surf=fractal_dim_surf,
        rg_cluster=rg_cluster,
        rg_primary=rg_primary,
    )
    return pars


tests = [

    # Accuracy tests based on content in test/utest_other_models.py  All
    # except first, changed so rg_cluster is the larger, RKH 30 May 2018
    [{'fractal_dim_mass':   1.8,
      'fractal_dim_surf':   2.3,
      'rg_cluster':   86.7,
      'rg_primary': 4000.0,
      'background':    0.0,
     }, 0.05, 1.77537e-05],

    # Additional tests with larger range of parameters
    [{'fractal_dim_mass':      3.3,
      'fractal_dim_surf':   1.0,
      'rg_cluster': 4000.0,
      'rg_primary':   90.0,
     }, 0.001, 0.0932516614456],

    [{'fractal_dim_mass':      1.3,
      'fractal_dim_surf':   2.0,
      'rg_cluster': 2000.0,
      'rg_primary':   90.0,
      'background':    0.8,
     }, 0.001, 1.28296431786],

    [{'fractal_dim_mass':      2.3,
      'fractal_dim_surf':   3.1,
      'rg_cluster':  1000.0,
      'rg_primary':  30.0,
      'scale':        10.0,
      'background':    0.0,
     }, 0.051, 0.00333804044899],
    ]
