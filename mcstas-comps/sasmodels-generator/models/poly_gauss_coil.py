#poly_gauss_coil model
#conversion of Poly_GaussCoil.py
#converted by Steve King, Mar 2016
r"""
This empirical model describes the scattering from *polydisperse* polymer
chains in theta solvents or polymer melts, assuming a Schulz-Zimm type
molecular weight distribution.

To describe the scattering from *monodisperse* polymer chains, see the
:ref:`mono-gauss-coil` model.

Definition
----------

.. math::

     I(q) = \text{scale} \cdot I_0 \cdot P(q) + \text{background}

where

.. math::

     I_0 &= \phi_\text{poly} \cdot V \cdot (\rho_\text{poly}-\rho_\text{solv})^2 \\
     P(q) &= 2 [(1 + UZ)^{-1/U} + Z - 1] / [(1 + U) Z^2] \\
     Z &= [(q R_g)^2] / (1 + 2U) \\
     U &= (Mw / Mn) - 1 = \text{polydispersity ratio} - 1 \\
     V &= M / (N_A \delta)

Here, $\phi_\text{poly}$, is the volume fraction of polymer, $V$ is the
volume of a polymer coil, $M$ is the molecular weight of the polymer,
$N_A$ is Avogadro's Number, $\delta$ is the bulk density of the polymer,
$\rho_\text{poly}$ is the sld of the polymer, $\rho_\text{solv}$ is the
sld of the solvent, and $R_g$ is the radius of gyration of the polymer coil.

The 2D scattering intensity is calculated in the same way as the 1D,
but where the $q$ vector is redefined as

.. math::

    q = \sqrt{q_x^2 + q_y^2}

References
----------

#. O Glatter and O Kratky (editors), *Small Angle X-ray Scattering*,
   Academic Press, (1982) Page 404
#. J S Higgins, H C Benoit, *Polymers and Neutron Scattering*,
   Oxford Science Publications, (1996)
#. S M King, *Small Angle Neutron Scattering*
   in *Modern Techniques for Polymer Characterisation*, Wiley, (1999)
#. http://www.ncnr.nist.gov/staff/hammouda/distance_learning/chapter_28.pdf

Authorship and Verification
----------------------------

* **Author:**
* **Last Modified by:**
* **Last Reviewed by:**
"""

import numpy as np
from numpy import inf, expm1, power

name = "poly_gauss_coil"
title = "Scattering from polydisperse polymer coils"

description = """
    Evaluates the scattering from
    polydisperse polymer chains.
    """
category = "shape-independent"

# pylint: disable=bad-whitespace, line-too-long
#   ["name", "units", default, [lower, upper], "type", "description"],
parameters = [
    ["i_zero",          "1/cm", 70.0, [0.0, inf], "", "Intensity at q=0"],
    ["rg",  "Ang", 75.0, [0.0, inf], "", "Radius of gyration"],
    ["polydispersity",  "None",  2.0, [1.0, inf], "", "Polymer Mw/Mn"],
    ]
# pylint: enable=bad-whitespace, line-too-long

# NB: Scale and Background are implicit parameters on every model
def Iq(q, i_zero, rg, polydispersity):
    # pylint: disable = missing-docstring
    u = polydispersity - 1.0
    z = q**2 * (rg**2 / (1.0 + 2.0*u))

    # need to trap the case of the polydispersity being 1 (ie, monodisperse!)
    if polydispersity == 1.0:
        result = 2.0 * (expm1(-z) + z)
        index = q != 0.
        result[index] /= z[index]**2
        result[~index] = 1.0
    else:
        # Taylor series around z=0 of (2*(1+uz)^(-1/u) + z - 1) / (z^2(u+1))
        p = [
            #(-1 - 20*u - 155*u**2 - 580*u**3 - 1044*u**4 - 720*u**5) / 2520.,
            #(+1 + 14*u + 71*u**2 + 154*u**3 + 120*u**4) / 360.,
            #(-1 - 9*u - 26*u**2 - 24*u**3) / 60.,
            (+1 + 5*u + 6*u**2) / 12.,
            (-1 - 2*u) / 3.,
            (+1),
            ]
        result = 2.0 * (power(1.0 + u*z, -1.0/u) + z - 1.0) / (1.0 + u)
        index = z > 1e-4
        result[index] /= z[index]**2
        result[~index] = np.polyval(p, z[~index])
    return i_zero * result
Iq.vectorized = True  # Iq accepts an array of q values

def random():
    """Return a random parameter set for the model."""
    rg = 10**np.random.uniform(0, 4)
    #rg = 1e3
    polydispersity = 10**np.random.uniform(0, 3)
    pars = dict(
        #scale=1, background=0,
        i_zero=1e7, # i_zero is a simple scale
        rg=rg,
        polydispersity=polydispersity,
    )
    return pars

# these unit test values taken from SasView 3.1.2
tests = [
    [{'scale': 1.0, 'i_zero': 70.0, 'rg': 75.0,
      'polydispersity': 2.0, 'background': 0.0},
     [0.0106939, 0.469418], [57.6405, 0.169016]],
    ]
