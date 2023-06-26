#mono_gauss_coil model
#conversion of DebyeModel.py
#converted by Steve King, Mar 2016
r"""
This Debye Gaussian coil model strictly describes the scattering from
*monodisperse* polymer chains in theta solvents or polymer melts, conditions
under which the distances between segments follow a Gaussian distribution.
Provided the number of segments is large (ie, high molecular weight polymers)
the single-chain form factor P(Q) is that described by Debye (1947).

To describe the scattering from *polydisperse* polymer chains see the
:ref:`poly-gauss-coil` model.

Definition
----------

.. math::

     I(q) = \text{scale} \cdot I_0 \cdot P(q) + \text{background}

where

.. math::

     I_0 &= \phi_\text{poly} \cdot V
            \cdot (\rho_\text{poly} - \rho_\text{solv})^2 \\
     P(q) &= 2 [\exp(-Z) + Z - 1] / Z^2 \\
     Z &= (q R_g)^2 \\
     V &= M / (N_A \delta)

Here, $\phi_\text{poly}$ is the volume fraction of polymer, $V$ is the
volume of a polymer coil, *M* is the molecular weight of the polymer,
$N_A$ is Avogadro's Number, $\delta$ is the bulk density of the polymer,
$\rho_\text{poly}$ is the sld of the polymer, $\rho\text{solv}$ is the
sld of the solvent, and $R_g$ is the radius of gyration of the polymer coil.

The 2D scattering intensity is calculated in the same way as the 1D,
but where the *q* vector is redefined as

.. math::

    q = \sqrt{q_x^2 + q_y^2}

References
----------

#.  P Debye, *J. Phys. Colloid. Chem.*, 51 (1947) 18.
#.  R J Roe, *Methods of X-Ray and Neutron Scattering in Polymer Science*,
    Oxford University Press, New York (2000).
#.  http://www.ncnr.nist.gov/staff/hammouda/distance_learning/chapter_28.pdf

Authorship and Verification
----------------------------

* **Author:**
* **Last Modified by:**
* **Last Reviewed by:**
"""

import numpy as np
from numpy import inf

name = "mono_gauss_coil"
title = "Scattering from monodisperse polymer coils"

description = """
    Evaluates the scattering from
    monodisperse polymer chains.
    """
category = "shape-independent"

# pylint: disable=bad-whitespace, line-too-long
#   ["name", "units", default, [lower, upper], "type", "description"],
parameters = [
    ["i_zero", "1/cm", 70.0, [0.0, inf], "", "Intensity at q=0"],
    ["rg", "Ang", 75.0, [0.0, inf], "volume", "Radius of gyration"],
    ]
# pylint: enable=bad-whitespace, line-too-long

source = ["mono_gauss_coil.c"]
have_Fq = False
radius_effective_modes = ["R_g", "2R_g", "3R_g", "sqrt(5/3)*R_g"]


def random():
    """Return a random parameter set for the model."""
    rg = 10**np.random.uniform(0, 4)
    #rg = 1e3
    pars = dict(
        #scale=1, background=0,
        i_zero=1e7, # i_zero is a simple scale
        rg=rg,
    )
    return pars

# these unit test values taken from SasView 3.1.2
tests = [
    [{'scale': 1.0, 'i_zero': 70.0, 'rg': 75.0, 'background': 0.0},
     [0.0106939, 0.469418], [57.1241, 0.112859]],
    ]
