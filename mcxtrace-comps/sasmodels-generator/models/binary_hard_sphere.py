r"""
Definition
----------

The binary hard sphere model provides the scattering intensity, for binary
mixture of hard spheres including hard sphere interaction between those
particles, using rhw Percus-Yevick closure. The calculation is an exact
multi-component solution that properly accounts for the 3 partial structure
factors as follows:

.. math::

    I(q) = (1-x)f_1^2(q) S_{11}(q) + 2[x(1-x)]^{1/2} f_1(q)f_2(q)S_{12}(q) +
    x\,f_2^2(q)S_{22}(q)

where $S_{ij}$ are the partial structure factors and $f_i$ are the scattering
amplitudes of the particles. The subscript 1 is for the smaller particle and 2
is for the larger. The number fraction of the larger particle,
($x = n2/(n1+n2)$, where $n$ = the number density) is internally calculated
based on the diameter ratio and the volume fractions.

.. math::
    :nowrap:

    \begin{align*}
    x &= \frac{(\phi_2 / \phi)\alpha^3}{(1-(\phi_2/\phi) + (\phi_2/\phi)
    \alpha^3)} \\
    \phi &= \phi_1 + \phi_2 = \text{total volume fraction} \\
    \alpha &= R_1/R_2 = \text{size ratio}
    \end{align*}

The 2D scattering intensity is the same as 1D, regardless of the orientation of
the *q* vector which is defined as

.. math::

    q = \sqrt{q_x^2 + q_y^2}


**NOTE 1:** The volume fractions and the scattering contrasts are loosely
correlated, so holding as many parameters fixed to known values during fitting
will improve the robustness of the fit.

**NOTE 2:** Since the calculation uses the Percus-Yevick closure, all of the
limitations of that closure relation apply here. Specifically, one should be
wary of results for (total) volume fractions greater than approximately 40%.
Depending on the size ratios or number fractions, the limit on total volume
fraction may be lower.

**NOTE 3:** The heavy arithmetic operations also mean that at present the
function is poorly behaved at very low qr.  In some cases very large qr may
also be poorly behaved.  These should however be outside any useful region of
qr.

The code for this model is based originally on a c-library implementation by the
NIST Center for Neutron Research (Kline, 2006).

See the references for details.

References
----------

#. N W Ashcroft and D C Langreth, *Physical Review*, 156 (1967) 685-692
   [Errata found in *Phys. Rev.* 166 (1968) 934]

#. S R Kline, *J Appl. Cryst.*, 39 (2006) 895

Authorship and Verification
----------------------------

* **Author:** NIST IGOR/DANSE **Date:** pre 2010
* **Last Modified by:** Paul Butler **Date:** March 20, 2016
* **Last Reviewed by:** Paul Butler **Date:** March 20, 2016
"""

import numpy as np
from numpy import inf

category = "shape:sphere"
single = False  # double precision only!

name = "binary_hard_sphere"
title = "binary mixture of hard spheres with hard sphere interactions."
description = """Describes the scattering from a mixture of two distinct
monodisperse, hard sphere particles.
        [Parameters];
        radius_lg: large radius of binary hard sphere,
        radius_sm: small radius of binary hard sphere,
        volfraction_lg: volume fraction of large spheres,
        volfraction_sm: volume fraction of small spheres,
        sld_lg: large sphere  scattering length density,
        sld_sm: small sphere scattering length density,
        sld_solvent: solvent scattering length density.
"""
#             ["name", "units", default, [lower, upper], "type", "description"],
parameters = [["radius_lg", "Ang", 100, [0, inf], "",
               "radius of large particle"],
              ["radius_sm", "Ang", 25, [0, inf], "",
               "radius of small particle"],
              ["volfraction_lg", "", 0.1, [0, 1], "",
               "volume fraction of large particle"],
              ["volfraction_sm", "", 0.2, [0, 1], "",
               "volume fraction of small particle"],
              ["sld_lg", "1e-6/Ang^2", 3.5, [-inf, inf], "sld",
               "scattering length density of large particle"],
              ["sld_sm", "1e-6/Ang^2", 0.5, [-inf, inf], "sld",
               "scattering length density of small particle"],
              ["sld_solvent", "1e-6/Ang^2", 6.36, [-inf, inf], "sld",
               "Solvent scattering length density"],
             ]

source = ["lib/sas_3j1x_x.c", "binary_hard_sphere.c"]

def random():
    """Return a random parameter set for the model."""
    # TODO: binary_hard_sphere fails at low qr
    radius_lg = 10**np.random.uniform(2, 4.7)
    radius_sm = 10**np.random.uniform(2, 4.7)
    volfraction_lg = 10**np.random.uniform(-3, -0.3)
    volfraction_sm = 10**np.random.uniform(-3, -0.3)
    # TODO: Get slightly different results if large and small are swapped
    # modify the model so it doesn't care which is which
    if radius_lg < radius_sm:
        radius_lg, radius_sm = radius_sm, radius_lg
        volfraction_lg, volfraction_sm = volfraction_sm, volfraction_lg
    pars = dict(
        radius_lg=radius_lg,
        radius_sm=radius_sm,
        volfraction_lg=volfraction_lg,
        volfraction_sm=volfraction_sm,
    )
    return pars

# NOTE: test results taken from values returned by SasView 3.1.2
tests = [[{}, 0.001, 25.8927262013]]
