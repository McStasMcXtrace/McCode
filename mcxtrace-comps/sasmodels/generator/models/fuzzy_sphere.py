r"""
For information about polarised and magnetic scattering, see
the :ref:`magnetism` documentation.

Definition
----------

The scattering intensity $I(q)$ is calculated as:

.. math::

    I(q) = \frac{\text{scale}}{V}(\Delta \rho)^2 A^2(q) S(q)
           + \text{background}


where the amplitude $A(q)$ is given as the typical sphere scattering convoluted
with a Gaussian to get a gradual drop-off in the scattering length density:

.. math::

    A(q) = \frac{3\left[\sin(qR) - qR \cos(qR)\right]}{(qR)^3}
           \exp\left(\frac{-(\sigma_\text{fuzzy}q)^2}{2}\right)

Here $A(q)^2$ is the form factor, $P(q)$. The scale is equivalent to the
volume fraction of spheres, each of volume, $V$. Contrast $(\Delta \rho)$
is the difference of scattering length densities of the sphere and the
surrounding solvent.

Poly-dispersion in radius and in fuzziness is provided for, though the
fuzziness must be kept much smaller than the sphere radius for meaningful
results.

From the reference:

  The "fuzziness" of the interface is defined by the parameter
  $\sigma_\text{fuzzy}$. The particle radius $R$ represents the radius of the
  particle where the scattering length density profile decreased to 1/2 of the
  core density. $\sigma_\text{fuzzy}$ is the width of the smeared particle
  surface; i.e., the standard deviation from the average height of the fuzzy
  interface. The inner regions of the microgel that display a higher density
  are described by the radial box profile extending to a radius of
  approximately $R_\text{box} \sim R - 2 \sigma$. The profile approaches
  zero as $R_\text{sans} \sim R + 2\sigma$.

For 2D data: The 2D scattering intensity is calculated in the same way as 1D,
where the $q$ vector is defined as

.. math:: q = \sqrt{{q_x}^2 + {q_y}^2}

References
----------

#. M Stieger, J. S Pedersen, P Lindner, W Richtering,
   *Langmuir*, 20 (2004) 7283-7292

Authorship and Verification
----------------------------

* **Author:**
* **Last Modified by:**
* **Last Reviewed by:**
"""

import numpy as np
from numpy import inf

name = "fuzzy_sphere"
title = "Scattering from spherical particles with a fuzzy surface."
description = """\
scale: scale factor times volume fraction,
or just volume fraction for absolute scale data
radius: radius of the solid sphere
fuzziness = the standard deviation of the fuzzy interfacial
thickness (ie., so-called interfacial roughness)
sld: the SLD of the sphere
solvend_sld: the SLD of the solvent
background: incoherent background
Note: By definition, this function works only when fuzziness << radius.
"""
category = "shape:sphere"

# pylint: disable=bad-whitespace,line-too-long
# ["name", "units", default, [lower, upper], "type","description"],
parameters = [["sld",         "1e-6/Ang^2",  1, [-inf, inf], "sld",    "Particle scattering length density"],
              ["sld_solvent", "1e-6/Ang^2",  3, [-inf, inf], "sld",    "Solvent scattering length density"],
              ["radius",      "Ang",        60, [0, inf],    "volume", "Sphere radius"],
              ["fuzziness",   "Ang",        10, [0, inf],    "volume",       "std deviation of Gaussian convolution for interface (must be << radius)"],
             ]
# pylint: enable=bad-whitespace,line-too-long

source = ["lib/sas_3j1x_x.c", "fuzzy_sphere.c"]
have_Fq = True
radius_effective_modes = ["radius", "radius + fuzziness"]

def random():
    """Return a random parameter set for the model."""
    radius = 10**np.random.uniform(1, 4.7)
    fuzziness = 10**np.random.uniform(-2, -0.5)*radius  # 1% to 31% fuzziness
    pars = dict(
        radius=radius,
        fuzziness=fuzziness,
    )
    return pars

tests = [
    # Accuracy tests based on content in test/utest_models_new1_3.py
    #[{'background': 0.001}, 1.0, 0.001],

    [{}, 0.00301005, 359.2315],

    ]
