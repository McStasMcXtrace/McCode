r"""
Definition
----------

The form factor for this bent disc is essentially that of a hyperbolic
paraboloid and calculated as

.. math::

    P(q) = (\Delta \rho )^2 V \int^{\pi/2}_0 d\psi \sin{\psi} sinc^2
    \left( \frac{qd\cos{\psi}}{2} \right)
    \left[ \left( S^2_0+C^2_0\right) + 2\sum_{n=1}^{\infty}
     \left( S^2_n+C^2_n\right) \right]

where

.. math::

    C_n = \frac{1}{r^2}\int^{R}_{0} r dr\cos(qr^2\alpha \cos{\psi})
    J_n\left( qr^2\beta \cos{\psi}\right)
    J_{2n}\left( qr \sin{\psi}\right)

.. math::

    S_n = \frac{1}{r^2}\int^{R}_{0} r dr\sin(qr^2\alpha \cos{\psi})
    J_n\left( qr^2\beta \cos{\psi}\right)
    J_{2n}\left( qr \sin{\psi}\right)

and $\Delta\rho\text{ is }\rho_{pringle}-\rho_{solvent}$, $V$ is the volume of
the disc, $\psi$ is the angle between the normal to the disc and the q vector,
$d$ and $R$ are the "pringle" thickness and radius respectively, $\alpha$ and
$\beta$ are the two curvature parameters, and $J_n$ is the n\ :sup:`th` order
Bessel function of the first kind.

.. figure:: img/pringles_fig1.png

    Schematic of model shape (Graphic from Matt Henderson, matt@matthen.com)

Reference
---------

#. Karen Edler, Universtiy of Bath, Private Communication. 2012.
   Derivation by Stefan Alexandru Rautu.
#. L. Onsager, *Ann. New York Acad. Sci.*, 51 (1949) 627-659

Authorship and Verification
----------------------------

* **Author:** Andrew Jackson **Date:** 2008
* **Last Modified by:** Wojciech Wpotrzebowski **Date:** March 20, 2016
* **Last Reviewed by:** Andrew Jackson **Date:** September 26, 2016
"""

import numpy as np
from numpy import inf

name = "pringle"
title = "The Pringle model provides the form factor, $P(q)$, for a 'pringle' \
or 'saddle-shaped' disc that is bent in two directions."
description = """\

"""
category = "shape:cylinder"

# pylint: disable=bad-whitespace, line-too-long
#   ["name", "units", default, [lower, upper], "type","description"],
parameters = [
    ["radius",      "Ang",         60.0,   [0, inf],    "volume", "Pringle radius"],
    ["thickness",   "Ang",         10.0,   [0, inf],    "volume", "Thickness of pringle"],
    ["alpha",       "",            0.001,  [-inf, inf], "volume", "Curvature parameter alpha"],
    ["beta",        "",            0.02,   [-inf, inf], "volume", "Curvature paramter beta"],
    ["sld", "1e-6/Ang^2",  1.0,    [-inf, inf], "sld", "Pringle sld"],
    ["sld_solvent", "1e-6/Ang^2",  6.3,    [-inf, inf], "sld", "Solvent sld"]
    ]
# pylint: enable=bad-whitespace, line-too-long


source = ["lib/polevl.c", "lib/sas_J0.c", "lib/sas_J1.c",
          "lib/sas_JN.c", "lib/gauss76.c", "pringle.c"]
radius_effective_modes = [
    "equivalent cylinder excluded volume",
    "equivalent volume sphere",
    "radius"]

def random():
    """Return a random parameter set for the model."""
    alpha, beta = 10**np.random.uniform(-1, 1, size=2)
    radius = 10**np.random.uniform(1, 3)
    thickness = 10**np.random.uniform(0.7, 2)
    pars = dict(
        radius=radius,
        thickness=thickness,
        alpha=alpha,
        beta=beta,
    )
    return pars

tests = [
    [{'scale' : 1.0,
      'radius': 60.0,
      'thickness': 10.0,
      'alpha': 0.001,
      'beta': 0.02,
      'sld': 1.0,
      'sld_solvent': 6.3,
      'background': 0.001,
     }, 0.1, 9.87676],

    [{'scale' : 1.0,
      'radius': 60.0,
      'thickness': 10.0,
      'alpha': 0.001,
      'beta': 0.02,
      'sld': 1.0,
      'sld_solvent': 6.3,
      'background': 0.001,
     }, 0.01, 290.56723],

    [{'scale' : 1.0,
      'radius': 60.0,
      'thickness': 10.0,
      'alpha': 0.001,
      'beta': 0.02,
      'sld': 1.0,
      'sld_solvent': 6.3,
      'background': 0.001,
     }, 0.001, 317.40847],
]
