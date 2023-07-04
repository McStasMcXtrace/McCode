r"""
This model provides the form factor for $N$ spherical pearls of radius $R$
linearly joined by short strings (or segment length or edge separation)
$l$ $(= A - 2R)$. $A$ is the center-to-center pearl separation distance.
The thickness of each string is assumed to be negligible.

.. figure:: img/linear_pearls_geometry.jpg


Definition
----------

The output of the scattering intensity function for the linear_pearls model
is given by (Dobrynin, 1996)

.. math::

    P(Q) = \frac{\text{scale}}{V}\left[ m_{p}^2
    \left(N+2\sum_{n-1}^{N-1}(N-n)\frac{\sin(qnl)}{qnl}\right)
    \left( 3\frac{\sin(qR)-qR\cos(qR)}{(qr)^3}\right)^2\right]

where the mass $m_p$ is $(SLD_{pearl}-SLD_{solvent})*(volume\ of\ N\ pearls)$.
V is the total volume.

The 2D scattering intensity is the same as P(q) above,
regardless of the orientation of the q vector.

References
----------

#.  A V Dobrynin, M Rubinstein and S P Obukhov, *Macromol.*, 29 (1996) 2974-2979

Authorship and Verification
----------------------------

* **Author:**
* **Last Modified by:**
* **Last Reviewed by:**
"""

import numpy as np
from numpy import inf

name = "linear_pearls"
title = "Linear pearls model of scattering from spherical pearls."
description = """
    Calculate form factor for Pearl Necklace Model
    [Macromol. 1996, 29, 2974-2979]
    Parameters:

    sld_pearl: the SLD of the pearl spheres
    sld_solv: the SLD of the solvent
    num_pearls: number of the pearls
    radius: the radius of a pearl
    edge_separation: the length of string segment; surface to surface
    """
category = "shape:sphere"

# pylint: disable=bad-whitespace, line-too-long
#            ["name", "units", default, [lower, upper], "type", "description"],
parameters = [
    ["radius",      "Ang",       80.0, [0, inf],     "", "Radius of the pearls"],
    ["edge_sep",    "Ang",      350.0, [0, inf],     "", "Length of the string segment - surface to surface"],
    ["num_pearls",  "",           3.0, [1, inf],     "", "Number of the pearls"],
    ["sld",   "1e-6/Ang^2", 1.0, [-inf, inf],  "sld", "SLD of the pearl spheres"],
    ["sld_solvent", "1e-6/Ang^2", 6.3, [-inf, inf],  "sld", "SLD of the solvent"],
    ]
# pylint: enable=bad-whitespace, line-too-long
single = False

source = ["lib/sas_3j1x_x.c", "linear_pearls.c"]

def random():
    """Return a random parameter set for the model."""
    radius = 10**np.random.uniform(1, 3) # 1 - 1000
    edge_sep = 10**np.random.uniform(0, 3)  # 1 - 1000
    num_pearls = np.round(10**np.random.uniform(0.3, 3)) # 2 - 1000
    pars = dict(
        radius=radius,
        edge_sep=edge_sep,
        num_pearls=num_pearls,
    )
    return pars

_ = """
Tests temporarily disabled, until single-double precision accuracy issue solved.

tests = [
    # Accuracy tests based on content in test/utest_model_pearlnecklace.py
    [{'radius':      20.0,
      'num_pearls':   2.0,
      'sld':    1.0,
      'sld_solvent':  6.3,
      'edge_sep':   400.0,
     }, 0.001, 185.135],

    # Additional tests with larger range of parameters
    [{'radius':     120.0,
      'num_pearls':   5.0,
      'sld':    2.0,
      'sld_solvent':  2.3,
      'edge_sep':   100.0,
     }, 0.01, 45.4984],

    [{'radius':       7.0,
      'num_pearls':   2.0,
      'sld':   10.0,
      'sld_solvent': 99.3,
      'edge_sep':    20.0,
     }, 1.0, 0.632811],
    ]
"""
