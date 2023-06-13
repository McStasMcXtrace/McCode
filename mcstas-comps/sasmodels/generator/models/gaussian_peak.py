r"""

Definition
----------

This model describes a Gaussian shaped peak on a flat background

.. math::

    I(q) = (\text{scale}) \exp\left[ -\tfrac12 (q-q_0)^2 / \sigma^2 \right]
        + \text{background}

with the peak having height of *scale* centered at $q_0$ and having a standard
deviation of $\sigma$. The FWHM (full-width half-maximum) is $2.354 \sigma$.

For 2D data, scattering intensity is calculated in the same way as 1D,
where the $q$ vector is defined as

.. math::

    q = \sqrt{q_x^2 + q_y^2}


References
----------

None.

Authorship and Verification
----------------------------

* **Author:**
* **Last Modified by:**
* **Last Reviewed by:**
"""

import numpy as np
from numpy import inf

name = "gaussian_peak"
title = "Gaussian shaped peak"
description = """
    Model describes a Gaussian shaped peak including a flat background
    Provide F(q) = scale*exp( -1/2 *[(q-peak_pos)/sigma]^2 )+ background
"""
category = "shape-independent"

#             ["name", "units", default, [lower, upper], "type","description"],
parameters = [["peak_pos", "1/Ang", 0.05, [-inf, inf], "", "Peak position"],
              ["sigma", "1/Ang", 0.005, [0, inf], "",
               "Peak width (standard deviation)"],
             ]

source = ["gaussian_peak.c"]  

def random():
    """Return a random parameter set for the model."""
    peak_pos = 10**np.random.uniform(-3, -1)
    sigma = 10**np.random.uniform(-1.3, -0.3)*peak_pos
    scale = 10**np.random.uniform(0, 4)
    pars = dict(
        #background=1e-8,
        scale=scale,
        peak_pos=peak_pos,
        sigma=sigma,
    )
    return pars
