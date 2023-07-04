r"""
This model describes a Lorentzian shaped peak on a flat background.

Definition
----------

The scattering intensity $I(q)$ is calculated as

.. math::

    I(q) = \frac{scale}{\bigl(1+\bigl(\frac{q-q_0}{B}\bigr)^2\bigr)} + background

with the peak having height of $I_0$ centered at $q_0$ and having
a HWHM (half-width half-maximum) of B.

For 2D data the scattering intensity is calculated in the same way as 1D,
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

name = "peak_lorentz"
title = "A Lorentzian peak on a flat background"
description = """\
      Class that evaluates a Lorentzian shaped peak.

        F(q) = scale/(1+[(q-q0)/B]^2 ) + background

        The model has three parameters:
            scale     =  scale
            peak_pos        =  peak position
            peak_hwhm        =  half-width-half-maximum of peak
            background=  incoherent background"""

category = "shape-independent"

#             ["name", "units", default, [lower, upper], "type", "description"],
parameters = [["peak_pos", "1/Ang", 0.05, [-inf, inf], "", "Peak postion in q"],
              ["peak_hwhm", "1/Ang", 0.005, [-inf, inf], "", "HWHM of peak"],
             ]

def Iq(q, peak_pos, peak_hwhm):
    """
        Return I(q)
    """
    inten = (1/(1+((q-peak_pos)/peak_hwhm)**2))
    return inten
Iq.vectorized = True  # Iq accepts an array of q values

def random():
    """Return a random parameter set for the model."""
    peak_pos = 10**np.random.uniform(-3, -1)
    peak_hwhm = peak_pos * 10**np.random.uniform(-3, 0)
    pars = dict(
        #background=0,
        scale=10**np.random.uniform(2, 6),
        peak_pos=peak_pos,
        peak_hwhm=peak_hwhm,
    )
    return pars

tests = [[{'scale':100.0, 'background':1.0}, 0.001, 2.0305]]
