#power_law model
#conversion of PowerLawAbsModel.py
#converted by Steve King, Dec 2015

r"""
This model calculates a simple power law with a flat background.

Definition
----------

.. math::

    I(q) = \text{scale} \cdot q^{-\text{power}} + \text{background}

Note the minus sign in front of the exponent. The exponent *power*
should therefore be entered as a **positive** number for fitting.

Also note that unlike many other models, *scale* in this model
is NOT explicitly related to a volume fraction. Be careful if
combining this model with other models.


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
from numpy import inf, errstate

name = "power_law"
title = "Simple power law with a flat background"

description = """
    Evaluates the function
    I(q) = scale * q^(-power) + background
    NB: enter power as a positive number!
    """
category = "shape-independent"

#             ["name", "units", default, [lower, upper], "type", "description"],
parameters = [["power", "", 4.0, [-inf, inf], "", "Power law exponent"]]

# NB: Scale and Background are implicit parameters on every model
def Iq(q, power):
    # pylint: disable=missing-docstring
    with errstate(divide='ignore'):
        result = q**-power
    return result
Iq.vectorized = True  # Iq accepts an array of q values

def random():
    """Return a random parameter set for the model."""
    power = np.random.uniform(1, 6)
    pars = dict(
        scale=0.1**power*10**np.random.uniform(-4, 2),
        power=power,
    )
    return pars

tests = [
    [{'scale': 1.0, 'power': 4.0, 'background' : 0.0},
     [0.0106939, 0.469418], [7.64644e+07, 20.5949]],
    ]
