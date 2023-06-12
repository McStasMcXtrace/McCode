r"""
Definition
----------

The scattering intensity $I(q)$ is calculated as

.. math::

    I(q) = \begin{cases}
    A q^{-m1} + \text{background} & q <= q_c \\
    C q^{-m2} + \text{background} & q > q_c
    \end{cases}

where $q_c$ = the location of the crossover from one slope to the other,
$A$ = the scaling coefficient that sets the overall intensity of the lower Q
power law region, $m1$ = power law exponent at low Q, and $m2$ = power law
exponent at high Q.  The scaling of the second power law region (coefficient C)
is then automatically scaled to match the first by following formula:

.. math::
    C = \frac{A q_c^{m2}}{q_c^{m1}}

.. note::
    Be sure to enter the power law exponents as positive values!

For 2D data the scattering intensity is calculated in the same way as 1D,
where the $q$ vector is defined as

.. math::

    q = \sqrt{q_x^2 + q_y^2}


References
----------

None.

Authorship and Verification
----------------------------

* **Author:** NIST IGOR/DANSE **Date:** pre 2010
* **Last Modified by:** Wojciech Wpotrzebowski **Date:** February 18, 2016
* **Last Reviewed by:** Paul Butler **Date:** March 21, 2016
"""

import numpy as np
from numpy import inf, power, empty, errstate

name = "two_power_law"
title = "This model calculates an empirical functional form for SAS data \
characterized by two power laws."
description = """
            I(q) = coef_A*pow(qval,-1.0*power1) + background for q<=q_c
            =C*pow(qval,-1.0*power2) + background for q>q_c
            where C=coef_A*pow(q_c,-1.0*power1)/pow(q_c,-1.0*power2).

            coef_A = scaling coefficient
            q_c = crossover location [1/A]
            power_1 (=m1) = power law exponent at low Q
            power_2 (=m2) = power law exponent at high Q
            background = Incoherent background [1/cm]
        """
category = "shape-independent"

# pylint: disable=bad-whitespace, line-too-long
#   ["name", "units", default, [lower, upper], "type", "description"],
parameters = [
    ["coefficent_1", "",       1.0, [-inf, inf], "", "coefficent A in low Q region"],
    ["crossover",    "1/Ang",  0.04,[0, inf],    "", "crossover location"],
    ["power_1",      "",       1.0, [0, inf],    "", "power law exponent at low Q"],
    ["power_2",      "",       4.0, [0, inf],    "", "power law exponent at high Q"],
    ]
# pylint: enable=bad-whitespace, line-too-long


def Iq(q,
       coefficent_1=1.0,
       crossover=0.04,
       power_1=1.0,
       power_2=4.0,
      ):
    """
    :param q:                   Input q-value (float or [float, float])
    :param coefficent_1:        Scaling coefficent at low Q
    :param crossover:           Crossover location
    :param power_1:             Exponent of power law function at low Q
    :param power_2:             Exponent of power law function at high Q
    :return:                    Calculated intensity
    """
    result = empty(q.shape, 'd')
    index = (q <= crossover)
    with errstate(divide='ignore'):
        coefficent_2 = coefficent_1 * power(crossover, power_2 - power_1)
        result[index] = coefficent_1 * power(q[index], -power_1)
        result[~index] = coefficent_2 * power(q[~index], -power_2)
    return result

Iq.vectorized = True  # Iq accepts an array of q values

def random():
    """Return a random parameter set for the model."""
    coefficient_1 = 1
    crossover = 10**np.random.uniform(-3, -1)
    power_1 = np.random.uniform(1, 6)
    power_2 = np.random.uniform(1, 6)
    pars = dict(
        scale=1, #background=0,
        coefficient_1=coefficient_1,
        crossover=crossover,
        power_1=power_1,
        power_2=power_2,
    )
    return pars

tests = [
    # Accuracy tests based on content in test/utest_extra_models.py
    [{'coefficent_1':     1.0,
      'crossover':  0.04,
      'power_1':    1.0,
      'power_2':    4.0,
      'background': 0.0,
     }, 0.001, 1000],

    [{'coefficent_1':     1.0,
      'crossover':  0.04,
      'power_1':    1.0,
      'power_2':    4.0,
      'background': 0.0,
     }, 0.150141, 0.125945],

    [{'coefficent_1':    1.0,
      'crossover':  0.04,
      'power_1':    1.0,
      'power_2':    4.0,
      'background': 0.0,
     }, 0.442528, 0.00166884],

    [{'coefficent_1':    1.0,
      'crossover':  0.04,
      'power_1':    1.0,
      'power_2':    4.0,
      'background': 0.0,
     }, (0.442528, 0.00166884), 0.00166884],

]
