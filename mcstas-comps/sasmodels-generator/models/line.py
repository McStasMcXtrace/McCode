r"""
This model calculates intensity using simple linear function

Definition
----------

The scattering intensity $I(q)$ is calculated as

.. math::

    I(q) = \text{scale} (A + B \cdot q) + \text{background}

.. note::
    For 2D plots intensity has different definition than other shape
    independent models

.. math::

    I(q) = \text{scale} (I(qx) \cdot I(qy)) + \text{background}

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

name = "line"
title = "Line model"
description = """\
      I(q) = A + B*q

      List of default parameters:
      A = intercept
      B = slope
      """
category = "shape-independent"

# pylint: disable=bad-whitespace, line-too-long
#             ["name", "units", default, [lower, upper], "type", "description"],
parameters = [["intercept", "1/cm",   1.0, [-inf, inf], "", "intercept in linear model"],
              ["slope",     "Ang/cm", 1.0, [-inf, inf], "", "slope in linear model"],
             ]
# pylint: enable=bad-whitespace, line-too-long

def Iq(q, intercept, slope):
    """
    :param q:           Input q-value
    :param intercept:   Intrecept in linear model
    :param slope:       Slope in linear model
    :return:            Calculated Intensity
    """
    inten = intercept + slope*q
    return inten

Iq.vectorized = True # Iq accepts an array of q values


def Iqxy(qx, qy, intercept, slope):
    """
    :param qx:   Input q_x-value
    :param qy:   Input q_y-value
    :param intercept:   Intrecept in linear model
    :param slope:       Slope in linear model
    :return:     2D-Intensity
    """
    # TODO: SasView documents 2D intensity as Iq(qx)*Iq(qy), but returns Iq(qy)
    # Note: SasView.run([r, theta]) does return Iq(qx)*Iq(qy)
    return Iq(qx, intercept, slope)*Iq(qy, intercept, slope)

Iqxy.vectorized = True  # Iqxy accepts an array of qx qy values

# uncomment the following to test Iqxy in C models
#del Iq, Iqxy
#c_code = """
#static double Iq(double q, double b, double m) { return m*q+b; }
#static double Iqxy(double qx, double qy, double b, double m)
#{ return (m*qx+b)*(m*qy+b); }
#"""

def random():
    """Return a random parameter set for the model."""
    scale = 10**np.random.uniform(0, 3)
    slope = np.random.uniform(-1, 1)*1e2
    offset = 1e-5 + (0 if slope > 0 else -slope)
    intercept = 10**np.random.uniform(0, 1) + offset
    pars = dict(
        #background=0,
        scale=scale,
        slope=slope,
        intercept=intercept,
    )
    return pars

tests = [
    [{'intercept': 1.0, 'slope': 1.0, }, 1.0, 2.001],
    [{'intercept': 1.0, 'slope': 1.0, }, 0.0, 1.001],
    [{'intercept': 1.0, 'slope': 1.0, }, 0.4, 1.401],
    [{'intercept': 1.0, 'slope': 1.0, }, 1.3, 2.301],
    [{'intercept': 1.0, 'slope': 1.0, }, 0.5, 1.501],
    [{'intercept': 1.0, 'slope': 1.0, }, [0.4, 0.5], [1.401, 1.501]],
    [{'intercept': 1.0, 'slope': 1.0, 'background': 0.0, }, (1.3, 1.57), 5.911],
]
