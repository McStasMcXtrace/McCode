r"""
This model calculates the scattering from a gel structure,
but typically a physical rather than chemical network.
It is modeled as a sum of a low-q exponential decay (which happens to
give a functional form similar to Guinier scattering, so interpret with
care) plus a Lorentzian at higher-q values. See also the gel_fit model.

Definition
----------

The scattering intensity $I(q)$ is calculated as (Eqn. 5 from the reference)

.. math:: I(q) = I_G(0) \exp(-q^2\Xi ^2/2) + I_L(0)/(1+q^2\xi^2)

$\Xi$ is the length scale of the static correlations in the gel, which can
be attributed to the "frozen-in" crosslinks. $\xi$ is the dynamic correlation
length, which can be attributed to the fluctuating polymer chains between
crosslinks. $I_G(0)$ and $I_L(0)$ are the scaling factors for each of these
structures. Think carefully about how these map to your particular system!

.. note::
    The peaked structure at higher $q$ values (Figure 2 from the reference)
    is not reproduced by the model. Peaks can be introduced into the model
    by summing this model with the :ref:`gaussian-peak` model.

For 2D data the scattering intensity is calculated in the same way as 1D,
where the $q$ vector is defined as

.. math:: q = \sqrt{q_x^2 + q_y^2}

References
----------

#. G Evmenenko, E Theunissen, K Mortensen, H Reynaers,
   *Polymer*, 42 (2001) 2907-2913

Authorship and Verification
----------------------------

* **Author:**
* **Last Modified by:**
* **Last Reviewed by:**
"""

import numpy as np
from numpy import inf, exp

name = "gauss_lorentz_gel"
title = "Gauss Lorentz Gel model of scattering from a gel structure"
description = """
            Class that evaluates a GaussLorentzGel model.

            I(q) = scale_g*exp(- q^2*Z^2 / 2)+scale_l/(1+q^2*z^2)
                    + background
            List of default parameters:
                scale_g = Gauss scale factor
                Z = Static correlation length
                scale_l = Lorentzian scale factor
                z = Dynamic correlation length
                background = Incoherent background
            """
category = "shape-independent"
# pylint: disable=bad-whitespace, line-too-long
#            ["name", "units", default, [lower, upper], "type", "description"],
parameters = [["gauss_scale",   "",    100.0,  [-inf, inf], "", "Gauss scale factor"],
              ["cor_length_static",    "Ang", 100.0,  [0, inf],    "", "Static correlation length"],
              ["lorentz_scale", "",     50.0,  [-inf, inf], "", "Lorentzian scale factor"],
              ["cor_length_dynamic",   "Ang",  20.0,  [0, inf],    "", "Dynamic correlation length"],
             ]
# pylint: enable=bad-whitespace, line-too-long

def Iq(q,
       gauss_scale=100.0,
       cor_length_static=100.0,
       lorentz_scale=50.0,
       cor_length_dynamic=20.0):
    """

    :param q:                    Input q-value
    :param gauss_scale:   Gauss scale factor
    :param cor_length_static:    Static correlation length
    :param lorentz_scale: Lorentzian scale factor
    :param cor_length_dynamic:   Dynamic correlation length
    :return:                     1-D intensity
    """

    term1 = gauss_scale *\
            exp(-1.0*q*q*cor_length_static*cor_length_static/2.0)
    term2 = lorentz_scale /\
            (1.0+(q*cor_length_dynamic)*(q*cor_length_dynamic))

    return term1 + term2

Iq.vectorized = True  # Iq accepts an array of q values


def random():
    """Return a random parameter set for the model."""
    gauss_scale = 10**np.random.uniform(1, 3)
    lorentz_scale = 10**np.random.uniform(1, 3)
    cor_length_static = 10**np.random.uniform(0, 3)
    cor_length_dynamic = 10**np.random.uniform(0, 3)
    pars = dict(
        #background=0,
        scale=1,
        gauss_scale=gauss_scale,
        lorentz_scale=lorentz_scale,
        cor_length_static=cor_length_static,
        cor_length_dynamic=cor_length_dynamic,
    )
    return pars


tests = [

    # Accuracy tests based on content in test/utest_extra_models.py
    [{'gauss_scale':  100.0,
      'cor_length_static':   100.0,
      'lorentz_scale': 50.0,
      'cor_length_dynamic':   20.0,
     }, 0.001, 149.482],

    [{'gauss_scale':  100.0,
      'cor_length_static':   100.0,
      'lorentz_scale': 50.0,
      'cor_length_dynamic':   20.0,
     }, 0.105363, 9.1913],

    [{'gauss_scale':  100.0,
      'cor_length_static':   100.0,
      'lorentz_scale': 50.0,
      'cor_length_dynamic':   20.0,
     }, 0.441623, 0.633811],

    # Additional tests with larger range of parameters
    [{'gauss_scale':  10.0,
      'cor_length_static':  100.0,
      'lorentz_scale': 3.0,
      'cor_length_dynamic':   1.0,
     }, 0.1, 2.9712970297],

    [{'gauss_scale':  10.0,
      'cor_length_static':  100.0,
      'lorentz_scale': 3.0,
      'cor_length_dynamic':   1.0,
      'background':         100.0
     }, 5.0, 100.116384615],

    [{'gauss_scale':  10.0,
      'cor_length_static':  100.0,
      'lorentz_scale': 3.0,
      'cor_length_dynamic':   1.0,
      'background':           0.0,
     }, 200., 7.49981250469e-05],
    ]
