r"""
Definition
----------

This model calculates an empirical functional form for SAS data characterized
by a broad scattering peak. Many SAS spectra are characterized by a broad peak
even though they are from amorphous soft materials. For example, soft systems
that show a SAS peak include copolymers, polyelectrolytes, multiphase systems,
layered structures, etc.

The d-spacing corresponding to the broad peak is a characteristic distance
between the scattering inhomogeneities (such as in lamellar, cylindrical, or
spherical morphologies, or for bicontinuous structures).

The scattering intensity $I(q)$ is calculated as

.. math:: I(q) = \frac{A}{q^n} + \frac{C}{1 + (|q - q_0|\xi)^m}^p + B

Here the peak position is related to the d-spacing as $q_0 = 2\pi / d_0$.

$A$ is the Porod law scale factor, $n$ the Porod exponent, $C$ is the
Lorentzian scale factor, $m$ the exponent of $q$, $\xi$ the screening length,
and $B$ the flat background. $p$ generalizes the model. With m = 2 and p = 1
the Lorentz model is obtained whereas for m = 2 and p = 2 the Broad-Peak model
is identical to the Debye-Anderson-Brumberger (dab) model.

For 2D data the scattering intensity is calculated in the same way as 1D,
where the $q$ vector is defined as

.. math:: q = \sqrt{q_x^2 + q_y^2}

References
----------

None.

Authorship and Verification
----------------------------

* **Author:** NIST IGOR/DANSE **Date:** pre 2010
* **Last Modified by:** Dirk Honecker **Date:** May 28, 2021
* **Last Reviewed by:** Richard Heenan **Date:** March 21, 2016
"""

import numpy as np
from numpy import inf, errstate

name = "broad_peak"
title = "Broad peak on top of a power law decay"
description = """\
      I(q) = scale_p/pow(q,exponent)+scale_l/
      pow(1.0 + pow(fabs(q-q_peak)*length_l,exponent_l), exponent_p )+ background

      List of default parameters:
      porod_scale = Porod term scaling
      porod_exp = Porod exponent
      peak_scale = Lorentzian term scaling
      correlation_length = Correlation length [A]
      peak_pos = peak location [1/A]
      width_exp = peak width exponent
      shape_exp = peak shape exponent
      background = Incoherent background"""
category = "shape-independent"

# pylint: disable=bad-whitespace, line-too-long
#             ["name", "units", default, [lower, upper], "type", "description"],
parameters = [["porod_scale",    "",  1.0e-05, [-inf, inf], "", "Power law scale factor"],
              ["porod_exp",      "",      3.0, [-inf, inf], "", "Exponent of power law"],
              ["peak_scale",  "",     10.0, [-inf, inf], "", "Scale factor for broad peak"],
              ["correlation_length", "Ang",  50.0, [-inf, inf], "", "screening length"],
              ["peak_pos",       "1/Ang", 0.1, [-inf, inf], "", "Peak position in q"],
              ["width_exp",    "",      2.0, [-inf, inf], "", "Exponent of peak width"],
              ["shape_exp",    "",      1.0, [-inf, inf], "", "Exponent of peak shape"],              
             ]
# pylint: enable=bad-whitespace, line-too-long

def Iq(q,
       porod_scale=1.0e-5,
       porod_exp=3.0,
       peak_scale=10.0,
       correlation_length=50.0,
       peak_pos=0.1,
       width_exp=2.0,
       shape_exp=1.0):
    """
    :param q:                    Input q-value
    :param porod_scale:          Power law scale factor
    :param porod_exp:            Exponent of power law
    :param peak_scale:           Scale factor for broad peak
    :param correlation_length:   Correlation length
    :param peak_pos:             Peak position in q
    :param width_exp:            Exponent of peak width
    :param shape_exp:            Exponent of peak shape    
    :return:                     Calculated intensity
    """
    z = abs(q - peak_pos) * correlation_length
    with errstate(divide='ignore'):
        inten = (porod_scale / q ** porod_exp
                 + peak_scale / (1 + z ** width_exp)) ** shape_exp
    return inten
Iq.vectorized = True  # Iq accepts an array of q values

def random():
    """Return a random parameter set for the model."""
    pars = dict(
        scale=1,
        porod_scale=10**np.random.uniform(-8, -5),
        porod_exp=np.random.uniform(1, 6),
        peak_scale=10**np.random.uniform(0.3, 6),
        correlation_length=10**np.random.uniform(0, 2),
        peak_pos=10**np.random.uniform(-3, -1),
        width_exp=np.random.uniform(1, 4),
        shape_exp=np.random.uniform(1, 2),      
    )
    pars['correlation_length'] /= pars['peak_pos']
    pars['peak_scale'] *= pars['porod_scale'] / pars['peak_pos']**pars['porod_exp']
    #pars['porod_scale'] = 0.
    return pars
