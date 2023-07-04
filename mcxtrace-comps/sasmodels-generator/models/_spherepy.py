r"""
For information about polarised and magnetic scattering, see
the :ref:`magnetism` documentation.

Definition
----------

The 1D scattering intensity is calculated in the following way [Guinier1955]_

.. math::

    I(q) = \frac{\text{scale}}{V} \cdot \left[
        3V(\Delta\rho) \cdot \frac{\sin(qr) - qr\cos(qr))}{(qr)^3}
        \right]^2 + \text{background}

where *scale* is a volume fraction, $V$ is the volume of the scatterer,
$r$ is the radius of the sphere, *background* is the background level and
*sld* and *sld_solvent* are the scattering length densities (SLDs) of the
scatterer and the solvent respectively.

Note that if your data is in absolute scale, the *scale* should represent
the volume fraction (which is unitless) if you have a good fit. If not,
it should represent the volume fraction times a factor (by which your data
might need to be rescaled).

The 2D scattering intensity is the same as above, regardless of the
orientation of $\vec q$.

Validation
----------

Validation of our code was done by comparing the output of the 1D model
to the output of the software provided by the NIST (Kline, 2006).

References
----------

.. [Guinier1955] A Guinier and G. Fournet, *Small-Angle Scattering of X-Rays*,
   John Wiley and Sons, New York, (1955)

Authorship and Verification
----------------------------

* **Author: P Kienzle**
* **Last Modified by:**
* **Last Reviewed by:** S King and P Parker **Date:** 2013/09/09 and 2014/01/06
"""

import numpy as np
from numpy import pi, inf, sin, cos, sqrt, log

name = " _sphere (python)"
title = "PAK testing ideas for Spheres with uniform scattering length density"
description = """\
P(q)=(scale/V)*[3V(sld-sld_solvent)*(sin(qr)-qr cos(qr))
                /(qr)^3]^2 + background
    r: radius of sphere
    V: The volume of the scatter
    sld: the SLD of the sphere
    sld_solvent: the SLD of the solvent
"""
category = "shape:sphere"

#             ["name", "units", default, [lower, upper], "type","description"],
parameters = [["sld", "1e-6/Ang^2", 1, [-inf, inf], "",
               "Layer scattering length density"],
              ["sld_solvent", "1e-6/Ang^2", 6, [-inf, inf], "",
               "Solvent scattering length density"],
              ["radius", "Ang", 50, [0, inf], "volume",
               "Sphere radius"],
             ]


def form_volume(radius):
    """Calculate volume for sphere"""
    return 1.333333333333333 * pi * radius ** 3

def radius_effective(mode, radius):
    """Calculate R_eff for sphere"""
    return radius if mode else 0.

def Iq(q, sld, sld_solvent, radius):
    """Calculate I(q) for sphere"""
    #print "q",q
    #print "sld,r",sld,sld_solvent,radius
    qr = q * radius
    sn, cn = sin(qr), cos(qr)
    ## The natural expression for the Bessel function is the following:
    ##     bes = 3 * (sn-qr*cn)/qr**3 if qr>0 else 1
    ## however, to support vector q values we need to handle the conditional
    ## as a vector, which we do by first evaluating the full expression
    ## everywhere, then fixing it up where it is broken.   We should probably
    ## set numpy to ignore the 0/0 error before we do though...
    bes = 3 * (sn - qr * cn) / qr ** 3 # may be 0/0 but we fix that next line
    bes[qr == 0] = 1
    fq = bes * (sld - sld_solvent) * form_volume(radius)
    return 1.0e-4 * fq ** 2
Iq.vectorized = True  # Iq accepts an array of q values

def sesans(z, sld, sld_solvent, radius):
    """
    Calculate SESANS-correlation function for a solid sphere.

    Wim Bouwman after formulae Timofei Kruglov J.Appl.Cryst. 2003 article
    """
    d = z / radius
    g = np.zeros_like(z)
    g[d == 0] = 1.
    low = ((d > 0) & (d < 2))
    dlow = d[low]
    dlow2 = dlow ** 2
    g[low] = (sqrt(1 - dlow2/4.) * (1 + dlow2/8.)
              + dlow2/2.*(1 - dlow2/16.) * log(dlow / (2. + sqrt(4. - dlow2))))
    return g
sesans.vectorized = True  # sesans accepts an array of z values
