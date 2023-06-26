r"""
Definition
----------

This model calculates the scattered intensity of a two-component system
using the Teubner-Strey model. Unlike :ref:`dab` this function generates
a peak. A two-phase material can be characterised by two length scales -
a correlation length and a domain size (periodicity).

The original paper by Teubner and Strey defined the function as:

.. math::

    I(q) \propto \frac{1}{a_2 + c_1 q^2 + c_2 q^4} + \text{background}

where the parameters $a_2$, $c_1$ and $c_2$ are defined in terms of the
periodicity, $d$, and correlation length $\xi$ as:

.. math::

    a_2 &= \biggl[1+\bigl(\frac{2\pi\xi}{d}\bigr)^2\biggr]^2\\
    c_1 &= -2\xi^2\bigl(\frac{2\pi\xi}{d}\bigr)^2+2\xi^2\\
    c_2 &= \xi^4

and thus, the periodicity, $d$ is given by

.. math::

    d = 2\pi\left[\frac12\left(\frac{a_2}{c_2}\right)^{1/2}
                  - \frac14\frac{c_1}{c_2}\right]^{-1/2}

and the correlation length, $\xi$, is given by

.. math::

    \xi = \left[\frac12\left(\frac{a_2}{c_2}\right)^{1/2}
                  + \frac14\frac{c_1}{c_2}\right]^{-1/2}

Here the model is parameterised in terms of  $d$ and $\xi$ and with an explicit
volume fraction for one phase, $\phi_a$, and contrast,
$\delta\rho^2 = (\rho_a - \rho_b)^2$ :

.. math::

    I(q) = \frac{8\pi\phi_a(1-\phi_a)(\Delta\rho)^2c_2/\xi}
        {a_2 + c_1q^2 + c_2q^4}

where :math:`8\pi\phi_a(1-\phi_a)(\Delta\rho)^2c_2/\xi` is the constant of
proportionality from the first equation above.

In the case of a microemulsion, $a_2 > 0$, $c_1 < 0$, and $c_2 >0$.

For 2D data, scattering intensity is calculated in the same way as 1D,
where the $q$ vector is defined as

.. math::

    q = \sqrt{q_x^2 + q_y^2}

References
----------

#. M Teubner, R Strey, *J. Chem. Phys.*, 87 (1987) 3195
#. K V Schubert, R Strey, S R Kline and E W Kaler,
   *J. Chem. Phys.*, 101 (1994) 5343
#. H Endo, M Mihailescu, M. Monkenbusch, J Allgaier, G Gompper, D Richter,
   B Jakobs, T Sottmann, R Strey, and I Grillo,
   *J. Chem. Phys.*, 115 (2001), 580

Authorship and Verification
----------------------------

* **Author:**
* **Last Modified by:**
* **Last Reviewed by:**
"""
from __future__ import division

import numpy as np
from numpy import inf, pi

name = "teubner_strey"
title = "Teubner-Strey model of microemulsions"
description = """\
    Calculates scattering according to the Teubner-Strey model
"""
category = "shape-independent"

#   ["name", "units", default, [lower, upper], "type","description"],
parameters = [
    ["volfraction_a", "", 0.5, [0, 1.0], "", "Volume fraction of phase a"],
    ["sld_a", "1e-6/Ang^2", 0.3, [-inf, inf], "", "SLD of phase a"],
    ["sld_b", "1e-6/Ang^2", 6.3, [-inf, inf], "", "SLD of phase b"],
    ["d", "Ang", 100.0, [0, inf], "", "Domain size (periodicity)"],
    ["xi", "Ang", 30.0, [0, inf], "", "Correlation length"],
    ]

def Iq(q, volfraction_a, sld_a, sld_b, d, xi):
    """SAS form"""
    drho = sld_a - sld_b
    k = 2.0*pi*xi/d
    a2 = (1.0 + k**2)**2
    c1 = 2.0*xi**2 * (1.0 - k**2)
    c2 = xi**4
    prefactor = 8.0*pi * volfraction_a*(1.0 - volfraction_a) * drho**2 * c2/xi
    return 1.0e-4*prefactor / np.polyval([c2, c1, a2], q**2)

Iq.vectorized = True  # Iq accepts an array of q values

def random():
    """Return a random parameter set for the model."""
    d = 10**np.random.uniform(1, 4)
    xi = 10**np.random.uniform(-0.3, 2)*d
    pars = dict(
        #background=0,
        scale=100,
        volfraction_a=10**np.random.uniform(-3, 0),
        sld_a=np.random.uniform(-0.5, 12),
        sld_b=np.random.uniform(-0.5, 12),
        d=d,
        xi=xi,
    )
    return pars

tests = [[{}, 0.06, 41.5918888453]]
