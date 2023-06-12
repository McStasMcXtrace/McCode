r"""
Definition
----------

This model fits the Guinier function

.. math::

    I(q) = \text{scale} \cdot \exp{\left[ \frac{-Q^2 R_g^2 }{3} \right]}
            + \text{background}

to the data directly without any need for linearization (*cf*. the usual
plot of $\ln I(q)$ vs $q^2$\ ). Note that you may have to restrict the data
range to include small q only, where the Guinier approximation actually
applies. See also the guinier_porod model.

For 2D data the scattering intensity is calculated in the same way as 1D,
where the $q$ vector is defined as

.. math:: q = \sqrt{q_x^2 + q_y^2}

In scattering, the radius of gyration $R_g$ quantifies the object's
distribution of SLD (not mass density, as in mechanics) from the object's
SLD centre of mass. It is defined by

.. math:: R_g^2 = \frac{\sum_i\rho_i\left(r_i-r_0\right)^2}{\sum_i\rho_i}

where $r_0$ denotes the object's SLD centre of mass and $\rho_i$ is the SLD at
a point $i$.

Notice that $R_g^2$ may be negative (since SLD can be negative), which happens
when a form factor $P(Q)$ is increasing with $Q$ rather than decreasing. This
can occur for core/shell particles, hollow particles, or for composite
particles with domains of different SLDs in a solvent with an SLD close to the
average match point. (Alternatively, this might be regarded as there being an
internal inter-domain "structure factor" within a single particle which gives
rise to a peak in the scattering).

To specify a negative value of $R_g^2$ in SasView, simply give $R_g$ a negative
value ($R_g^2$ will be evaluated as $R_g |R_g|$). Note that the physical radius
of gyration, of the exterior of the particle, will still be large and positive.
It is only the apparent size from the small $Q$ data that will give a small or
negative value of $R_g^2$.

References
----------

#. A Guinier and G Fournet, *Small-Angle Scattering of X-Rays*,
   John Wiley & Sons, New York (1955)

Authorship and Verification
----------------------------

* **Author:**
* **Last Modified by:**
* **Last Reviewed by:**
"""

import numpy as np
from numpy import inf

name = "guinier"
title = ""
description = """
 I(q) = scale.exp ( - rg^2 q^2 / 3.0 )

    List of default parameters:
    scale = scale
    rg = Radius of gyration
"""
category = "shape-independent"

#             ["name", "units", default, [lower, upper], "type","description"],
parameters = [["rg", "Ang", 60.0, [-inf, inf], "", "Radius of Gyration"]]

source = ["guinier.c"] 


def random():
    """Return a random parameter set for the model."""
    scale = 10**np.random.uniform(1, 4)
    # Note: compare.py has Rg cutoff of 1e-30 at q=1 for Guinier, so use that
    # log_10 Ae^(-(q Rg)^2/3) = log_10(A) - (q Rg)^2/ (3 ln 10) > -30
    #   => log_10(A) > Rg^2/(3 ln 10) - 30
    q_max = 1.0
    rg_max = np.sqrt(90*np.log(10) + 3*np.log(scale))/q_max
    rg = 10**np.random.uniform(0, np.log10(rg_max))
    pars = dict(
        #background=0,
        scale=scale,
        rg=rg,
    )
    return pars

# parameters for unit tests
tests = [[{'rg' : 31.5}, 0.005, 0.992756]]
