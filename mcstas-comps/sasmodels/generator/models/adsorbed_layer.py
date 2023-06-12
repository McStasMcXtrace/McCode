r"""
Definition
----------

This model describes the scattering from a layer of surfactant or polymer
adsorbed on large, smooth, notionally spherical particles under the conditions
that (i) the particles (cores) are contrast-matched to the dispersion medium,
(ii) $S(Q) \sim 1$ (ie, the particle volume fraction is dilute), (iii) the
particle radius is >> layer thickness (ie, the interface is locally flat),
and (iv) scattering from excess unadsorbed adsorbate in the bulk medium is
absent or has been corrected for.

Unlike many other core-shell models, this model does not assume any form
for the density distribution of the adsorbed species normal to the interface
(cf, a core-shell model normally assumes the density distribution to be a
homogeneous step-function). For comparison, if the thickness of a (traditional
core-shell like) step function distribution is $t$, the second moment about
the mean of the density distribution (ie, the distance of the centre-of-mass
of the distribution from the interface), $\sigma = \sqrt{t^2/12}$.

.. math::

     I(q) = \text{scale} \cdot (\rho_\text{poly}-\rho_\text{solvent})^2
         \left[
             \frac{6\pi\phi_\text{core}}{Q^2}
             \frac{\Gamma^2}{\delta_\text{poly}^2R_\text{core}}
             \exp(-Q^2\sigma^2)
         \right] + \text{background}

where *scale* is a scale factor, $\rho_\text{poly}$ is the sld of the
polymer (or surfactant) layer, $\rho_\text{solv}$ is the sld of the
solvent/medium and cores, $\phi_\text{core}$ is the volume fraction of
the core particles, $\delta_\text{poly}$ is the bulk density of the
polymer, $\Gamma$ is the adsorbed amount, and $\sigma$ is the second
moment of the thickness distribution.

Note that all parameters except $\sigma$ are correlated so fitting more
than one of these parameters will generally fail. Also note that unlike
other shape models, no volume normalization is applied to this model (the
calculation is exact).

The code for this model is based originally on a a fortran implementation by
Steve King at ISIS in the SANDRA package c. 1990 [#King2002]_.

References
----------

.. [#King2002] S King, P Griffiths, J Hone, and T Cosgrove, *SANS from
   Adsorbed Polymer Layers*, *Macromol. Symp.*, 190 (2002) 33-42.

Authorship and Verification
----------------------------

* **Author:** Jae-Hi Cho **Date:** pre 2010
* **Last Modified by:** Paul Kienzle **Date:** April 14, 2016
* **Last Reviewed by:** Steve King **Date:** March 18, 2016
"""

import numpy as np
from numpy import inf, pi, exp, errstate

name = "adsorbed_layer"
title = "Scattering from an adsorbed layer on particles"

description = """
    Evaluates the scattering from large particles
    with an adsorbed layer of surfactant or
    polymer, independent of the form of the
    density distribution.
    """
category = "shape:sphere"

# pylint: disable=bad-whitespace, line-too-long
#   ["name", "units", default, [lower, upper], "type", "description"],
parameters = [
    ["second_moment", "Ang", 23.0, [0.0, inf], "", "Second moment of polymer distribution"],
    ["adsorbed_amount", "mg/m^2", 1.9, [0.0, inf], "", "Adsorbed amount of polymer"],
    ["density_shell", "g/cm^3", 0.7, [0.0, inf], "", "Bulk density of polymer in the shell"],
    ["radius", "Ang", 500.0, [0.0, inf], "", "Core particle radius"],
    ["volfraction", "None", 0.14, [0.0, inf], "", "Core particle volume fraction"],
    ["sld_shell", "1e-6/Ang^2", 1.5, [-inf, inf], "sld", "Polymer shell SLD"],
    ["sld_solvent", "1e-6/Ang^2", 6.3, [-inf, inf], "sld", "Solvent SLD"],
]
# pylint: enable=bad-whitespace, line-too-long

# NB: Scale and Background are implicit parameters on every model
def Iq(q, second_moment, adsorbed_amount, density_shell, radius,
       volfraction, sld_shell, sld_solvent):
    """Return I(q) for adsorbed layer model."""
    with errstate(divide='ignore'):
        aa = ((sld_shell - sld_solvent)/density_shell * adsorbed_amount) / q
    bb = q * second_moment
    #scale by 10^-2 for units conversion to cm^-1
    inten = 6.0e-02 * pi * volfraction * aa**2 * exp(-bb**2) / radius
    return inten
Iq.vectorized = True  # Iq accepts an array of q values

def random():
    """Return a random parameter set for the model."""
    # only care about the value of second_moment:
    #    curve = scale * e**(-second_moment^2 q^2)/q^2
    #    scale = 6 pi/100 (contrast/density*absorbed_amount)^2 * Vf/radius
    # the remaining parameters can be randomly generated from zero to
    # twice the default value as done by default in compare.py
    pars = dict(
        scale=1,
        second_moment=10**np.random.uniform(1, 3),
    )
    return pars

# unit test values taken from SasView 3.1.2
tests = [
    [{'scale': 1.0, 'second_moment': 23.0, 'adsorbed_amount': 1.9,
      'density_shell': 0.7, 'radius': 500.0, 'volfraction': 0.14,
      'sld_shell': 1.5, 'sld_solvent': 6.3, 'background': 0.0},
     [0.0106939, 0.1], [73.741, 4.51684e-3]],
]

# 2016-03-16 SMK converted from sasview, checked vs SANDRA
# 2016-03-18 RKH some edits & renaming
# 2016-04-14 PAK reformatting
