r"""
This model provides the scattering intensity, $I(q) = P(q) S(q)$, for a
lamellar phase where a random distribution in solution are assumed.
Here a Caille $S(q)$ is used for the lamellar stacks.

Definition
----------

The scattering intensity $I(q)$ is

.. math::

    I(q) = 2\pi \frac{P(q)S(q)}{q^2\delta }

The form factor is

.. math::

    P(q) = \frac{2\Delta\rho^2}{q^2}\left(1-\cos q\delta \right)

and the structure factor is

.. math::

    S(q) = 1 + 2 \sum_1^{N-1}\left(1-\frac{n}{N}\right)
           \cos(qdn)\exp\left(-\frac{2q^2d^2\alpha(n)}{2}\right)

where

.. math::
    :nowrap:

    \begin{align*}
    \alpha(n) &= \frac{\eta_{cp}}{4\pi^2} \left(\ln(\pi n)+\gamma_E\right)
              && \\
    \gamma_E  &= 0.5772156649
              && \text{Euler's constant} \\
    \eta_{cp} &= \frac{q_o^2k_B T}{8\pi\sqrt{K\overline{B}}}
              && \text{Caille constant}
    \end{align*}

Here $d$ = (repeat) d_spacing, $\delta$ = bilayer thickness,
the contrast $\Delta\rho$ = SLD(headgroup) - SLD(solvent),
$K$ = smectic bending elasticity, $B$ = compression modulus, and
$N$ = number of lamellar plates (*n_plates*).

NB: **When the Caille parameter is greater than approximately 0.8 to 1.0, the
assumptions of the model are incorrect.** And due to a complication of the
model function, users are responsible for making sure that all the assumptions
are handled accurately (see the original reference below for more details).

Non-integer numbers of stacks are calculated as a linear combination of
results for the next lower and higher values.

The 2D scattering intensity is calculated in the same way as 1D, where the
$q$ vector is defined as

.. math::

    q = \sqrt{q_x^2 + q_y^2}



References
----------

#. F Nallet, R Laversanne, and D Roux, *J. Phys. II France*, 3, (1993) 487-502
#. J Berghausen, J Zipfel, P Lindner, W Richtering,
   *J. Phys. Chem. B*, 105, (2001) 11081-11088

Authorship and Verification
----------------------------

* **Author:**
* **Last Modified by:**
* **Last Reviewed by:**
"""

import numpy as np
from numpy import inf

name = "lamellar_stack_caille"
title = "Random lamellar sheet with Caille structure factor"
description = """\
    [Random lamellar phase with Caille  structure factor]
    randomly oriented stacks of infinite sheets
    with Caille S(Q), having polydisperse spacing.
    sld = sheet scattering length density
    sld_solvent = solvent scattering length density
    background = incoherent background
    scale = scale factor
"""
category = "shape:lamellae"

single = False  # TODO: check
# pylint: disable=bad-whitespace, line-too-long
#             ["name", "units", default, [lower, upper], "type","description"],
parameters = [
    ["thickness",        "Ang",      30.0,  [0, inf],   "volume", "sheet thickness"],
    ["Nlayers",          "",          20,   [1, inf],   "",       "Number of layers"],
    ["d_spacing",        "Ang",      400.,  [0.0,inf],  "volume", "lamellar d-spacing of Caille S(Q)"],
    ["Caille_parameter", "1/Ang^2",    0.1, [0.0,0.8],  "",       "Caille parameter"],
    ["sld",              "1e-6/Ang^2", 6.3, [-inf,inf], "sld",    "layer scattering length density"],
    ["sld_solvent",      "1e-6/Ang^2", 1.0, [-inf,inf], "sld",    "Solvent scattering length density"],
    ]
# pylint: enable=bad-whitespace, line-too-long

source = ["lamellar_stack_caille.c"]

def random():
    """Return a random parameter set for the model."""
    total_thickness = 10**np.random.uniform(2, 4.7)
    Nlayers = np.random.randint(2, 200)
    d_spacing = total_thickness / Nlayers
    thickness = d_spacing * np.random.uniform(0, 1)
    Caille_parameter = np.random.uniform(0, 0.8)
    pars = dict(
        thickness=thickness,
        Nlayers=Nlayers,
        d_spacing=d_spacing,
        Caille_parameter=Caille_parameter,
    )
    return pars

# No volume normalization despite having a volume parameter
# This should perhaps be volume normalized?
form_volume = """
    return 1.0;
    """

#
tests = [
    [{'scale': 1.0, 'background': 0.0, 'thickness': 30., 'Nlayers': 20.0,
      'd_spacing': 400., 'Caille_parameter': 0.1, 'sld': 6.3,
      'sld_solvent': 1.0, 'thickness_pd': 0.0, 'd_spacing_pd': 0.0},
     [0.001], [28895.13397]]
    ]
# ADDED by: RKH  ON: 18Mar2016  converted from sasview previously, now renaming everything & sorting the docs
