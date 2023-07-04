r"""
.. _core_shell_sphere:

This model provides the form factor, $P(q)$, for a spherical particle with
a core-shell structure. The form factor is normalized by the particle volume.

For information about polarised and magnetic scattering, see
the :ref:`magnetism` documentation.

Definition
----------

The 1D scattering intensity is calculated in the following way (Guinier, 1955)

.. math::

    P(q) = \frac{\text{scale}}{V} F^2(q) + \text{background}

where

.. math::

    F(q) = \frac{3}{V_s}\left[
       V_c(\rho_c-\rho_s)\frac{\sin(qr_c)-qr_c\cos(qr_c)}{(qr_c)^3} +
       V_s(\rho_s-\rho_\text{solv})\frac{\sin(qr_s)-qr_s\cos(qr_s)}{(qr_s)^3}
       \right]

where $V_s$ is the volume of the whole particle, $V_c$ is the volume of the
core, $r_s$ = $radius$ + $thickness$ is the radius of the particle, $r_c$
is the radius of the core, $\rho_c$ is the scattering length density of the
core, $\rho_s$ is the scattering length density of the shell,
$\rho_\text{solv}$, is the scattering length density of the solvent.

The 2D scattering intensity is the same as $P(q)$ above, regardless of the
orientation of the $q$ vector.

NB: The outer most radius (ie, = radius + thickness) is used as the
effective radius for $S(Q)$ when $P(Q) \cdot S(Q)$ is applied.

Validation
----------

Validation of our code was done by comparing the output of the 1D model to
the output of the software provided by NIST (Kline, 2006). Figure 1 shows a
comparison of the output of our model and the output of the NIST software.

References
----------

#. A Guinier and G Fournet, *Small-Angle Scattering of X-Rays*,
   John Wiley and Sons, New York, (1955)

Authorship and Verification
----------------------------

* **Author:**
* **Last Modified by:**
* **Last Reviewed by:**
"""

import numpy as np
from numpy import pi, inf

name = "core_shell_sphere"
title = "Form factor for a monodisperse spherical particle with particle with a core-shell structure."
description = """
    F(q) = [V_c (sld_core-sld_shell) 3 (sin(q*radius)-q*radius*cos(q*radius))/(q*radius)^3
            + V_s (sld_shell-sld_solvent) 3 (sin(q*r_s)-q*r_s*cos(q*r_s))/(q*r_s)^3]

            V_s: Volume of the sphere shell
            V_c: Volume of the sphere core
            r_s: Shell radius = radius + thickness
"""
category = "shape:sphere"

# pylint: disable=bad-whitespace, line-too-long
#             ["name", "units", default, [lower, upper], "type","description"],
parameters = [["radius",      "Ang",        60.0, [0, inf],    "volume", "Sphere core radius"],
              ["thickness",   "Ang",        10.0, [0, inf],    "volume", "Sphere shell thickness"],
              ["sld_core",    "1e-6/Ang^2", 1.0,  [-inf, inf], "sld",    "core scattering length density"],
              ["sld_shell",   "1e-6/Ang^2", 2.0,  [-inf, inf], "sld",    "shell scattering length density"],
              ["sld_solvent", "1e-6/Ang^2", 3.0,  [-inf, inf], "sld",    "Solvent scattering length density"]]
# pylint: enable=bad-whitespace, line-too-long

source = ["lib/sas_3j1x_x.c", "lib/core_shell.c", "core_shell_sphere.c"]
have_Fq = True
radius_effective_modes = ["outer radius", "core radius"]

def random():
    """Return a random parameter set for the model."""
    outer_radius = 10**np.random.uniform(1.3, 4.3)
    # Use a distribution with a preference for thin shell or thin core
    # Avoid core,shell radii < 1
    radius = np.random.beta(0.5, 0.5)*(outer_radius-2) + 1
    thickness = outer_radius - radius
    pars = dict(
        radius=radius,
        thickness=thickness,
    )
    return pars

tests = [
    [{'radius': 20.0, 'thickness': 10.0}, 0.1, None, None, 30.0, 4.*pi/3*30**3, 1.0],

    # The SasView test result was 0.00169, with a background of 0.001
    [{'radius': 60.0, 'thickness': 10.0, 'sld_core': 1.0, 'sld_shell': 2.0,
      'sld_solvent': 3.0, 'background': 0.0}, 0.4, 0.000698838],
]
