r"""
Definition
----------

This model provides the form factor, $P(q)$, for a monodisperse hollow right
angle circular cylinder (rigid tube) where the The inside and outside of the
hollow cylinder are assumed to have the same SLD and the form factor is thus
normalized by the volume of the tube (i.e. not by the total cylinder volume).

.. math::

    P(q) = \text{scale} \left<F^2\right>/V_\text{shell} + \text{background}

where the averaging $\left<\ldots\right>$ is applied only for the 1D
calculation. If Intensity is given on an absolute scale, the scale factor here
is the volume fraction of the shell.  This differs from
the :ref:`core-shell-cylinder` in that, in that case, scale is the volume
fraction of the entire cylinder (core+shell). The application might be for a
bilayer which wraps into a hollow tube and the volume fraction of material is
all in the shell, whereas the :ref:`core-shell-cylinder` model might be used for
a cylindrical micelle where the tails in the core have a different SLD than the
headgroups (in the shell) and the volume fraction of material comes fromm the
whole cyclinder.  NOTE: the hollow_cylinder represents a tube whereas the
core_shell_cylinder includes a shell layer covering the ends (end caps) as well.


The 1D scattering intensity is calculated in the following way (Guinier, 1955)

.. math::

    P(q)           &= (\text{scale})V_\text{shell}\Delta\rho^2
            \int_0^{1}\Psi^2
            \left[q_z, R_\text{outer}(1-x^2)^{1/2},
                       R_\text{core}(1-x^2)^{1/2}\right]
            \left[\frac{\sin(qHx)}{qHx}\right]^2 dx \\
    \Psi[q,y,z]    &= \frac{1}{1-\gamma^2}
            \left[ \Lambda(qy) - \gamma^2\Lambda(qz) \right] \\
    \Lambda(a)     &= 2 J_1(a) / a \\
    \gamma         &= R_\text{core} / R_\text{outer} \\
    V_\text{shell} &= \pi \left(R_\text{outer}^2 - R_\text{core}^2 \right)L \\
    J_1(x)         &= (\sin(x)-x\cdot \cos(x)) / x^2

where *scale* is a scale factor, $H = L/2$ and $J_1$ is the 1st order
Bessel function.

**NB**: The 2nd virial coefficient of the cylinder is calculated
based on the outer radius and full length, which give an the effective radius
for structure factor $S(q)$ when $P(q) \cdot S(q)$ is applied.

In the parameters,the *radius* is $R_\text{core}$ while *thickness*
is $R_\text{outer} - R_\text{core}$.

To provide easy access to the orientation of the core-shell cylinder, we define
the axis of the cylinder using two angles $\theta$ and $\phi$
(see :ref:`cylinder model <cylinder-angle-definition>`).

References
----------

#. L A Feigin and D I Svergun, *Structure Analysis by Small-Angle X-Ray and
   Neutron Scattering*, Plenum Press, New York, (1987)
#. L. Onsager, *Ann. New York Acad. Sci.*, 51 (1949) 627-659

Authorship and Verification
----------------------------

* **Author:** NIST IGOR/DANSE **Date:** pre 2010
* **Last Modified by:** Paul Butler **Date:** September 06, 2018
   (corrected VR calculation)
* **Last Reviewed by:** Paul Butler **Date:** September 06, 2018
"""
from __future__ import division

import numpy as np
from numpy import pi, inf, sin, cos

name = "hollow_cylinder"
title = ""
description = """
P(q) = scale*<f*f>/Vol + background, where f is the scattering amplitude.
radius = the radius of core
thickness = the thickness of shell
length = the total length of the cylinder
sld = SLD of the shell
sld_solvent = SLD of the solvent
background = incoherent background
"""
category = "shape:cylinder"
# pylint: disable=bad-whitespace, line-too-long
#   ["name", "units", default, [lower, upper], "type","description"],
parameters = [
    ["radius",      "Ang",     20.0, [0, inf],    "volume",      "Cylinder core radius"],
    ["thickness",   "Ang",     10.0, [0, inf],    "volume",      "Cylinder wall thickness"],
    ["length",      "Ang",    400.0, [0, inf],    "volume",      "Cylinder total length"],
    ["sld",         "1e-6/Ang^2",  6.3, [-inf, inf], "sld",      "Cylinder sld"],
    ["sld_solvent", "1e-6/Ang^2",  1,   [-inf, inf], "sld",      "Solvent sld"],
    ["theta",       "degrees", 90,   [-360, 360], "orientation", "Cylinder axis to beam angle"],
    ["phi",         "degrees",  0,   [-360, 360], "orientation", "Rotation about beam"],
    ]
# pylint: enable=bad-whitespace, line-too-long

source = ["lib/polevl.c", "lib/sas_J1.c", "lib/gauss76.c", "hollow_cylinder.c"]
have_Fq = True
radius_effective_modes = [
    "excluded volume", "equivalent outer volume sphere",
    "outer radius", "half length",
    "half outer min dimension", "half outer max dimension",
    "half outer diagonal",
    ]

def random():
    """Return a random parameter set for the model."""
    length = 10**np.random.uniform(1, 4.7)
    outer_radius = 10**np.random.uniform(1, 4.7)
    # Use a distribution with a preference for thin shell or thin core
    # Avoid core,shell radii < 1
    thickness = np.random.beta(0.5, 0.5)*(outer_radius-2) + 1
    radius = outer_radius - thickness
    pars = dict(
        length=length,
        radius=radius,
        thickness=thickness,
    )
    return pars

# Tests for structure factor support functions. These functions duplicate
# the equivalent functions from the C code for the hollow cylinder model.
def r_eff(radius, thickness, length):
    """R_eff from excluded volume"""
    radius += thickness
    return (0.5*(0.75*radius*(2.0*radius*length
                              + (radius + length)*(pi*radius + length))
                )**(1./3.))

def shell_volume(radius, thickness, length):
    """shell volume for parameter set"""
    return pi*((radius+thickness)**2-radius**2)*length

def form_shell_ratio(radius, thickness, length):
    """form:shell ratio"""
    return (radius+thickness)**2/((radius+thickness)**2 - radius**2)

q = 0.1
# april 6 2017, rkh added a 2d unit test, assume correct!
qx = q*cos(pi/6.0)
qy = q*sin(pi/6.0)
test_pars = [
    parameters[0][2], # radius
    parameters[1][2], # thickness
    parameters[2][2], # length
]
# Parameters for unit tests
tests = [
    [{}, 0.00005, 1764.926],
    [{}, 0.1, None, None,
     r_eff(*test_pars), shell_volume(*test_pars), form_shell_ratio(*test_pars),
    ],
    [{}, 0.001, 1756.76],
    [{}, (qx, qy), 2.36885476192],
]
del qx, qy, test_pars  # not necessary to delete, but cleaner
