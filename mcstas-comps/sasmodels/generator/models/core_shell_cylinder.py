r"""
Definition
----------

The output of the 2D scattering intensity function for oriented core-shell
cylinders is given by Kline [#Kline2006]_. The form factor is normalized
by the particle volume. Note that in this model the shell envelops the entire
core so that besides a "sleeve" around the core, the shell also provides two
flat end caps of thickness = shell thickness. In other words the length of the
total cylinder is the length of the core cylinder plus twice the thickness of
the shell. If no end caps are desired one should use the
:ref:`core-shell-bicelle` and set the thickness of the end caps (in this case
the "thick_face") to zero.

.. math::

    I(q,\alpha) = \frac{\text{scale}}{V_s} F^2(q,\alpha).sin(\alpha) + \text{background}

where

.. math::

    F(q,\alpha) = &\ (\rho_c - \rho_s) V_c
           \frac{\sin \left( q \tfrac12 L\cos\alpha \right)}
                {q \tfrac12 L\cos\alpha}
           \frac{2 J_1 \left( qR\sin\alpha \right)}
                {qR\sin\alpha} \\
         &\ + (\rho_s - \rho_\text{solv}) V_s
           \frac{\sin \left( q \left(\tfrac12 L+T\right) \cos\alpha \right)}
                {q \left(\tfrac12 L +T \right) \cos\alpha}
           \frac{ 2 J_1 \left( q(R+T)\sin\alpha \right)}
                {q(R+T)\sin\alpha}

and

.. math::

    V_s = \pi (R + T)^2 (L + 2T)

and $\alpha$ is the angle between the axis of the cylinder and $\vec q$,
$V_s$ is the total volume (i.e. including both the core and the outer shell),
$V_c$ is the volume of the core, $L$ is the length of the core,
$R$ is the radius of the core, $T$ is the thickness of the shell, $\rho_c$
is the scattering length density of the core, $\rho_s$ is the scattering
length density of the shell, $\rho_\text{solv}$ is the scattering length
density of the solvent, and *background* is the background level.  The outer
radius of the shell is given by $R+T$ and the total length of the outer
shell is given by $L+2T$. $J_1$ is the first order Bessel function.

.. _core-shell-cylinder-geometry:

.. figure:: img/core_shell_cylinder_geometry.jpg

    Core shell cylinder schematic.

To provide easy access to the orientation of the core-shell cylinder, we
define the axis of the cylinder using two angles $\theta$ and $\phi$.
(see :ref:`cylinder model <cylinder-angle-definition>`)

NB: The 2nd virial coefficient of the cylinder is calculated based on
the radius and 2 length values, and used as the effective radius for
$S(q)$ when $P(q) \cdot S(q)$ is applied.

The $\theta$ and $\phi$ parameters are not used for the 1D output.

Reference
---------

See also Livsey [#Livsey1987]_ and Onsager [#Onsager1949]_.

.. [#Livsey1987] I Livsey, *J. Chem. Soc., Faraday Trans. 2*, 83 (1987) 1445-1452

.. [#Kline2006] S R Kline, *J Appl. Cryst.*, 39 (2006) 895

.. [#Onsager1949] L. Onsager, *Ann. New York Acad. Sci.*, 51 (1949) 627-659

Authorship and Verification
----------------------------

* **Author:** NIST IGOR/DANSE **Date:** pre 2010
* **Last Modified by:** Paul Kienzle **Date:** Aug 8, 2016
* **Last Reviewed by:** Richard Heenan **Date:** March 18, 2016
"""

import numpy as np
from numpy import pi, inf, sin, cos

name = "core_shell_cylinder"
title = "Right circular cylinder with a core-shell scattering length density profile."
description = """
P(q,alpha)= scale/Vs*f(q)^(2) + background,
      where: f(q)= 2(sld_core - solvant_sld)
        * Vc*sin[qLcos(alpha/2)]
        /[qLcos(alpha/2)]*J1(qRsin(alpha))
        /[qRsin(alpha)]+2(sld_shell-sld_solvent)
        *Vs*sin[q(L+T)cos(alpha/2)][[q(L+T)
        *cos(alpha/2)]*J1(q(R+T)sin(alpha))
        /q(R+T)sin(alpha)]

    alpha:is the angle between the axis of
        the cylinder and the q-vector
    Vs: the volume of the outer shell
    Vc: the volume of the core
    L: the length of the core
        sld_shell: the scattering length density of the shell
    sld_solvent: the scattering length density of the solvent
    background: the background
    T: the thickness
        R+T: is the outer radius
     L+2T: The total length of the outershell
    J1: the first order Bessel function
     theta: axis_theta of the cylinder
     phi: the axis_phi of the cylinder
"""
category = "shape:cylinder"

#             ["name", "units", default, [lower, upper], "type", "description"],
parameters = [["sld_core", "1e-6/Ang^2", 4, [-inf, inf], "sld",
               "Cylinder core scattering length density"],
              ["sld_shell", "1e-6/Ang^2", 4, [-inf, inf], "sld",
               "Cylinder shell scattering length density"],
              ["sld_solvent", "1e-6/Ang^2", 1, [-inf, inf], "sld",
               "Solvent scattering length density"],
              ["radius", "Ang", 20, [0, inf], "volume",
               "Cylinder core radius"],
              ["thickness", "Ang", 20, [0, inf], "volume",
               "Cylinder shell thickness"],
              ["length", "Ang", 400, [0, inf], "volume",
               "Cylinder length"],
              ["theta", "degrees", 60, [-360, 360], "orientation",
               "cylinder axis to beam angle"],
              ["phi", "degrees", 60, [-360, 360], "orientation",
               "rotation about beam"],
             ]

source = ["lib/polevl.c", "lib/sas_J1.c", "lib/gauss76.c", "core_shell_cylinder.c"]
have_Fq = True
radius_effective_modes = [
    "excluded volume", "equivalent volume sphere", "outer radius", "half outer length",
    "half min outer dimension", "half max outer dimension", "half outer diagonal",
    ]

def random():
    """Return a random parameter set for the model."""
    outer_radius = 10**np.random.uniform(1, 4.7)
    # Use a distribution with a preference for thin shell or thin core
    # Avoid core,shell radii < 1
    radius = np.random.beta(0.5, 0.5)*(outer_radius-2) + 1
    thickness = outer_radius - radius
    length = np.random.uniform(1, 4.7)
    pars = dict(
        radius=radius,
        thickness=thickness,
        length=length,
    )
    return pars

q = 0.1
# april 6 2017, rkh add unit tests, NOT compared with any other calc method, assume correct!
qx = q*cos(pi/6.0)
qy = q*sin(pi/6.0)
tests = [
    [{}, 0.075, 10.8552692237],
    [{}, (qx, qy), 0.444618752741],
]
del qx, qy  # not necessary to delete, but cleaner
