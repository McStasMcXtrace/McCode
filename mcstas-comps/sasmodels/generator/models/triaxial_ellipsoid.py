# triaxial ellipsoid model
# Note: model title and parameter table are inserted automatically
r"""
Definition
----------

.. figure:: img/triaxial_ellipsoid_geometry.jpg

    Ellipsoid with $R_a$ as *radius_equat_minor*, $R_b$ as *radius_equat_major*
    and $R_c$ as *radius_polar*.

Given an ellipsoid

.. math::

    \frac{X^2}{R_a^2} + \frac{Y^2}{R_b^2} + \frac{Z^2}{R_c^2} = 1

the scattering for randomly oriented particles is defined by the average over
all orientations $\Omega$ of:

.. math::

    P(q) = \text{scale}(\Delta\rho)^2\frac{V}{4 \pi}\int_\Omega\Phi^2(qr)\,d\Omega
           + \text{background}

where

.. math::

    \Phi(qr) &= 3 j_1(qr)/qr = 3 (\sin qr - qr \cos qr)/(qr)^3 \\
    r^2 &= R_a^2e^2 + R_b^2f^2 + R_c^2g^2 \\
    V &= \tfrac{4}{3} \pi R_a R_b R_c

The $e$, $f$ and $g$ terms are the projections of the orientation vector on $X$,
$Y$ and $Z$ respectively.  Keeping the orientation fixed at the canonical
axes, we can integrate over the incident direction using polar angle
$-\pi/2 \le \gamma \le \pi/2$ and equatorial angle $0 \le \phi \le 2\pi$
(as defined in ref [1]),

 .. math::

     \langle\Phi^2\rangle = \int_0^{2\pi} \int_{-\pi/2}^{\pi/2} \Phi^2(qr)
                                                \cos \gamma\,d\gamma d\phi

with $e = \cos\gamma \sin\phi$, $f = \cos\gamma \cos\phi$ and $g = \sin\gamma$.
A little algebra yields

.. math::

    r^2 = b^2(p_a \sin^2 \phi \cos^2 \gamma + 1 + p_c \sin^2 \gamma)

for

.. math::

    p_a = \frac{a^2}{b^2} - 1 \text{ and } p_c = \frac{c^2}{b^2} - 1

Due to symmetry, the ranges can be restricted to a single quadrant
$0 \le \gamma \le \pi/2$ and $0 \le \phi \le \pi/2$, scaling the resulting
integral by 8. The computation is done using the substitution $u = \sin\gamma$,
$du = \cos\gamma\,d\gamma$, giving

.. math::

    \langle\Phi^2\rangle &= 8 \int_0^{\pi/2} \int_0^1 \Phi^2(qr) du d\phi \\
    r^2 &= b^2(p_a \sin^2(\phi)(1 - u^2) + 1 + p_c u^2)

Though for convenience we describe the three radii of the ellipsoid as
equatorial and polar, they may be given in $any$ size order. To avoid
multiple solutions, especially with Monte-Carlo fit methods, it may be
advisable to restrict their ranges. For typical small angle diffraction
situations there may be a number of closely similar "best fits", so some
trial and error, or fixing of some radii at expected values, may help.

To provide easy access to the orientation of the triaxial ellipsoid, we
define the axis of the cylinder using the angles $\theta$, $\phi$ and $\psi$.
These angles are defined analogously to the elliptical_cylinder below, note
that angle $\phi$ is now NOT the same as in the equations above.

.. figure:: img/elliptical_cylinder_angle_definition.png

    Definition of angles for oriented triaxial ellipsoid, where radii are for
    illustration here $a < b << c$ and angle $\Psi$ is a rotation around the
    axis of the particle.

For oriented ellipsoids the *theta*, *phi* and *psi* orientation parameters
will appear when fitting 2D data, see the :ref:`elliptical-cylinder` model
for further information.

.. _triaxial-ellipsoid-angles:

.. figure:: img/triaxial_ellipsoid_angle_projection.png

    Some examples for an oriented triaxial ellipsoid.

The radius-of-gyration for this system is  $R_g^2 = (R_a R_b R_c)^2/5$.

The contrast $\Delta\rho$ is defined as SLD(ellipsoid) - SLD(solvent).  In the
parameters, $R_a$ is the minor equatorial radius, $R_b$ is the major
equatorial radius, and $R_c$ is the polar radius of the ellipsoid.

NB: The 2nd virial coefficient of the triaxial solid ellipsoid is calculated
after sorting the three radii to give the most appropriate prolate or oblate
form, from the new polar radius $R_p = R_c$ and effective equatorial radius,
$R_e = \sqrt{R_a R_b}$, to then be used as the effective radius for $S(q)$
when $P(q) \cdot S(q)$ is applied.

Validation
----------

Validation of our code was done by comparing the output of the
1D calculation to the angular average of the output of 2D calculation
over all possible angles.


References
----------

#. Finnigan, J.A., Jacobs, D.J., 1971. *Light scattering by ellipsoidal
   particles in solution*, J. Phys. D: Appl. Phys. 4, 72-77.
   doi:10.1088/0022-3727/4/1/310

Authorship and Verification
----------------------------

* **Author:** NIST IGOR/DANSE **Date:** pre 2010
* **Last Modified by:** Paul Kienzle (improved calculation) **Date:** April 4, 2017
* **Last Reviewed by:** Paul Kienzle & Richard Heenan **Date:**  April 4, 2017
"""

import numpy as np
from numpy import inf, sin, cos, pi

name = "triaxial_ellipsoid"
title = "Ellipsoid of uniform scattering length density with three independent axes."

description = """
   Triaxial ellipsoid - see main documentation.
"""
category = "shape:ellipsoid"

#             ["name", "units", default, [lower, upper], "type","description"],
parameters = [["sld", "1e-6/Ang^2", 4, [-inf, inf], "sld",
               "Ellipsoid scattering length density"],
              ["sld_solvent", "1e-6/Ang^2", 1, [-inf, inf], "sld",
               "Solvent scattering length density"],
              ["radius_equat_minor", "Ang", 20, [0, inf], "volume",
               "Minor equatorial radius, Ra"],
              ["radius_equat_major", "Ang", 400, [0, inf], "volume",
               "Major equatorial radius, Rb"],
              ["radius_polar", "Ang", 10, [0, inf], "volume",
               "Polar radius, Rc"],
              ["theta", "degrees", 60, [-360, 360], "orientation",
               "polar axis to beam angle"],
              ["phi", "degrees", 60, [-360, 360], "orientation",
               "rotation about beam"],
              ["psi", "degrees", 60, [-360, 360], "orientation",
               "rotation about polar axis"],
             ]

source = ["lib/sas_3j1x_x.c", "lib/gauss76.c", "triaxial_ellipsoid.c"]
# Equations do not require Ra <= Rb <= Rc so don't test for it.
#valid = ("radius_equat_minor <= radius_equat_major"
#         " && radius_equat_major <= radius_polar")
have_Fq = True
radius_effective_modes = [
    "equivalent biaxial ellipsoid average curvature",
    "equivalent volume sphere", "min radius", "max radius",
    ]

def random():
    """Return a random parameter set for the model."""
    a, b, c = 10**np.random.uniform(1, 4.7, size=3)
    pars = dict(
        radius_equat_minor=a,
        radius_equat_major=b,
        radius_polar=c,
    )
    return pars

q = 0.1
# april 6 2017, rkh add unit tests
#     NOT compared with any other calc method, assume correct!
# check 2d test after pull #890
qx = q*cos(pi/6.0)
qy = q*sin(pi/6.0)
tests = [
    [{}, 0.05, 24.8839548033],
    [{'theta':80., 'phi':10.}, (qx, qy), 166.712060266],
    ]
del qx, qy  # not necessary to delete, but cleaner
