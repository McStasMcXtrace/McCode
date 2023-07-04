r"""
Definitions
-----------

Calculates the scattering from a cylinder with spherical section end-caps.
Like :ref:`barbell`, this is a sphereocylinder with end caps that have a
radius larger than that of the cylinder, but with the center of the end cap
radius lying within the cylinder. This model simply becomes a convex
lens when the length of the cylinder $L=0$. See the diagram for the details
of the geometry and restrictions on parameter values.

.. figure:: img/capped_cylinder_geometry.jpg

    Capped cylinder geometry, where $r$ is *radius*, $R$ is *radius_cap* and
    $L$ is *length*. Since the end cap radius $R \geq r$ and by definition
    for this geometry $h \le 0$, $h$ is then defined by $r$ and $R$ as
    $h = -\sqrt{R^2 - r^2}$

The scattered intensity $I(q)$ is calculated as

.. math::

    I(q) = \frac{\Delta \rho^2}{V} \left<A^2(q,\alpha).sin(\alpha)\right>

where the amplitude $A(q,\alpha)$ with the rod axis at angle $\alpha$ to $q$
is given as

.. math::

    A(q) =&\ \pi r^2L
        \frac{\sin\left(\tfrac12 qL\cos\alpha\right)}
            {\tfrac12 qL\cos\alpha}
        \frac{2 J_1(qr\sin\alpha)}{qr\sin\alpha} \\
        &\ + 4 \pi R^3 \int_{-h/R}^1 dt
        \cos\left[ q\cos\alpha
            \left(Rt + h + {\tfrac12} L\right)\right]
        \times (1-t^2)
        \frac{J_1\left[qR\sin\alpha \left(1-t^2\right)^{1/2}\right]}
             {qR\sin\alpha \left(1-t^2\right)^{1/2}}

The $\left<\ldots\right>$ brackets denote an average of the structure over
all orientations. $\left< A^2(q)\right>$ is then the form factor, $P(q)$.
The scale factor is equivalent to the volume fraction of cylinders, each of
volume, $V$. Contrast $\Delta\rho$ is the difference of scattering length
densities of the cylinder and the surrounding solvent.

The volume of the capped cylinder is (with $h$ as a positive value here)

.. math::

    V = \pi r_c^2 L + 2\pi\left(\tfrac23R^3 + R^2h - \tfrac13h^3\right)

and its radius of gyration is

.. math::

    R_g^2 =&\ \left[ \tfrac{12}{5}R^5
        + R^4\left(6h+\tfrac32 L\right)
        + R^3\left(4h^2 + L^2 + 4Lh\right)
        + R^2\left(3Lh^2 + \tfrac32 L^2h\right) \right. \\
        &\ \left. + \tfrac25 h^5 - \tfrac12 Lh^4 - \tfrac12 L^2h^3
        + \tfrac14 L^3r^2 + \tfrac32 Lr^4 \right]
        \left( 4R^3 + 6R^2h - 2h^3 + 3r^2L \right)^{-1}


.. note::

    The requirement that $R \geq r$ is not enforced in the model!
    It is up to you to restrict this during analysis.

The 2D scattering intensity is calculated similar to the 2D cylinder model.

.. figure:: img/cylinder_angle_definition.png

    Definition of the angles for oriented 2D cylinders.


References
----------

#. H Kaya, *J. Appl. Cryst.*, 37 (2004) 223-230

#. H Kaya and N R deSouza, *J. Appl. Cryst.*, 37 (2004) 508-509
   (addenda and errata)

#. L. Onsager, *Ann. New York Acad. Sci.*, 51 (1949) 627-659

Authorship and Verification
----------------------------

* **Author:** NIST IGOR/DANSE **Date:** pre 2010
* **Last Modified by:** Paul Butler **Date:** September 30, 2016
* **Last Reviewed by:** Richard Heenan **Date:** January 4, 2017
"""

import numpy as np
from numpy import inf, sin, cos, pi

name = "capped_cylinder"
title = "Right circular cylinder with spherical end caps and uniform SLD"
description = """That is, a sphereocylinder
    with end caps that have a radius larger than
    that of the cylinder and the center of the
    end cap radius lies within the cylinder.
    Note: As the length of cylinder -->0,
    it becomes a Convex Lens.
    It must be that radius <(=) radius_cap.
    [Parameters];
    scale: volume fraction of spheres,
    background:incoherent background,
    radius: radius of the cylinder,
    length: length of the cylinder,
    radius_cap: radius of the semi-spherical cap,
    sld: SLD of the capped cylinder,
    sld_solvent: SLD of the solvent.
"""
category = "shape:cylinder"
# pylint: disable=bad-whitespace, line-too-long
#             ["name", "units", default, [lower, upper], "type", "description"],
parameters = [["sld",         "1e-6/Ang^2", 4, [-inf, inf], "sld",    "Cylinder scattering length density"],
              ["sld_solvent", "1e-6/Ang^2", 1, [-inf, inf], "sld",    "Solvent scattering length density"],
              ["radius",      "Ang",       20, [0, inf],    "volume", "Cylinder radius"],

              # TODO: use an expression for cap radius with fixed bounds.
              # The current form requires cap radius R bigger than cylinder radius r.
              # Could instead use R/r in [1,inf], r/R in [0,1], or the angle between
              # cylinder and cap in [0,90].  The problem is similar for the barbell
              # model.  Propose r/R in [0,1] in both cases, with the model specifying
              # cylinder radius in the capped cylinder model and sphere radius in the
              # barbell model.  This leads to the natural value of zero for no cap
              # in the capped cylinder, and zero for no bar in the barbell model.  In
              # both models, one would be a pill.
              ["radius_cap", "Ang",     20, [0, inf],    "volume", "Cap radius"],
              ["length",     "Ang",    400, [0, inf],    "volume", "Cylinder length"],
              ["theta",      "degrees", 60, [-360, 360], "orientation", "cylinder axis to beam angle"],
              ["phi",        "degrees", 60, [-360, 360], "orientation", "rotation about beam"],
             ]
# pylint: enable=bad-whitespace, line-too-long

source = ["lib/polevl.c", "lib/sas_J1.c", "lib/gauss76.c", "capped_cylinder.c"]
valid = "radius_cap >= radius"
have_Fq = True
radius_effective_modes = [
    "equivalent cylinder excluded volume", "equivalent volume sphere",
    "radius", "half length", "half total length",
    ]

def random():
    """Return a random parameter set for the model."""
    # TODO: increase volume range once problem with bell radius is fixed
    # The issue is that bell radii of more than about 200 fail at high q
    volume = 10**np.random.uniform(7, 9)
    bar_volume = 10**np.random.uniform(-4, -1)*volume
    bell_volume = volume - bar_volume
    bell_radius = (bell_volume/6)**0.3333  # approximate
    min_bar = bar_volume/np.pi/bell_radius**2
    bar_length = 10**np.random.uniform(0, 3)*min_bar
    bar_radius = np.sqrt(bar_volume/bar_length/np.pi)
    if bar_radius > bell_radius:
        bell_radius, bar_radius = bar_radius, bell_radius
    pars = dict(
        #background=0,
        radius_cap=bell_radius,
        radius=bar_radius,
        length=bar_length,
    )
    return pars


q = 0.1
# 2017-04-06: rkh add unit tests, NOT compared with any other calc method, assume correct!
# 2019-05-17: pak added barbell/capped cylinder to realspace sampling tests
qx = q*cos(pi/6.0)
qy = q*sin(pi/6.0)
tests = [
    [{}, 0.075, 26.0698570695],
    [{'theta':80., 'phi':10.}, (qx, qy), 0.561811990502],
]
del qx, qy  # not necessary to delete, but cleaner
