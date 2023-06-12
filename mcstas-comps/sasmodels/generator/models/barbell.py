r"""
Definition
----------

Calculates the scattering from a barbell-shaped cylinder.  Like
:ref:`capped-cylinder`, this is a spherocylinder with spherical end
caps that have a radius larger than that of the cylinder, but with the center
of the end cap radius lying outside of the cylinder. See the diagram for
the details of the geometry and restrictions on parameter values.

.. figure:: img/barbell_geometry.jpg

    Barbell geometry, where $r$ is *radius*, $R$ is *radius_bell* and
    $L$ is *length*. Since the end cap radius $R \geq r$ and by definition
    for this geometry $h \ge 0$, $h$ is then defined by $r$ and $R$ as
    $h = \sqrt{R^2 - r^2}$

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
all orientations. $\left<A^2(q,\alpha)\right>$ is then the form factor, $P(q)$.
The scale factor is equivalent to the volume fraction of cylinders, each of
volume, $V$. Contrast $\Delta\rho$ is the difference of scattering length
densities of the cylinder and the surrounding solvent.

The volume of the barbell is

.. math::

    V = \pi r_c^2 L + 2\pi\left(\tfrac23R^3 + R^2h-\tfrac13h^3\right)

and its radius of gyration is

.. math::

    R_g^2 =&\ \left[ \tfrac{12}{5}R^4
        + R^3\left(3L + \tfrac{18}{5} h\right)
        + R^2\left(L^2 + Lh + \tfrac25 h^2\right)
        + R\left(\tfrac14 L^3 + \tfrac12 L^2h - Lh^2\right) \right. \\
        &\ \left. + Lh^4 - \tfrac12 L^2h^3 - \tfrac14 L^3h + \tfrac25 h^4\right]
        \left( 4R^2 + 3LR + 2Rh - 3Lh - 2h^2\right)^{-1}

.. note::
    The requirement that $R \geq r$ is not enforced in the model! It is
    up to you to restrict this during analysis.

The 2D scattering intensity is calculated similar to the 2D cylinder model.

.. figure:: img/cylinder_angle_definition.png

    Definition of the angles for oriented 2D barbells.


References
----------

#. H Kaya, *J. Appl. Cryst.*, 37 (2004) 223-230

#. H Kaya and N R deSouza, *J. Appl. Cryst.*, 37 (2004) 508-509
   (addenda and errata)

#. L. Onsager, *Ann. New York Acad. Sci.*, 51 (1949) 627-659

Authorship and Verification
----------------------------

* **Author:** NIST IGOR/DANSE **Date:** pre 2010
* **Last Modified by:** Paul Butler **Date:** March 20, 2016
* **Last Reviewed by:** Richard Heenan **Date:** January 4, 2017
"""

import numpy as np
from numpy import inf, sin, cos, pi

name = "barbell"
title = "Cylinder with spherical end caps"
description = """
    Calculates the scattering from a barbell-shaped cylinder.
    That is a sphereocylinder with spherical end caps that have a radius larger
    than that of the cylinder and the center of the end cap radius lies outside
    of the cylinder.
    Note: As the length of cylinder(bar) -->0,it becomes a dumbbell. And when
    rad_bar = rad_bell, it is a spherocylinder.
    It must be that rad_bar <(=) rad_bell.
"""
category = "shape:cylinder"
# pylint: disable=bad-whitespace, line-too-long
#   ["name", "units", default, [lower, upper], "type","description"],
parameters = [
    ["sld",         "1e-6/Ang^2",   4, [-inf, inf], "sld",         "Barbell scattering length density"],
    ["sld_solvent", "1e-6/Ang^2",   1, [-inf, inf], "sld",         "Solvent scattering length density"],
    ["radius_bell", "Ang",         40, [0, inf],    "volume",      "Spherical bell radius"],
    ["radius",      "Ang",         20, [0, inf],    "volume",      "Cylindrical bar radius"],
    ["length",      "Ang",        400, [0, inf],    "volume",      "Cylinder bar length"],
    ["theta",       "degrees",     60, [-360, 360], "orientation", "Barbell axis to beam angle"],
    ["phi",         "degrees",     60, [-360, 360], "orientation", "Rotation about beam"],
    ]
# pylint: enable=bad-whitespace, line-too-long

source = ["lib/polevl.c", "lib/sas_J1.c", "lib/gauss76.c", "barbell.c"]
valid = "radius_bell >= radius"
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
        radius_bell=bell_radius,
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
    [{}, 0.075, 25.5691260532],
    [{'theta':80., 'phi':10.}, (qx, qy), 3.04233067789],
]
del qx, qy  # not necessary to delete, but cleaner
