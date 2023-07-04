r"""
Definition
----------

This model provides the form factor for a circular cylinder with a
core-shell scattering length density profile. Thus this is a variation
of a core-shell cylinder or disc where the shell on the walls and ends
may be of different thicknesses and scattering length densities. The form
factor is normalized by the particle volume.


.. figure:: img/core_shell_bicelle_geometry.png

    Schematic cross-section of bicelle. Note however that the model here
    calculates for rectangular, not curved, rims as shown below.

.. figure:: img/core_shell_bicelle_parameters.png

   Cross section of cylindrical symmetry model used here. Users will have
   to decide how to distribute "heads" and "tails" between the rim, face
   and core regions in order to estimate appropriate starting parameters.

Given the scattering length densities (sld) $\rho_c$, the core sld, $\rho_f$,
the face sld, $\rho_r$, the rim sld and $\rho_s$ the solvent sld, the
scattering length density variation along the cylinder axis is:

.. math::

    \rho(r) =
      \begin{cases}
      &\rho_c \text{ for } 0 \lt r \lt R; -L \lt z\lt L \\[1.5ex]
      &\rho_f \text{ for } 0 \lt r \lt R; -(L+2t) \lt z\lt -L;
      L \lt z\lt (L+2t) \\[1.5ex]
      &\rho_r\text{ for } 0 \lt r \lt R; -(L+2t) \lt z\lt -L; L \lt z\lt (L+2t)
      \end{cases}

The form factor for the bicelle is calculated in cylindrical coordinates, where
$\alpha$ is the angle between the $Q$ vector and the cylinder axis, to give:

.. math::

    I(Q,\alpha) = \frac{\text{scale}}{V_t} \cdot
        F(Q,\alpha)^2 \cdot sin(\alpha) + \text{background}

where

.. math::
    :nowrap:

    \begin{align*}
    F(Q,\alpha) = &\bigg[
    (\rho_c - \rho_f) V_c
     \frac{2J_1(QRsin \alpha)}{QRsin\alpha}
     \frac{sin(QLcos\alpha/2)}{Q(L/2)cos\alpha} \\
    &+(\rho_f - \rho_r) V_{c+f}
     \frac{2J_1(QRsin\alpha)}{QRsin\alpha}
     \frac{sin(Q(L/2+t_f)cos\alpha)}{Q(L/2+t_f)cos\alpha} \\
    &+(\rho_r - \rho_s) V_t
     \frac{2J_1(Q(R+t_r)sin\alpha)}{Q(R+t_r)sin\alpha}
     \frac{sin(Q(L/2+t_f)cos\alpha)}{Q(L/2+t_f)cos\alpha}
    \bigg]
    \end{align*}

where $V_t$ is the total volume of the bicelle, $V_c$ the volume of the core,
$V_{c+f}$ the volume of the core plus the volume of the faces, $R$ is the radius
of the core, $L$ the length of the core, $t_f$ the thickness of the face, $t_r$
the thickness of the rim and $J_1$ the usual first order Bessel function.

The output of the 1D scattering intensity function for randomly oriented
cylinders is then given by integrating over all possible $\theta$ and $\phi$.

For oriented bicelles the *theta*, and *phi* orientation parameters will appear
when fitting 2D data, see the :ref:`cylinder` model for further information.
Our implementation of the scattering kernel and the 1D scattering intensity
use the c-library from NIST.

.. figure:: img/cylinder_angle_definition.png

    Definition of the angles for the oriented core shell bicelle model,
    note that the cylinder axis of the bicelle starts along the beam direction
    when $\theta  = \phi = 0$.


References
----------

#. D Singh (2009). *Small angle scattering studies of self assembly in
   lipid mixtures*, John's Hopkins University Thesis (2009) 223-225. `Available
   from Proquest <http://search.proquest.com/docview/304915826>`_

#.  L. Onsager, *Ann. New York Acad. Sci.*, 51 (1949) 627-659

Authorship and Verification
----------------------------

* **Author:** NIST IGOR/DANSE **Date:** pre 2010
* **Last Modified by:** Paul Butler **Date:** September 30, 2016
* **Last Reviewed by:** Richard Heenan **Date:** January 4, 2017
"""

import numpy as np
from numpy import inf, sin, cos, pi

name = "core_shell_bicelle"
title = "Circular cylinder with a core-shell scattering length density profile.."
description = """
    P(q,alpha)= (scale/Vs)*f(q)^(2) + bkg,  where:
    f(q)= Vt(sld_rim - sld_solvent)* sin[qLt.cos(alpha)/2]
    /[qLt.cos(alpha)/2]*J1(qRout.sin(alpha))
    /[qRout.sin(alpha)]+
    (sld_core-sld_face)*Vc*sin[qLcos(alpha)/2][[qL
    *cos(alpha)/2]*J1(qRc.sin(alpha))
    /qRc.sin(alpha)]+
    (sld_face-sld_rim)*(Vc+Vf)*sin[q(L+2.thick_face).
    cos(alpha)/2][[q(L+2.thick_face)*cos(alpha)/2]*
    J1(qRc.sin(alpha))/qRc.sin(alpha)]

    alpha:is the angle between the axis of
    the cylinder and the q-vector
    Vt = pi.(Rc + thick_rim)^2.Lt : total volume
    Vc = pi.Rc^2.L :the volume of the core
    Vf = 2.pi.Rc^2.thick_face
    Rc = radius: is the core radius
    L: the length of the core
    Lt = L + 2.thick_face: total length
    Rout = radius + thick_rim
    sld_core, sld_rim, sld_face:scattering length
    densities within the particle
    sld_solvent: the scattering length density
    of the solvent
    bkg: the background
    J1: the first order Bessel function
    theta: axis_theta of the cylinder
    phi: the axis_phi of the cylinder...
        """
category = "shape:cylinder"

# pylint: disable=bad-whitespace, line-too-long
#             ["name", "units", default, [lower, upper], "type", "description"],
parameters = [
    ["radius",         "Ang",       80, [0, inf],    "volume",      "Cylinder core radius"],
    ["thick_rim",  "Ang",       10, [0, inf],    "volume",      "Rim shell thickness"],
    ["thick_face", "Ang",       10, [0, inf],    "volume",      "Cylinder face thickness"],
    ["length",         "Ang",      50, [0, inf],    "volume",      "Cylinder length"],
    ["sld_core",       "1e-6/Ang^2", 1, [-inf, inf], "sld",         "Cylinder core scattering length density"],
    ["sld_face",       "1e-6/Ang^2", 4, [-inf, inf], "sld",         "Cylinder face scattering length density"],
    ["sld_rim",        "1e-6/Ang^2", 4, [-inf, inf], "sld",         "Cylinder rim scattering length density"],
    ["sld_solvent",    "1e-6/Ang^2", 1, [-inf, inf], "sld",         "Solvent scattering length density"],
    ["theta",          "degrees",   90, [-360, 360], "orientation", "cylinder axis to beam angle"],
    ["phi",            "degrees",    0, [-360, 360], "orientation", "rotation about beam"]
    ]

# pylint: enable=bad-whitespace, line-too-long

source = ["lib/sas_Si.c", "lib/polevl.c", "lib/sas_J1.c", "lib/gauss76.c",
          "core_shell_bicelle.c"]
have_Fq = True
radius_effective_modes = [
    "excluded volume", "equivalent volume sphere", "outer rim radius",
    "half outer thickness", "half diagonal",
    ]

def random():
    """Return a random parameter set for the model."""
    pars = dict(
        radius=10**np.random.uniform(1.3, 3),
        length=10**np.random.uniform(1.3, 4),
        thick_rim=10**np.random.uniform(0, 1.7),
        thick_face=10**np.random.uniform(0, 1.7),
    )
    return pars

q = 0.1
# april 6 2017, rkh add unit tests, NOT compared with any other calc method, assume correct!
qx = q*cos(pi/6.0)
qy = q*sin(pi/6.0)
tests = [
    [{}, 0.05, 7.4883545957],
    [{'theta':80., 'phi':10.}, (qx, qy), 2.81048892474]
]
del qx, qy  # not necessary to delete, but cleaner
