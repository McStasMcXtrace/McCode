# cylinder model
# Note: model title and parameter table are inserted automatically
r"""

For information about polarised and magnetic scattering, see
the :ref:`magnetism` documentation.

Definition
----------

The output of the 2D scattering intensity function for oriented cylinders is
given by (Guinier, 1955)

.. math::

    I(q,\alpha) = \frac{\text{scale}}{V} F^2(q,\alpha) + \text{background}

where

.. math::

    F(q,\alpha) = 2 (\Delta \rho) V
           \frac{\sin \left(\tfrac12 qL\cos\alpha \right)}
                {\tfrac12 qL \cos \alpha}
           \frac{J_1 \left(q R \sin \alpha\right)}{q R \sin \alpha}

and $\alpha$ is the angle between the axis of the cylinder and $\vec q$,
$V =\pi R^2L$ is the volume of the cylinder, $L$ is the length of the cylinder,
$R$ is the radius of the cylinder, and $\Delta\rho$ (contrast) is the
scattering length density difference between the scatterer and the solvent.
$J_1$ is the first order Bessel function.

For randomly oriented particles:

.. math::

    P(q)=F^2(q)=\int_{0}^{\pi/2}{F^2(q,\alpha)\sin(\alpha)d\alpha}


The output of the 1D scattering intensity function for randomly oriented
cylinders is thus given by

.. math::

    I(q) = \frac{\text{scale}}{V}
        \int_0^{\pi/2} F^2(q,\alpha) \sin \alpha\ d\alpha + \text{background}


NB: The 2nd virial coefficient of the cylinder is calculated based on the
radius and length values, and used as the effective radius for $S(q)$
when $P(q) \cdot S(q)$ is applied.

For 2d scattering from oriented cylinders, we define the direction of the
axis of the cylinder using two angles $\theta$ (note this is not the same as
the scattering angle used in q) and $\phi$. Those angles are defined in
:numref:`cylinder-angle-definition` , for further details see
:ref:`orientation`.

.. _cylinder-angle-definition:

.. figure:: img/cylinder_angle_definition.png

    Angles $\theta$ and $\phi$ orient the cylinder relative to the beam line
    coordinates, where the beam is along the $z$ axis. Rotation $\theta$,
    initially in the $xz$ plane, is carried out first, then rotation $\phi$
    about the $z$ axis. Orientation distributions are described as rotations
    about two perpendicular axes $\delta_1$ and $\delta_2$ in the frame of
    the cylinder itself, which when $\theta = \phi = 0$ are parallel to the
    $Y$ and $X$ axes.

.. figure:: img/cylinder_angle_projection.png

    Examples for oriented cylinders.

The $\theta$ and $\phi$ parameters to orient the cylinder only appear in the
model when fitting 2d data.

Validation
----------

Validation of the code was done by comparing the output of the 1D model
to the output of the software provided by the NIST (Kline, 2006).
The implementation of the intensity for fully oriented cylinders was done
by averaging over a uniform distribution of orientations using

.. math::

    P(q) = \int_0^{\pi/2} d\phi
        \int_0^\pi p(\theta) P_0(q,\theta) \sin \theta\ d\theta


where $p(\theta,\phi) = 1$ is the probability distribution for the orientation
and $P_0(q,\theta)$ is the scattering intensity for the fully oriented
system, and then comparing to the 1D result.

References
----------

#.  J. Pedersen, *Adv. Colloid Interface Sci.*, 70 (1997) 171-210
#.  G. Fournet, *Bull. Soc. Fr. Mineral. Cristallogr.*, 74 (1951) 39-113
#.  L. Onsager, *Ann. New York Acad. Sci.*, 51 (1949) 627-659

Authorship and Verification
----------------------------

* **Author:**
* **Last Modified by:** Paul Butler (docs only) November 10, 2022
* **Last Reviewed by:**
"""

import numpy as np  # type: ignore
from numpy import pi, inf  # type: ignore

name = "cylinder"
title = "Right circular cylinder with uniform scattering length density."
description = """
     f(q,alpha) = 2*(sld - sld_solvent)*V*sin(qLcos(alpha)/2))
                /[qLcos(alpha)/2]*J1(qRsin(alpha))/[qRsin(alpha)]

            P(q,alpha)= scale/V*f(q,alpha)^(2)+background
            V: Volume of the cylinder
            R: Radius of the cylinder
            L: Length of the cylinder
            J1: The Bessel function
            alpha: angle between the axis of the
            cylinder and the q-vector for 1D
            :the ouput is P(q)=scale/V*integral
            from pi/2 to zero of...
            f(q,alpha)^(2)*sin(alpha)*dalpha + background
"""
category = "shape:cylinder"

# pylint: disable=bad-whitespace, line-too-long
#             [ "name", "units", default, [lower, upper], "type", "description"],
parameters = [
    ["sld",         "1e-6/Ang^2",   4, [-inf, inf], "sld",         "Cylinder scattering length density"],
    ["sld_solvent", "1e-6/Ang^2",   1, [-inf, inf], "sld",         "Solvent scattering length density"],
    ["radius",      "Ang",         20, [0, inf],    "volume",      "Cylinder radius"],
    ["length",      "Ang",        400, [0, inf],    "volume",      "Cylinder length"],
    ["theta",       "degrees",     60, [-360, 360], "orientation", "cylinder axis to beam angle"],
    ["phi",         "degrees",     60, [-360, 360], "orientation", "rotation about beam"],
    ]
# pylint: enable=bad-whitespace, line-too-long

source = ["lib/polevl.c", "lib/sas_J1.c", "lib/gauss76.c", "cylinder.c"]
valid = "radius >= 0.0 && length >= 0.0"
have_Fq = True
radius_effective_modes = [
    "excluded volume", "equivalent volume sphere", "radius",
    "half length", "half min dimension", "half max dimension", "half diagonal",
    ]

def random():
    """Return a random parameter set for the model."""
    volume = 10**np.random.uniform(5, 12)
    length = 10**np.random.uniform(-2, 2)*volume**0.333
    radius = np.sqrt(volume/length/np.pi)
    pars = dict(
        #scale=1,
        #background=0,
        length=length,
        radius=radius,
    )
    return pars


# Test 1-D and 2-D models
qx, qy = 0.2 * np.cos(2.5), 0.2 * np.sin(2.5)
theta, phi = 80.1534480601659, 10.1510817110481  # (10, 10) in sasview 3.x
tests = [
    [{}, 0.2, 0.042761386790780453],
    [{}, [0.2], [0.042761386790780453]],
    [{"scale": 1., "background": 0.}, [0.01, 0.05, 0.2],
     # these numerical results NOT independently verified
     [3.01823887e+02, 5.36661653e+01, 4.17613868e-02]],
    [{"scale": 1., "background": 0.},
     # the longer list here checks  F1, F2=I(Q)*V, R_eff, volume, volume_ratio
     0.05, 2214.9614083, 26975556.88749548, 73.34013315261608,
     502654.8245743669, 1.0],
#2345678901234567890123456789012345678901234567890123456789012345678901234567890
    [{"@S": "hardsphere",         # MONODISPERSE
      "scale": 5., "background": 0., "volfraction": 0.2,
      "structure_factor_mode": 0,  # normal decoupling approx
      "radius_effective_mode": 1,  # Reff "excluded volume"
     }, [0.01, 0.05, 0.2], [7.35571916e+01, 5.78147797e+01, 4.15623248e-2]
     ],
    [{"@S": "hardsphere",
      "scale": 5., "background": 0., "volfraction": 0.2,
      "structure_factor_mode": 1,  # beta approx
      "radius_effective_mode": 1,  # Reff "excluded volume"
     }, [0.01, 0.05, 0.2], [8.29729770e+01, 5.44206752e+01, 4.17598382e-2]
     ],
    [{"@S": "hardsphere",         # POLYDISPERSE
      "scale": 5., "background": 0., "volfraction": 0.2,
      "radius_pd": 0.2, "radius_pd_n": 15, "length_pd": 0.1,
      "structure_factor_mode": 0,  # normal decoupling approx
      "radius_effective_mode": 1,  # Reff "excluded volume"
     }, [0.01, 0.05, 0.2], [87.50350037, 63.95202427, 0.27889988]
     ],
    [{"@S": "hardsphere",
      "scale": 5., "background": 0., "volfraction": 0.2,
      "radius_pd": 0.2, "radius_pd_n": 15, "length_pd": 0.1,
      "structure_factor_mode": 1,  # beta approx
      "radius_effective_mode": 1,  # Reff "excluded volume"
     }, [0.01, 0.05, 0.2], [132.2101165, 59.89948174, 0.28048784]
     ],
    #
    [{'theta': theta, 'phi': phi}, (qx, qy), 0.03514647218513852],
    [{'theta': theta, 'phi': phi}, [(qx, qy)], [0.03514647218513852]],
]
del qx, qy, theta, phi  # not necessary to delete, but cleaner

def _extend_with_reff_tests(radius, length):
    """Test R_eff and form volume calculations"""
    # V and Vr are the same for each R_eff mode
    V = pi*radius**2*length  # shell volume = form volume for solid objects
    Vr = 1.0  # form:shell volume ratio
    # Use test value for I(0.2) from above to check Fsq value.  Need to
    # remove scale and background before testing.
    q = 0.2
    scale, background = V, 0.001
    Fsq = (0.042761386790780453 - background)*scale
    F = None  # Need target value for <F>
    # Various values for R_eff, depending on mode
    r_effs = [
        0.,
        0.5*(0.75*radius*(2.0*radius*length
                          + (radius + length)*(pi*radius + length)))**(1./3.),
        (0.75*radius**2*length)**(1./3.),
        radius,
        length/2.,
        min(radius, length/2.),
        max(radius, length/2.),
        np.sqrt(4*radius**2 + length**2)/2.,
    ]
    tests.extend([
        ({'radius_effective_mode': 0}, q, F, Fsq, r_effs[0], V, Vr),
        ({'radius_effective_mode': 1}, q, F, Fsq, r_effs[1], V, Vr),
        ({'radius_effective_mode': 2}, q, F, Fsq, r_effs[2], V, Vr),
        ({'radius_effective_mode': 3}, q, F, Fsq, r_effs[3], V, Vr),
        ({'radius_effective_mode': 4}, q, F, Fsq, r_effs[4], V, Vr),
        ({'radius_effective_mode': 5}, q, F, Fsq, r_effs[5], V, Vr),
        ({'radius_effective_mode': 6}, q, F, Fsq, r_effs[6], V, Vr),
        ({'radius_effective_mode': 7}, q, F, Fsq, r_effs[7], V, Vr),
    ])

# Test Reff and volume with default model parameters
_extend_with_reff_tests(parameters[2][2], parameters[3][2])
del _extend_with_reff_tests

# ADDED by:  RKH  ON: 18Mar2016 renamed sld's etc
