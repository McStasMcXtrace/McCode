# ellipsoid model
# Note: model title and parameter table are inserted automatically
r"""
The form factor is normalized by the particle volume

Definition
----------

The output of the 2D scattering intensity function for oriented ellipsoids
is given by (Feigin, 1987)

.. math::

    P(q,\alpha) = \frac{\text{scale}}{V} F^2(q,\alpha) + \text{background}

where

.. math::

    F(q,\alpha) = \Delta \rho V \frac{3(\sin qr  - qr \cos qr)}{(qr)^3}

for

.. math::

    r = \left[ R_e^2 \sin^2 \alpha + R_p^2 \cos^2 \alpha \right]^{1/2}


$\alpha$ is the angle between the axis of the ellipsoid and $\vec q$,
$V = (4/3)\pi R_pR_e^2$ is the volume of the ellipsoid, $R_p$ is the polar
radius along the rotational axis of the ellipsoid, $R_e$ is the equatorial
radius perpendicular to the rotational axis of the ellipsoid and
$\Delta \rho$ (contrast) is the scattering length density difference between
the scatterer and the solvent.

For randomly oriented particles use the orientational average,

.. math::

   \langle F^2(q) \rangle = \int_{0}^{\pi/2}{F^2(q,\alpha)\sin(\alpha)\,d\alpha}


computed via substitution of $u=\sin(\alpha)$, $du=\cos(\alpha)\,d\alpha$ as

.. math::

    \langle F^2(q) \rangle = \int_0^1{F^2(q, u)\,du}

with

.. math::

    r = R_e \left[ 1 + u^2\left(R_p^2/R_e^2 - 1\right)\right]^{1/2}

For 2d data from oriented ellipsoids the direction of the rotation axis of
the ellipsoid is defined using two angles $\theta$ and $\phi$ as for the
:ref:`cylinder orientation figure <cylinder-angle-definition>`.
For the ellipsoid, $\theta$ is the angle between the rotational axis
and the $z$ -axis in the $xz$ plane followed by a rotation by $\phi$
in the $xy$ plane, for further details of the calculation and angular
dispersions see :ref:`orientation`.

NB: The 2nd virial coefficient of the solid ellipsoid is calculated based
on the $R_p$ and $R_e$ values, and used as the effective radius for
$S(q)$ when $P(q) \cdot S(q)$ is applied.


The $\theta$ and $\phi$ parameters are not used for the 1D output.

Validation
----------

Validation of the code was done by comparing the output of the 1D model
to the output of the software provided by the NIST (Kline, 2006).

The implementation of the intensity for fully oriented ellipsoids was
validated by averaging the 2D output using a uniform distribution
$p(\theta,\phi) = 1.0$ and comparing with the output of the 1D calculation.


.. _ellipsoid-comparison-2d:

.. figure:: img/ellipsoid_comparison_2d.jpg

    Comparison of the intensity for uniformly distributed ellipsoids
    calculated from our 2D model and the intensity from the NIST SANS
    analysis software. The parameters used were: *scale* = 1.0,
    *radius_polar* = 20 |Ang|, *radius_equatorial* = 400 |Ang|,
    *contrast* = 3e-6 |Ang^-2|, and *background* = 0.0 |cm^-1|.

The discrepancy above $q$ = 0.3 |cm^-1| is due to the way the form factors
are calculated in the c-library provided by NIST. A numerical integration
has to be performed to obtain $P(q)$ for randomly oriented particles.
The NIST software performs that integration with a 76-point Gaussian
quadrature rule, which will become imprecise at high $q$ where the amplitude
varies quickly as a function of $q$. The SasView result shown has been
obtained by summing over 501 equidistant points. Our result was found
to be stable over the range of $q$ shown for a number of points higher
than 500.

Model was also tested against the triaxial ellipsoid model with equal major
and minor equatorial radii.  It is also consistent with the cylinder model
with polar radius equal to length and equatorial radius equal to radius.

References
----------

#. L. A. Feigin and D. I. Svergun. *Structure Analysis by Small-Angle X-Ray
   and Neutron Scattering*, Plenum Press, New York, 1987
#. A. Isihara. *J. Chem. Phys.*, 18 (1950) 1446-1449

Authorship and Verification
----------------------------

* **Author:** NIST IGOR/DANSE **Date:** pre 2010
* **Converted to sasmodels by:** Helen Park **Date:** July 9, 2014
* **Last Modified by:** Paul Kienzle **Date:** March 22, 2017
"""
from __future__ import division

import numpy as np
from numpy import inf, sin, cos, pi

name = "ellipsoid"
title = "Ellipsoid of revolution with uniform scattering length density."

description = """\
P(q.alpha)= scale*f(q)^2 + background, where f(q)= 3*(sld
        - sld_solvent)*V*[sin(q*r(Rp,Re,alpha))
        -q*r*cos(qr(Rp,Re,alpha))]
        /[qr(Rp,Re,alpha)]^3"

     r(Rp,Re,alpha)= [Re^(2)*(sin(alpha))^2
        + Rp^(2)*(cos(alpha))^2]^(1/2)

        sld: SLD of the ellipsoid
        sld_solvent: SLD of the solvent
        V: volume of the ellipsoid
        Rp: polar radius of the ellipsoid
        Re: equatorial radius of the ellipsoid
"""
category = "shape:ellipsoid"

# pylint: disable=bad-whitespace, line-too-long
#   ["name", "units", default, [lower, upper], "type","description"],
parameters = [
    ["sld",         "1e-6/Ang^2",   4, [-inf, inf], "sld",         "Ellipsoid scattering length density"],
    ["sld_solvent", "1e-6/Ang^2",   1, [-inf, inf], "sld",         "Solvent scattering length density"],
    ["radius_polar",       "Ang",  20, [0, inf],    "volume",      "Polar radius"],
    ["radius_equatorial",  "Ang", 400, [0, inf],    "volume",      "Equatorial radius"],
    ["theta",          "degrees",  60, [-360, 360], "orientation", "ellipsoid axis to beam angle"],
    ["phi",            "degrees",  60, [-360, 360], "orientation", "rotation about beam"],
    ]
# pylint: enable=bad-whitespace, line-too-long


source = ["lib/sas_3j1x_x.c", "lib/gauss76.c", "ellipsoid.c"]
have_Fq = True
radius_effective_modes = [
    "average curvature", "equivalent volume sphere", "min radius", "max radius",
    ]

def random():
    """Return a random parameter set for the model."""
    volume = 10**np.random.uniform(5, 12)
    radius_polar = 10**np.random.uniform(1.3, 4)
    radius_equatorial = np.sqrt(volume/radius_polar) # ignore 4/3 pi
    pars = dict(
        #background=0, sld=0, sld_solvent=1,
        radius_polar=radius_polar,
        radius_equatorial=radius_equatorial,
    )
    return pars

q = 0.1
# april 6 2017, rkh add unit tests, NOT compared with any other calc method, assume correct!
qx = q*cos(pi/6.0)
qy = q*sin(pi/6.0)
tests = [
    [{}, 0.05, 54.8525847025],
    [{'theta': 80., 'phi': 10.}, (qx, qy), 1.74134670026],

    # Test beta and the effective radius with the equivalent sphere (May 15, 2019)
    # Yun's matlab report [0.006,0.05,0.1], [330.0, 10.976, 1.369]
    # The values in the code here are calculated results from SasView (May 15, 2019)
    [{"@S": "hardsphere",
      "scale": 1., "background": 0., "volfraction": 0.2,
      "structure_factor_mode": 1,  # beta approx
      "radius_effective_mode": 2   # Reff "equivalent sphere"
     },
     [0.006, 0.05, 0.1],
     [330.0082676127404, 10.96932155837644, 1.35347369429977]],

    # Test beta and the effective radius with the minor radius (May 15, 2019)
    # Yun's matlab report [0.006,0.05,0.1], [273.67, 10.942, 1.3683]
    # The values in the code here are calculated results from SasView (May 15, 2019)
    [{"@S": "hardsphere",
      "scale": 1., "background": 0., "volfraction": 0.2,
      "structure_factor_mode": 1,  # beta approx
      "radius_effective_mode": 3   # Reff "equivalent sphere"
     },
     [0.006, 0.05, 0.1],
     [273.64522316236287, 10.93682961898512, 1.352864896244188]],

    # Test beta and the effective radius with the major radius (May 15, 2019)
    # Yun's matlab report [0.006,0.05,0.1], [1062.37, 10.977, 1.369]
    # The values in the code here are calculated results from SasView (May 15, 2019)
    [{"@S": "hardsphere",
      "scale": 1., "background": 0., "volfraction": 0.2,
      "structure_factor_mode": 1,  # beta approx
      "radius_effective_mode": 4   # Reff "equivalent sphere"
     },
     [0.006, 0.05, 0.1],
     [1062.3690121068357, 10.970147034298845, 1.3534794742102454]],

    # Test beta and the effective radius with the average curvature (May 15, 2019)
    # Effective radius is taken from the SasView calculation.
    # With default values, the effective radius is 270.745.
    # The calculated values using Yun's Matlab code are
    #     [0.006,0.05,0.1], [529.03,419.22, 10.977, 1.369]
    # The values in the code here are calculated results from SasView (May 15, 2019)
    [{"@S": "hardsphere",
      "scale": 1., "background": 0., "volfraction": 0.2,
      "structure_factor_mode": 1,  # beta approx
      "radius_effective_mode": 1   # Reff "equivalent sphere"
     },
     [0.006, 0.01, 0.05, 0.1],
     [529.0109355849872, 419.2280055002956, 10.970118278295908, 1.3534762023877278]]
]
del qx, qy  # not necessary to delete, but cleaner
