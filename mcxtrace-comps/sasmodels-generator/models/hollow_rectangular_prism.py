# rectangular_prism model
# Note: model title and parameter table are inserted automatically
r"""
Definition
----------

This model provides the form factor, $P(q)$, for a hollow rectangular
parallelepiped with a wall of thickness $\Delta$. The 1D scattering intensity
for this model is calculated by forming the difference of the amplitudes of two
massive parallelepipeds differing in their outermost dimensions in each
direction by the same length increment $2\Delta$ (\ [#Nayuk2012]_ Nayuk, 2012).

As in the case of the massive parallelepiped model (:ref:`rectangular-prism`),
the scattering amplitude is computed for a particular orientation of the
parallelepiped with respect to the scattering vector and then averaged over all
possible orientations, giving

.. math::
  P(q) =  \frac{1}{V^2} \frac{2}{\pi} \times \, \int_0^{\frac{\pi}{2}} \,
  \int_0^{\frac{\pi}{2}} A_{P\Delta}^2(q) \, \sin\theta \, d\theta \, d\phi

where $\theta$ is the angle between the $z$ axis and the $C$ axis
of the parallelepiped, $\phi$ is the angle between the scattering vector
(lying in the $xy$ plane) and the $y$ axis, and

.. math::
  :nowrap:

  \begin{align*}
  A_{P\Delta}(q) & =  A B C
    \left[\frac{\sin \bigl( q \frac{C}{2} \cos\theta \bigr)}
    {\left( q \frac{C}{2} \cos\theta \right)} \right]
    \left[\frac{\sin \bigl( q \frac{A}{2} \sin\theta \sin\phi \bigr)}
    {\left( q \frac{A}{2} \sin\theta \sin\phi \right)}\right]
    \left[\frac{\sin \bigl( q \frac{B}{2} \sin\theta \cos\phi \bigr)}
    {\left( q \frac{B}{2} \sin\theta \cos\phi \right)}\right] \\
    & {} - A' B' C'
    \left[ \frac{\sin \bigl[ q \frac{C'}{2} \cos\theta \bigr]}
    {q \frac{C'}{2} \cos\theta} \right]
    \left[ \frac{\sin \bigl[ q \frac{A'}{2} \sin\theta \sin\phi \bigr]}
    {q \frac{A'}{2} \sin\theta \sin\phi} \right]
    \left[ \frac{\sin \bigl[ q \frac{B'}{2} \sin\theta \cos\phi \bigr]}
    {q \frac{B'}{2} \sin\theta \cos\phi} \right]
  \end{align*}

$A'$, $B'$, $C'$ are the inner dimensions and $A = A' + 2\Delta$,
$B = B' + 2\Delta$, $C = C' + 2\Delta$ are the outer dimensions of the
parallelepiped shell, giving a shell volume of $V = ABC - A'B'C'$.

The 1D scattering intensity is then calculated as

.. math::
  I(q) = \text{scale} \times V \times (\rho_\text{p} -
  \rho_\text{solvent})^2 \times P(q) + \text{background}

where $\rho_\text{p}$ is the scattering length density of the parallelepiped,
$\rho_\text{solvent}$ is the scattering length density of the solvent,
and (if the data are in absolute units) *scale* represents the volume fraction
(which is unitless) of the rectangular shell of material (i.e. not including
the volume of the solvent filled core).

For 2d data the orientation of the particle is required, described using
angles $\theta$, $\phi$ and $\Psi$ as in the diagrams below, for further details
of the calculation and angular dispersions see :ref:`orientation`.
The angle $\Psi$ is the rotational angle around the long *C* axis. For example,
$\Psi = 0$ when the *B* axis is parallel to the *x*-axis of the detector.

For 2d, constraints must be applied during fitting to ensure that the inequality
$A < B < C$ is not violated, and hence the correct definition of angles is
preserved. The calculation will not report an error if the inequality is *not*
preserved, but the results may be not correct.

.. figure:: img/parallelepiped_angle_definition.png

    Definition of the angles for oriented hollow rectangular prism. Note that
    rotation $\theta$, initially in the $xz$ plane, is carried out first,
    then rotation $\phi$ about the $z$ axis, finally rotation $\Psi$ is now
    around the axis of the prism. The neutron or X-ray beam is along the $z$
    axis.

.. figure:: img/parallelepiped_angle_projection.png

    Examples of the angles for oriented hollow rectangular prisms against the
    detector plane.


Validation
----------

Validation of the code was conducted by qualitatively comparing the output
of the 1D model to the curves shown in (Nayuk, 2012).


References
----------

See also Onsager [#Onsager1949]_.

.. [#Nayuk2012] R Nayuk and K Huber, *Z. Phys. Chem.*, 226 (2012) 837-854
.. [#Onsager1949] L. Onsager, *Ann. New York Acad. Sci.*, 51 (1949) 627-659

Authorship and Verification
----------------------------

* **Author:** Miguel Gonzales **Date:** February 26, 2016
* **Last Modified by:** Paul Kienzle **Date:** December 14, 2017
* **Last Reviewed by:** Paul Butler **Date:** September 06, 2018
"""

import numpy as np
from numpy import inf

name = "hollow_rectangular_prism"
title = "Hollow rectangular parallelepiped with uniform scattering length density."
description = """
    I(q)= scale*V*(sld - sld_solvent)^2*P(q,theta,phi)+background
        P(q,theta,phi) = (2/pi/V^2) * double integral from 0 to pi/2 of ...
           (AP1-AP2)^2(q)*sin(theta)*dtheta*dphi
        AP1 = S(q*C*cos(theta)/2) * S(q*A*sin(theta)*sin(phi)/2) * S(q*B*sin(theta)*cos(phi)/2)
        AP2 = S(q*C'*cos(theta)) * S(q*A'*sin(theta)*sin(phi)) * S(q*B'*sin(theta)*cos(phi))
        C' = (C/2-thickness)
        B' = (B/2-thickness)
        A' = (A/2-thickness)
        S(x) = sin(x)/x
"""
category = "shape:parallelepiped"

#             ["name", "units", default, [lower, upper], "type","description"],
parameters = [["sld", "1e-6/Ang^2", 6.3, [-inf, inf], "sld",
               "Parallelepiped scattering length density"],
              ["sld_solvent", "1e-6/Ang^2", 1, [-inf, inf], "sld",
               "Solvent scattering length density"],
              ["length_a", "Ang", 35, [0, inf], "volume",
               "Shortest, external, size of the parallelepiped"],
              ["b2a_ratio", "Ang", 1, [0, inf], "volume",
               "Ratio sides b/a"],
              ["c2a_ratio", "Ang", 1, [0, inf], "volume",
               "Ratio sides c/a"],
              ["thickness", "Ang", 1, [0, inf], "volume",
               "Thickness of parallelepiped"],
              ["theta", "degrees", 0, [-360, 360], "orientation",
               "c axis to beam angle"],
              ["phi", "degrees", 0, [-360, 360], "orientation",
               "rotation about beam"],
              ["psi", "degrees", 0, [-360, 360], "orientation",
               "rotation about c axis"],
             ]

source = ["lib/gauss76.c", "hollow_rectangular_prism.c"]
have_Fq = True
radius_effective_modes = [
    "equivalent cylinder excluded volume", "equivalent outer volume sphere",
    "half length_a", "half length_b", "half length_c",
    "equivalent outer circular cross-section",
    "half ab diagonal", "half diagonal",
    ]

def random():
    """Return a random parameter set for the model."""
    a, b, c = 10**np.random.uniform(1, 4.7, size=3)
    # Thickness is limited to 1/2 the smallest dimension
    # Use a distribution with a preference for thin shell or thin core
    # Avoid core,shell radii < 1
    min_dim = 0.5*min(a, b, c)
    thickness = np.random.beta(0.5, 0.5)*(min_dim-2) + 1
    #print(a, b, c, thickness, thickness/min_dim)
    pars = dict(
        length_a=a,
        b2a_ratio=b/a,
        c2a_ratio=c/a,
        thickness=thickness,
    )
    return pars

tests = [[{}, 0.2, 0.76687283098],
         [{}, [0.2], [0.76687283098]],
        ]
