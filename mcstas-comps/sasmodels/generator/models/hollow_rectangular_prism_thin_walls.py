# rectangular_prism model
# Note: model title and parameter table are inserted automatically
r"""
Definition
----------
This model provides the form factor, $P(q)$, for a hollow rectangular prism
with infinitely thin walls. It computes only the 1D scattering, not the 2D.
The 1D scattering intensity for this model is calculated according to the
equations given by Nayuk and Huber\ [#Nayuk2012]_.

Assuming a hollow parallelepiped with infinitely thin walls, edge lengths
$A \le B \le C$ and presenting an orientation with respect to the
scattering vector given by $\theta$ and $\phi$, where $\theta$ is the angle
between the $z$ axis and the longest axis of the parallelepiped $C$, and
$\phi$ is the angle between the scattering vector (lying in the $xy$ plane)
and the $y$ axis, the form factor is given by

.. math::

    P(q) = \frac{1}{V^2} \frac{2}{\pi} \int_0^{\frac{\pi}{2}}
           \int_0^{\frac{\pi}{2}} [A_L(q)+A_T(q)]^2 \sin\theta\,d\theta\,d\phi

where

.. math::

    V &= 2AB + 2AC + 2BC \\
    A_L(q) &=  8 \times \frac{
            \sin \left( \tfrac{1}{2} q A \sin\phi \sin\theta \right)
            \sin \left( \tfrac{1}{2} q B \cos\phi \sin\theta \right)
            \cos \left( \tfrac{1}{2} q C \cos\theta \right)
        }{q^2 \, \sin^2\theta \, \sin\phi \cos\phi} \\
    A_T(q) &=  A_F(q) \times
      \frac{2\,\sin \left( \tfrac{1}{2} q C \cos\theta \right)}{q\,\cos\theta}

and

.. math::

  A_F(q) =  4 \frac{ \cos \left( \tfrac{1}{2} q A \sin\phi \sin\theta \right)
                       \sin \left( \tfrac{1}{2} q B \cos\phi \sin\theta \right) }
                     {q \, \cos\phi \, \sin\theta} +
              4 \frac{ \sin \left( \tfrac{1}{2} q A \sin\phi \sin\theta \right)
                       \cos \left( \tfrac{1}{2} q B \cos\phi \sin\theta \right) }
                     {q \, \sin\phi \, \sin\theta}

The 1D scattering intensity is then calculated as

.. math::

  I(q) = \text{scale} \times V \times (\rho_\text{p} - \rho_\text{solvent})^2 \times P(q)

where $V$ is the surface area of the rectangular prism, $\rho_\text{p}$
is the scattering length density of the parallelepiped, $\rho_\text{solvent}$
is the scattering length density of the solvent, and (if the data are in
absolute units) *scale* is related to the total surface area.

**The 2D scattering intensity is not computed by this model.**


Validation
----------

Validation of the code was conducted  by qualitatively comparing the output
of the 1D model to the curves shown in (Nayuk, 2012\ [#Nayuk2012]_).


References
----------

See also Onsager [#Onsager1949]_.

.. [#Nayuk2012] R Nayuk and K Huber, *Z. Phys. Chem.*, 226 (2012) 837-854

.. [#Onsager1949] L. Onsager, *Ann. New York Acad. Sci.*, 51 (1949) 627-659

Authorship and Verification
----------------------------

* **Author:** Miguel Gonzales **Date:** February 26, 2016
* **Last Modified by:** Paul Kienzle **Date:** October 15, 2016
* **Last Reviewed by:** Paul Butler **Date:** September 07, 2018
"""

import numpy as np
from numpy import inf

name = "hollow_rectangular_prism_thin_walls"
title = "Hollow rectangular parallelepiped with thin walls."
description = """
    I(q)= scale*V*(sld - sld_solvent)^2*P(q)+background
        with P(q) being the form factor corresponding to a hollow rectangular
        parallelepiped with infinitely thin walls.
"""
category = "shape:parallelepiped"

#             ["name", "units", default, [lower, upper], "type","description"],
parameters = [["sld", "1e-6/Ang^2", 6.3, [-inf, inf], "sld",
               "Parallelepiped scattering length density"],
              ["sld_solvent", "1e-6/Ang^2", 1, [-inf, inf], "sld",
               "Solvent scattering length density"],
              ["length_a", "Ang", 35, [0, inf], "volume",
               "Shorter side of the parallelepiped"],
              ["b2a_ratio", "Ang", 1, [0, inf], "volume",
               "Ratio sides b/a"],
              ["c2a_ratio", "Ang", 1, [0, inf], "volume",
               "Ratio sides c/a"],
             ]

source = ["lib/gauss76.c", "hollow_rectangular_prism_thin_walls.c"]
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
    pars = dict(
        length_a=a,
        b2a_ratio=b/a,
        c2a_ratio=c/a,
    )
    return pars

tests = [[{}, 0.2, 0.837719188592],
         [{}, [0.2], [0.837719188592]],
        ]
