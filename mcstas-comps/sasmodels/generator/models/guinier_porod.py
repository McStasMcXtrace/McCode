r"""
Calculates the scattering for a generalized Guinier/power law object. This is
an empirical model that can be used to determine the size and dimensionality
of scattering objects, including asymmetric objects such as rods or
platelets, and shapes intermediate between spheres and rods or between rods
and platelets, and overcomes some of the deficiencies of the (Beaucage)
:ref:`unified-power-rg` model (see Hammouda, 2010).

Definition
----------

The following functional form is used

.. math::

    I(q) = \begin{cases}
    \frac{G}{Q^s}\ \exp{\left[\frac{-Q^2R_g^2}{3-s} \right]} & Q \leq Q_1 \\
    D / Q^m  & Q \geq Q_1
    \end{cases}

This is based on the generalized Guinier law for such elongated objects
(see the Glatter reference below). For 3D globular objects (such as spheres),
$s = 0$ and one recovers the standard Guinier formula. For 2D symmetry
(such as for rods) $s = 1$, and for 1D symmetry (such as for lamellae or
platelets) $s = 2$. A dimensionality parameter ($3-s$) is thus defined,
and is 3 for spherical objects, 2 for rods, and 1 for plates.

Enforcing the continuity of the Guinier and Porod functions and their
derivatives yields

.. math::

    Q_1 = \frac{1}{R_g} \sqrt{(m-s)(3-s)/2}

and

.. math::

    D &= G \ \exp{ \left[ \frac{-Q_1^2 R_g^2}{3-s} \right]} \ Q_1^{m-s}

      &= \frac{G}{R_g^{m-s}} \ \exp \left[ -\frac{m-s}{2} \right]
          \left( \frac{(m-s)(3-s)}{2} \right)^{\frac{m-s}{2}}


Note that the radius of gyration for a sphere of radius $R$ is given
by $R_g = R \sqrt{3/5}$. For a cylinder of radius $R$ and length $L$,
$R_g^2 = \frac{L^2}{12} + \frac{R^2}{2}$ from which the cross-sectional
radius of gyration for a randomly oriented thin cylinder is $R_g = R/\sqrt{2}$
and the cross-sectional radius of gyration of a randomly oriented lamella
of thickness $T$ is given by $R_g = T / \sqrt{12}$.

For 2D data: The 2D scattering intensity is calculated in the same way as 1D,
where the q vector is defined as

.. math::
    q = \sqrt{q_x^2+q_y^2}


Reference
---------

#. B Hammouda, *A new Guinier-Porod model*,
   *J. Appl. Cryst.*, (2010), 43, 716-719
#. B Hammouda, *Analysis of the Beaucage model*,
   *J. Appl. Cryst.*, (2010), 43, 1474-1478

Authorship and Verification
----------------------------

* **Author:**
* **Last Modified by:**
* **Last Reviewed by:**
"""

import numpy as np
from numpy import inf, sqrt, exp, errstate

name = "guinier_porod"
title = "Guinier-Porod function"
description = """\
         I(q) = scale/q^s* exp ( - R_g^2 q^2 / (3-s) ) for q<= ql
         = scale/q^porod_exp*exp((-ql^2*Rg^2)/(3-s))*ql^(porod_exp-s) for q>=ql
                        where ql = sqrt((porod_exp-s)(3-s)/2)/Rg.
                        List of parameters:
                        scale = Guinier Scale
                        s = Dimension Variable
                        Rg = Radius of Gyration [A]
                        porod_exp = Porod Exponent
                        background  = Background [1/cm]"""

category = "shape-independent"

# pylint: disable=bad-whitespace, line-too-long
#             ["name", "units", default, [lower, upper], "type","description"],
parameters = [["rg", "Ang", 60.0, [0, inf], "", "Radius of gyration"],
              ["s",  "",    1.0,  [0, inf], "", "Dimension variable"],
              ["porod_exp",  "",    3.0,  [0, inf], "", "Porod exponent"]]
# pylint: enable=bad-whitespace, line-too-long

# pylint: disable=C0103
def Iq(q, rg, s, porod_exp):
    """
    @param q: Input q-value
    """
    n = 3.0 - s
    ms = 0.5*(porod_exp-s) # =(n-3+porod_exp)/2

    # preallocate return value
    iq = 0.0*q

    # Take care of the singular points
    if rg <= 0.0 or ms <= 0.0:
        return iq

    # Do the calculation and return the function value
    idx = q < sqrt(n*ms)/rg
    with errstate(divide='ignore'):
        iq[idx] = q[idx]**-s * exp(-(q[idx]*rg)**2/n)
        iq[~idx] = q[~idx]**-porod_exp * (exp(-ms) * (n*ms/rg**2)**ms)
    return iq

Iq.vectorized = True # Iq accepts an array of q values

def random():
    """Return a random parameter set for the model."""
    rg = 10**np.random.uniform(1, 5)
    s = np.random.uniform(0, 3)
    porod_exp = s + np.random.uniform(0, 3)
    pars = dict(
        #scale=1, background=0,
        rg=rg,
        s=s,
        porod_exp=porod_exp,
    )
    return pars

tests = [[{'scale': 1.5, 'background':0.5}, 0.04, 5.290096890253155]]
