r"""
Definition
----------

Calcuates the scattering from a simple star polymer with f equal Gaussian coil
arms. A star being defined as a branched polymer with all the branches
emanating from a common central (in the case of this model) point.  It is
derived as a special case of on the Benoit model for general branched
polymers\ [#Benoit1953]_ as also used by Richter *et al.*\ [#Richter1989]_

For a star with $f$ arms the scattering intensity $I(q)$ is calculated as

.. math::

    I(q) = \frac{2}{fv^2}\left[ v-1+\exp(-v)+\frac{f-1}{2}
           \left[ 1-\exp(-v)\right]^2\right]

where

.. math:: v=\frac{uf}{(3f-2)}

and

.. math:: u = \left\langle R_{g}^2\right\rangle q^2

contains the square of the ensemble average radius-of-gyration of the full
polymer while v contains the radius of gyration of a single arm $R_{arm}$.
The two are related as:

.. math:: R_{arm}^2 = \frac{f}{3f-2} R_{g}^2

Note that when there is only one arm, $f = 1$, the Debye Gaussian coil
equation is recovered.

.. note::
   Star polymers in solutions tend to have strong interparticle and osmotic
   effects. Thus the Benoit equation may not work well for many real cases.
   A newer model for star polymer incorporating excluded volume has been
   developed by Li et al in arXiv:1404.6269 [physics.chem-ph].  Also, at small
   $q$ the scattering, i.e. the Guinier term, is not sensitive to the number of
   arms, and hence 'scale' here is simply $I(q=0)$ as described for the
   :ref:`mono-gauss-coil` model, using volume fraction $\phi$ and volume V
   for the whole star polymer.

References
----------

.. [#Benoit1953] H Benoit *J. Polymer Science*, 11, 507-510 (1953)
.. [#Richter1989] D Richter, B. Farago, J. S. Huang, L. J. Fetters,
   B Ewen *Macromolecules*, 22, 468-472 (1989)

Authorship and Verification
----------------------------

* **Author:** Kieran Campbell **Date:** July 24, 2012
* **Last Modified by:** Paul Butler **Date:** Auguts 26, 2017
* **Last Reviewed by:** Ziang Li and Richard Heenan **Date:** May 17, 2017
"""

import numpy as np
from numpy import inf

name = "star_polymer"
title = "Star polymer model with Gaussian statistics"
description = """
        Benoit 'Star polymer with Gaussian statistics'
        with
        P(q) = 2/{fv^2} * (v - (1-exp(-v)) + {f-1}/2 * (1-exp(-v))^2)
        where
        - v = u^2f/(3f-2)
        - u = <R_g^2>q^2, where <R_g^2> is the ensemble average radius of
        gyration squared of the entire polymer
        - f is the number of arms on the star
        - the radius of gyration of an arm is given b
        Rg_arm^2 = R_g^2 * f/(3f-2)
        """
category = "shape-independent"
single = False
# pylint: disable=bad-whitespace, line-too-long
#             ["name", "units", default, [lower, upper], "type","description"],
parameters = [["rg_squared", "Ang^2", 100.0, [0.0, inf], "", "Ensemble radius of gyration SQUARED of the full polymer"],
              ["arms",    "",      3,   [1.0, 6.0], "", "Number of arms in the model"],
             ]
# pylint: enable=bad-whitespace, line-too-long

source = ["star_polymer.c"]

def random():
    """Return a random parameter set for the model."""
    pars = dict(
        #background=0,
        scale=10**np.random.uniform(1, 4),
        rg_squared=10**np.random.uniform(1, 8),
        arms=np.random.uniform(1, 6),
    )
    return pars

tests = [[{'rg_squared': 2.0,
           'arms':    3.3,
          }, 0.5, 0.851646091108],

         [{'rg_squared':    1.0,
           'arms':       2.0,
           'background': 1.8,
          }, 1.0, 2.53575888234],
        ]
# 23Mar2016  RKH edited docs, would this better use rg not rg^2 ? Numerical noise at extremely small q.rg
