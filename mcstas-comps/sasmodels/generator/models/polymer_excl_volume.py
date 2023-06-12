r"""
This model describes the scattering from polymer chains subject to excluded
volume effects and has been used as a template for describing mass fractals.

Definition
----------

The form factor was originally presented in the following integral form
(Benoit, 1957)

.. math::

    P(Q)=2\int_0^{1}dx(1-x)exp\left[-\frac{Q^2a^2}{6}n^{2v}x^{2v}\right]

where $\nu$ is the excluded volume parameter
(which is related to the Porod exponent $m$ as $\nu=1/m$ ),
$a$ is the statistical segment length of the polymer chain,
and $n$ is the degree of polymerization.

This integral was put into an almost analytical form as follows
(Hammouda, 1993)

.. math::

    P(Q)=\frac{1}{\nu U^{1/2\nu}}
    \left\{
        \gamma\left(\frac{1}{2\nu},U\right) -
        \frac{1}{U^{1/2\nu}}\gamma\left(\frac{1}{\nu},U\right)
    \right\}

and later recast as (for example, Hore, 2013; Hammouda & Kim, 2017)

.. math::

    P(Q)=\frac{1}{\nu U^{1/2\nu}}\gamma\left(\frac{1}{2\nu},U\right) -
    \frac{1}{\nu U^{1/\nu}}\gamma\left(\frac{1}{\nu},U\right)

where $\gamma(x,U)$ is the incomplete gamma function

.. math::

    \gamma(x,U)=\int_0^{U}dt\ \exp(-t)t^{x-1}

and the variable $U$ is given in terms of the scattering vector $Q$ as

.. math::

    U=\frac{Q^2a^2n^{2\nu}}{6} = \frac{Q^2R_{g}^2(2\nu+1)(2\nu+2)}{6}

The two analytic forms are equivalent. In the 1993 paper

.. math::

    \frac{1}{\nu U^{1/2\nu}}

has been factored out.

**SasView implements the 1993 expression**.

The square of the radius-of-gyration is defined as

.. math::

    R_{g}^2 = \frac{a^2n^{2\nu}}{(2\nu+1)(2\nu+2)}

.. note::
    This model applies only in the mass fractal range (ie, $5/3<=m<=3$)
    and **does not apply** to surface fractals ($3<m<=4$).
    It also does not reproduce the rigid rod limit (m=1) because it assumes
    chain flexibility from the outset. It may cover a portion of the
    semi-flexible chain range ($1<m<5/3$).

A low-Q expansion yields the Guinier form and a high-Q expansion yields the
Porod form which is given by

.. math::

    P(Q\rightarrow \infty) = \frac{1}{\nu U^{1/2\nu}}\Gamma\left(
    \frac{1}{2\nu}\right) - \frac{1}{\nu U^{1/\nu}}\Gamma\left(
    \frac{1}{\nu}\right)

Here $\Gamma(x) = \gamma(x,\infty)$ is the gamma function.

The asymptotic limit is dominated by the first term

.. math::

    P(Q\rightarrow \infty)
       \sim \frac{1}{\nu U^{1/2\nu}}\Gamma\left(\frac{1}{2\nu}\right) =
    \frac{m}{\left(QR_{g}\right)^m}
       \left[\frac{6}{(2\nu +1)(2\nu +2)} \right]^{m/2} \Gamma (m/2)

The special case when $\nu=0.5$ (or $m=1/\nu=2$ ) corresponds to Gaussian
chains for which the form factor is given by the familiar Debye function.

.. math::

    P(Q) = \frac{2}{Q^4R_{g}^4} \left[\exp(-Q^2R_{g}^2) - 1 + Q^2R_{g}^2 \right]

For 2D data: The 2D scattering intensity is calculated in the same way as 1D,
where the $q$ vector is defined as

.. math::

    q = \sqrt{q_x^2 + q_y^2}


References
----------

#. H Benoit, *Comptes Rendus*, 245 (1957) 2244-2247
#. B Hammouda, *SANS from Homogeneous Polymer Mixtures - A Unified Overview*,
   *Advances in Polym. Sci.* 106 (1993) 87-133
#. M Hore et al, *Co-Nonsolvency of Poly(N-isopropylacrylamide) in Deuterated
   Water/Ethanol Mixtures*, *Macromolecules* 46 (2013) 7894-7901
#. B Hammouda & M-H Kim, *The empirical core-chain model*,
   *Journal of Molecular Liquids* 247 (2017) 434-440

Authorship and Verification
----------------------------

* **Author:**
* **Last Modified by:**
* **Last Reviewed by:**
"""

import numpy as np
from numpy import inf, power, errstate
from scipy.special import gammainc, gamma

name = "polymer_excl_volume"
title = "Polymer Excluded Volume model"
description = """Compute the scattering intensity from polymers with excluded
                volume effects.
                rg:         radius of gyration
                porod_exp:  Porod exponent
              """
category = "shape-independent"

# pylint: disable=bad-whitespace, line-too-long
#   ["name", "units", default, [lower, upper], "type", "description"],
parameters = [
    ["rg",        "Ang", 60.0, [0, inf], "", "Radius of Gyration"],
    ["porod_exp", "",     3.0, [0, inf], "", "Porod exponent"],
]
# pylint: enable=bad-whitespace, line-too-long


def Iq(q, rg=60.0, porod_exp=3.0):
    """
    :param q:         Input q-value (float or [float, float])
    :param rg:        Radius of gyration
    :param porod_exp: Porod exponent
    :return:          Calculated intensity
    """
    usub = (q*rg)**2 * (2.0/porod_exp + 1.0) * (2.0/porod_exp + 2.0)/6.0
    with errstate(divide='ignore', invalid='ignore'):
        upow = power(usub, -0.5*porod_exp)
        # Note: scipy gammainc is "regularized", being gamma(s,x)/Gamma(s),
        # so need to scale by Gamma(s) to recover gamma(s, x).
        result = (porod_exp*upow *
                  (gamma(0.5*porod_exp)*gammainc(0.5*porod_exp, usub) -
                   upow*gamma(porod_exp)*gammainc(porod_exp, usub)))
    result[q <= 0] = 1.0

    return result

Iq.vectorized = True  # Iq accepts an array of q values

def random():
    """Return a random parameter set for the model."""
    rg = 10**np.random.uniform(0, 4)
    porod_exp = np.random.uniform(1e-3, 6)
    scale = 10**np.random.uniform(1, 5)
    pars = dict(
        #background=0,
        scale=scale,
        rg=rg,
        porod_exp=porod_exp,
    )
    return pars

tests = [
    # Accuracy tests based on content in test/polyexclvol_default_igor.txt
    [{'rg': 60, 'porod_exp': 3.0}, 0.001, 0.999801],
    [{'rg': 60, 'porod_exp': 3.0}, 0.105363, 0.0172751],
    [{'rg': 60, 'porod_exp': 3.0, 'background': 0.0}, 0.665075, 6.56261e-05],

    # Additional tests with larger range of parameters
    [{'rg': 10, 'porod_exp': 4.0}, 0.1, 0.724436675809],
    [{'rg': 2.2, 'porod_exp': 22.0, 'background': 100.0}, 5.0, 100.0],
    [{'rg': 1.1, 'porod_exp': 1, 'background': 10.0, 'scale': 1.25},
     20000., 10.0000712097]
    ]
