#correlation length model
# Note: model title and parameter table are inserted automatically
r"""
Definition
----------

The scattering intensity I(q) is calculated as

.. math::
    I(Q) = \frac{A}{Q^n} + \frac{C}{1 + (Q\xi)^m} + \text{background}

The first term describes Porod scattering from clusters (exponent = $n$) and
the second term is a Lorentzian function describing scattering from
polymer chains (exponent = $m$). This second term characterizes the
polymer/solvent interactions and therefore the thermodynamics. The two
multiplicative factors $A$ and $C$, and the two exponents $n$ and $m$ are
used as fitting parameters. (Respectively *porod_scale*, *lorentz_scale*,
*porod_exp* and *lorentz_exp* in the parameter list.) The remaining
parameter $\xi$ (*cor_length* in the parameter list) is a correlation
length for the polymer chains. Note that when $m=2$ this functional form
becomes the familiar Lorentzian function. Some interpretation of the
values of $A$ and $C$ may be possible depending on the values of $m$ and $n$.

For 2D data: The 2D scattering intensity is calculated in the same way as 1D,
where the q vector is defined as

.. math::  q = \sqrt{q_x^2 + q_y^2}

References
----------

#. B Hammouda, D L Ho and S R Kline, Insight into Clustering in
   Poly(ethylene oxide) Solutions, Macromolecules, 37 (2004) 6932-6937

Authorship and Verification
----------------------------

* **Author:** NIST IGOR/DANSE **Date:** pre 2010
* **Last Modified by:** Steve King **Date:** September 24, 2019
* **Last Reviewed by:**
"""

from numpy import inf, errstate

name = "correlation_length"
title = """Calculates an empirical functional form for SAS data characterized
by a low-Q signal and a high-Q signal."""
description = """
"""
category = "shape-independent"
# pylint: disable=bad-continuation, line-too-long
#             ["name", "units", default, [lower, upper], "type","description"],
parameters = [
              ["lorentz_scale", "", 10.0, [0, inf], "", "Lorentzian Scaling Factor"],
              ["porod_scale", "", 1e-06, [0, inf], "", "Porod Scaling Factor"],
              ["cor_length", "Ang", 50.0, [0, inf], "", "Correlation length, xi, in Lorentzian"],
              ["porod_exp", "", 3.0, [0, inf], "", "Porod Exponent, n, in q^-n"],
              ["lorentz_exp", "", 2.0, [0, inf], "", "Lorentzian Exponent, m, in 1/( 1 + (q.xi)^m)"],
             ]
# pylint: enable=bad-continuation, line-too-long

def Iq(q, lorentz_scale, porod_scale, cor_length, porod_exp, lorentz_exp):
    """
    1D calculation of the Correlation length model
    """
    with errstate(divide='ignore'):
        porod = porod_scale / q**porod_exp
        lorentz = lorentz_scale / (1.0 + (q * cor_length)**lorentz_exp)
    inten = porod + lorentz
    return inten
Iq.vectorized = True

tests = [[{}, 0.001, 1009.98],
         [{}, 0.150141, 0.175645],
         [{}, 0.442528, 0.0213957]]
