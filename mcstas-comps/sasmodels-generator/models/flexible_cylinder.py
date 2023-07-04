r"""
This model provides the form factor, $P(q)$, for a flexible cylinder
where the form factor is normalized by the volume of the cylinder.
**Inter-cylinder interactions are NOT provided for.**

.. math::

    P(q) = \text{scale} \left<F^2\right>/V + \text{background}

where the averaging $\left<\ldots\right>$ is applied only for the 1D
calculation

The 2D scattering intensity is the same as 1D, regardless of the orientation
of the q vector which is defined as

.. math::

    q = \sqrt{q_x^2 + q_y^2}

Definitions
-----------

.. figure:: img/flexible_cylinder_geometry.jpg


The chain of contour length, $L$, (the total length) can be described as a
chain of some number of locally stiff segments of length $l_p$, the
persistence length (the length along the cylinder over which the flexible
cylinder can be considered a rigid rod). The Kuhn length $(b = 2*l_p)$ is
also used to describe the stiffness of a chain.

In the parameters, the sld and sld\_solvent represent the SLD of the cylinder
and solvent respectively.

Our model uses the form factor calculations in reference [1] as implemented in
a c-library provided by the NIST Center for Neutron Research (Kline, 2006).
This states:

    'Method 3 With Excluded Volume' is used.
    The model is a parametrization of simulations of a discrete representation
    of the worm-like chain model of Kratky and Porod applied in the
    pseudocontinuous limit.
    See equations (13,26-27) in the original reference for the details.

.. note::

    There are several typos in the original reference that have been
    corrected by Chen *et al* (WRC) [2]. Details of the corrections are in the
    reference below. Most notably

    - Equation (13): the term $(1 - w(QR))$ should swap position with $w(QR)$

    - Equations (23) and (24) are incorrect; WRC has entered these into
      Mathematica and solved analytically. The results were then converted to
      code.

    - Equation (27) should be $q0 = max(a3/(Rg^2)^{1/2},3)$ instead of
      $max(a3*b(Rg^2)^{1/2},3)$

    - The scattering function is negative for a range of parameter values and
      q-values that are experimentally accessible. A correction function has
      been added to give the proper behavior.


**This is a model with complex behaviour depending on the ratio of** $L/b$
**and the reader is strongly encouraged to read reference [1] before use. In
particular, the cylinder form factor used as the limiting case for long
narrow rods will not be exactly correct for short and/or wide rods.**

References
----------

#. J S Pedersen and P Schurtenberger. *Scattering functions of semiflexible
   polymers with and without excluded volume effects.*
   Macromolecules, 29 (1996) 7602-7612
#. W R Chen, P D Butler and L J Magid, *Incorporating Intermicellar
   Interactions in the Fitting of SANS Data from Cationic Wormlike Micelles.*
   Langmuir, 22(15) 2006 6539-6548

Authorship and Verification
----------------------------

* **Author:**
* **Last Modified by:**
* **Last Reviewed by:** Steve King **Date:** March 6, 2020
"""

import numpy as np
from numpy import inf

name = "flexible_cylinder"
title = "Flexible cylinder where the form factor is normalized by the volume " \
        "of the cylinder."
description = """Note : scale and contrast = (sld - sld_solvent) are both
                multiplicative factors in the model and are perfectly
                correlated. One or both of these parameters must be held fixed
                during model fitting.
              """

category = "shape:cylinder"
single = False  # double precision only!

# pylint: disable=bad-whitespace, line-too-long
#             ["name", "units", default, [lower, upper], "type", "description"],
parameters = [
    ["length",      "Ang",       1000.0, [0, inf],    "volume", "Length of the flexible cylinder"],
    ["kuhn_length", "Ang",        100.0, [0, inf],    "volume", "Kuhn length of the flexible cylinder"],
    ["radius",      "Ang",         20.0, [0, inf],    "volume", "Radius of the flexible cylinder"],
    ["sld",         "1e-6/Ang^2",   1.0, [-inf, inf], "sld",    "Cylinder scattering length density"],
    ["sld_solvent", "1e-6/Ang^2",   6.3, [-inf, inf], "sld",    "Solvent scattering length density"],
    ]
# pylint: enable=bad-whitespace, line-too-long
source = ["lib/polevl.c", "lib/sas_J1.c", "lib/wrc_cyl.c", "flexible_cylinder.c"]

def random():
    """Return a random parameter set for the model."""
    length = 10**np.random.uniform(2, 6)
    radius = 10**np.random.uniform(1, 3)
    kuhn_length = 10**np.random.uniform(-2, 0)*length
    pars = dict(
        length=length,
        radius=radius,
        kuhn_length=kuhn_length,
    )
    return pars

tests = [
    # Accuracy tests based on content in test/utest_other_models.py
    [{'length':     1000.0,  # test T1
      'kuhn_length': 100.0,
      'radius':       20.0,
      'sld':           1.0,
      'sld_solvent':   6.3,
      'background':    0.0001,
     }, 0.001, 3509.2187],

    # Additional tests with larger range of parameters
    [{'length':    1000.0,  # test T2
      'kuhn_length': 100.0,
      'radius':       20.0,
      'sld':           1.0,
      'sld_solvent':   6.3,
      'background':    0.0001,
     }, 1.0, 0.000595345],
    [{'length':        10.0,  # test T3
      'kuhn_length': 800.0,
      'radius':        2.0,
      'sld':           6.0,
      'sld_solvent':  12.3,
      'background':    0.001,
     }, 0.1, 1.55228],
    [{'length':        100.0,  # test T4
      'kuhn_length': 800.0,
      'radius':       50.0,
      'sld':           0.1,
      'sld_solvent':   5.1,
      'background':    0.0,
     }, 1.0, 0.000938456]
    ]

# There are a few branches in the code that ought to have test values:
#
# For length > 4 * kuhn_length
#        if length > 10 * kuhn_length then C is scaled by 3.06 (L/b)^(-0.44)
#        q*kuhn_length <= 3.1  => Sexv_new
#           dS/dQ < 0 has different behaviour from dS/dQ >= 0
#  T2    q*kuhn_length > 3.1   => a_long
#
# For length <= 4 * kuhn_length
#        q*kuhn_length <= max(1.9/Rg_short, 3.0)  => Sdebye((q*Rg)^2)
#           q*Rg < 0.5 uses Pade approx, q*Rg > 1.0 uses math lib
#  T3,T4 q*kuhn_length > max(1.9/Rg_short, 3.0)   => a_short
#
# Note that the transitions between branches may be abrupt.  You can see a
# several percent change around length=10*kuhn_length and length=4*kuhn_length
# using the following:
#
#    sascomp flexible_cylinder -calc=double -sets=10 length=10*kuhn_length,10.000001*kuhn_length
#    sascomp flexible_cylinder -calc=double -sets=10 length=4*kuhn_length,4.000001*kuhn_length
#
# The transition between low q and high q around q*kuhn_length = 3 seems
# to be good to 4 digits or better.  This was tested by computing the value
# on each branches near the transition point and reporting the relative error
# for kuhn lengths of 10, 100 and 1000 and a variety of length:kuhn_length
# ratios.
