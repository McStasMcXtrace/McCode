r"""
This model calculates the form factor for a flexible cylinder with an
elliptical cross section and a uniform scattering length density.
The non-negligible diameter of the cylinder is included by accounting
for excluded volume interactions within the walk of a single cylinder.
**Inter-cylinder interactions are NOT provided for.**

The form factor is normalized by the particle volume such that

.. math::

    P(q) = \text{scale} \left<F^2\right>/V + \text{background}

where the averaging $\left<\ldots\right>$ is over all possible orientations
of the flexible cylinder.

The 2D scattering intensity is the same as 1D, regardless of the orientation
of the q vector which is defined as

.. math::

    q = \sqrt{q_x^2 + q_y^2}


Definitions
-----------

The function is calculated in a similar way to that for the
:ref:`flexible-cylinder` model in reference [1] below using the author's
"Method 3 With Excluded Volume".

The model is a parameterization of simulations of a discrete representation of
the worm-like chain model of Kratky and Porod applied in the pseudo-continuous
limit. See equations (13, 26-27) in the original reference for the details.

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

.. figure:: img/flexible_cylinder_ex_geometry.jpg


The chain of contour length, $L$, (the total length) can be described as a
chain of some number of locally stiff segments of length $l_p$, the
persistence length (the length along the cylinder over which the flexible
cylinder can be considered a rigid rod). The Kuhn length $(b = 2*l_p)$ is
also used to describe the stiffness of a chain.

The cross section of the cylinder is elliptical, with minor radius $a$ . The
major radius is larger, so of course, **the axis_ratio must be greater than
one.** Simple constraints should be applied during curve fitting to maintain
this inequality.

In the parameters, the $sld$ and $sld\_solvent$ represent the SLD of the
chain/cylinder and solvent respectively. The *scale*, and the contrast are
both multiplicative factors in the model and are perfectly correlated. One or
both of these parameters must be held fixed during model fitting.

**This is a model with complex behaviour depending on the ratio of** $L/b$
**and the reader is strongly encouraged to read reference [1] before use.**

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
* **Last Modified by:** Richard Heenan **Date:** December, 2016
* **Last Reviewed by:** Steve King **Date:** March 26, 2019
"""

import numpy as np
from numpy import inf

name = "flexible_cylinder_elliptical"
title = "Flexible cylinder wth an elliptical cross section and a uniform " \
        "scattering length density."
description = """Note : scale and contrast=sldCyl-sldSolv are both multiplicative
        factors in the model and are perfectly correlated. One or both of
        these parameters must be held fixed during model fitting.
        """
single = False

category = "shape:cylinder"
# pylint: disable=bad-whitespace, line-too-long
#             ["name", "units", default, [lower, upper], "type", "description"],
parameters = [
    ["length",      "Ang",       1000.0, [0, inf],    "volume", "Length of the flexible cylinder"],
    ["kuhn_length", "Ang",        100.0, [0, inf],    "volume", "Kuhn length of the flexible cylinder"],
    ["radius",      "Ang",         20.0, [1, inf],    "volume", "Radius of the flexible cylinder"],
    ["axis_ratio",  "",             1.5, [0, inf],    "",       "Axis_ratio (major_radius/minor_radius"],
    ["sld",         "1e-6/Ang^2",   1.0, [-inf, inf], "sld",    "Cylinder scattering length density"],
    ["sld_solvent", "1e-6/Ang^2",   6.3, [-inf, inf], "sld",    "Solvent scattering length density"],
    ]
# pylint: enable=bad-whitespace, line-too-long

source = ["lib/polevl.c", "lib/sas_J1.c", "lib/gauss76.c", "lib/wrc_cyl.c",
          "flexible_cylinder_elliptical.c"]

def random():
    """Return a random parameter set for the model."""
    length = 10**np.random.uniform(2, 6)
    radius = 10**np.random.uniform(1, 3)
    axis_ratio = 10**np.random.uniform(-1, 1)
    kuhn_length = 10**np.random.uniform(-2, -0.7)*length  # at least 10 segments
    pars = dict(
        length=length,
        radius=radius,
        axis_ratio=axis_ratio,
        kuhn_length=kuhn_length,
    )
    return pars

tests = [
    # Accuracy tests based on content in test/utest_other_models.py
    # Currently fails in OCL
    # [{'length':     1000.0,
    #  'kuhn_length': 100.0,
    #  'radius':       20.0,
    #  'axis_ratio':    1.5,
    #  'sld':           1.0,
    #  'sld_solvent':   6.3,
    #  'background':    0.0001,
    # }, 0.001, 3509.2187],

    # Additional tests with larger range of parameters
    [{'length':     1000.0,
      'kuhn_length': 100.0,
      'radius':       20.0,
      'axis_ratio':    1.5,
      'sld':           1.0,
      'sld_solvent':   6.3,
      'background':    0.0001,
     }, 1.0, 0.00223819],
    [{'length':        10.0,
      'kuhn_length': 800.0,
      'radius':        2.0,
      'axis_ratio':    0.5,
      'sld':           6.0,
      'sld_solvent':  12.3,
      'background':    0.001,
     }, 0.1, 0.390281],
    [{'length':        100.0,
      'kuhn_length': 800.0,
      'radius':       50.0,
      'axis_ratio':    4.5,
      'sld':           0.1,
      'sld_solvent':   5.1,
      'background':    0.0,
     }, 1.0, 0.0016338264790]
    ]
