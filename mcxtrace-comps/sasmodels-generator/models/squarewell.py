# Note: model title and parameter table are inserted automatically
r"""
Calculates the interparticle structure factor for a hard sphere fluid
with a narrow, attractive, square well potential. **The Mean Spherical
Approximation (MSA) closure relationship is used, but it is not the most
appropriate closure for an attractive interparticle potential.** However,
the solution has been compared to Monte Carlo simulations for a square
well fluid and these show the MSA calculation to be limited to well
depths $\epsilon < 1.5$ kT and volume fractions $\phi < 0.08$.

Positive well depths correspond to an attractive potential well. Negative
well depths correspond to a potential "shoulder", which may or may not be
physically reasonable. The :ref:`stickyhardsphere` model may be a better
choice in some circumstances.

Computed values may behave badly at extremely small $qR$.

.. note::

   Earlier versions of SasView did not incorporate the so-called
   $\beta(q)$ ("beta") correction [2] for polydispersity and non-sphericity.
   This is only available in SasView versions 5.0 and higher.

The well width $(\lambda)$ is defined as multiples of the particle diameter
$(2 R)$.

The interaction potential is:

.. math::

    U(r) = \begin{cases}
    \infty & r < 2R \\
    -\epsilon & 2R \leq r < 2R\lambda \\
    0 & r \geq 2R\lambda
    \end{cases}

where $r$ is the distance from the center of a sphere of a radius $R$.

In SasView the effective radius may be calculated from the parameters
used in the form factor $P(q)$ that this $S(q)$ is combined with.

For 2D data: The 2D scattering intensity is calculated in the same way as 1D,
where the $q$ vector is defined as

.. math::

    q = \sqrt{q_x^2 + q_y^2}

References
----------

#.  R V Sharma, K C Sharma, *Physica*, 89A (1977) 213

#.  M Kotlarchyk and S-H Chen, *J. Chem. Phys.*, 79 (1983) 2461-2469

Authorship and Verification
----------------------------

* **Author:**
* **Last Modified by:**
* **Last Reviewed by:** Steve King **Date:** March 27, 2019
"""

import numpy as np
from numpy import inf

name = "squarewell"
title = "Square well structure factor with Mean Spherical Approximation closure"
description = """\
    [Square well structure factor, with MSA closure]
        Interparticle structure factor S(Q) for a hard sphere fluid
    with a narrow attractive well. Fits are prone to deliver non-
    physical parameters; use with care and read the references in
    the model documentation.The "beta(q)" correction is available
    in versions 4.2.2 and higher.
"""
category = "structure-factor"
structure_factor = True
single = False

#single = False
#             ["name", "units", default, [lower, upper], "type","description"],
parameters = [
    #   [ "name", "units", default, [lower, upper], "type",
    #     "description" ],
    ["radius_effective", "Ang", 50.0, [0, inf], "volume",
     "effective radius of hard sphere"],
    ["volfraction", "", 0.04, [0, 0.08], "",
     "volume fraction of spheres"],
    ["welldepth", "kT", 1.5, [0.0, 1.5], "",
     "depth of well, epsilon"],
    ["wellwidth", "diameters", 1.2, [1.0, inf], "",
     "width of well in diameters (=2R) units, must be > 1"],
    ]

# No volume normalization despite having a volume parameter
# This should perhaps be volume normalized?
form_volume = """
    return 1.0;
    """

source = ["squarewell.c"]

def random():
    """Return a random parameter set for the model."""
    pars = dict(
        scale=1, background=0,
        radius_effective=10**np.random.uniform(1, 4.7),
        volfraction=np.random.uniform(0.00001, 0.08),
        welldepth=np.random.uniform(0, 1.5),
        wellwidth=np.random.uniform(1, 1.2),
    )
    return pars

#
tests = [
    [{'scale': 1.0, 'background': 0.0, 'radius_effective': 50.0,
      'volfraction': 0.04, 'welldepth': 1.5, 'wellwidth': 1.2,
      'radius_effective_pd': 0}, [0.001], [0.97665742]],
    ]
# ADDED by: converting from sasview RKH  ON: 16Mar2016
