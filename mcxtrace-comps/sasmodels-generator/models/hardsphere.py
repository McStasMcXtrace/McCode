# Note: model title and parameter table are inserted automatically
r"""
Calculates the interparticle structure factor for monodisperse
spherical particles interacting through hard sphere (excluded volume)
interactions. This $S(q)$ may also be a reasonable approximation for
other particle shapes that freely rotate (but see the note below),
and for moderately polydisperse systems.

.. note::

   This routine is intended for uncharged particles! For charged
   particles try using the :ref:`hayter-msa` $S(q)$ instead.

.. note::

   Earlier versions of SasView did not incorporate the so-called
   $\beta(q)$ ("beta") correction [1] for polydispersity and non-sphericity.
   This is only available in SasView versions 5.0 and higher.

radius_effective is the effective hard sphere radius.
volfraction is the volume fraction occupied by the spheres.

In SasView the effective radius may be calculated from the parameters
used in the form factor $P(q)$ that this $S(q)$ is combined with.

For numerical stability the computation uses a Taylor series expansion
at very small $qR$, but there may be a very minor glitch at the
transition point in some circumstances.

This S(q) uses the Percus-Yevick closure relationship [2] where the
interparticle potential $U(r)$ is

.. math::

    U(r) = \begin{cases}
    \infty & r < 2R \\
    0 & r \geq 2R
    \end{cases}

where $r$ is the distance from the center of a sphere of a radius $R$.

For a 2D plot, the wave transfer is defined as

.. math::

    q = \sqrt{q_x^2 + q_y^2}


References
----------

#.  M Kotlarchyk & S-H Chen, *J. Chem. Phys.*, 79 (1983) 2461-2469

#.  J K Percus, J Yevick, *J. Phys. Rev.*, 110, (1958) 1

Authorship and Verification
----------------------------

* **Author:**
* **Last Modified by:**
* **Last Reviewed by:**
"""

import numpy as np
from numpy import inf

name = "hardsphere"
title = "Hard sphere structure factor, with Percus-Yevick closure"
description = """\
    [Hard sphere structure factor, with Percus-Yevick closure]
        Interparticle S(Q) for random, non-interacting spheres.
    May be a reasonable approximation for other particle shapes
    that freely rotate, and for moderately polydisperse systems
    . The "beta(q)" correction is available in versions 4.2.2
    and higher.
    radius_effective is the hard sphere radius
    volfraction is the volume fraction occupied by the spheres.
"""
category = "structure-factor"
structure_factor = True
single = False # TODO: check

#             ["name", "units", default, [lower, upper], "type","description"],
parameters = [["radius_effective", "Ang", 50.0, [0, inf], "",
               "effective radius of hard sphere"],
              ["volfraction", "", 0.2, [0, 0.74], "",
               "volume fraction of hard spheres"],
             ]

source = ["hardsphere.c"]

def random():
    """Return a random parameter set for the model."""
    pars = dict(
        scale=1, background=0,
        radius_effective=10**np.random.uniform(1, 4),
        volfraction=10**np.random.uniform(-2, 0),  # high volume fraction
    )
    return pars

# Q=0.001 is in the Taylor series, low Q part, so add Q=0.1,
# assuming double precision sasview is correct
tests = [
    [{'scale': 1.0, 'background' : 0.0, 'radius_effective' : 50.0,
      'volfraction' : 0.2},
     [0.001, 0.1], [0.209128, 0.930587]],
]
# ADDED by: RKH  ON: 16Mar2016  using equations from FISH as better than
# orig sasview, see notes above. Added Taylor expansions at small Q.
