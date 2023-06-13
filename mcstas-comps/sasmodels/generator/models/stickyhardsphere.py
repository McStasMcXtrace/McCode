# Note: model title and parameter table are inserted automatically
r"""
Calculates the interparticle structure factor for a hard sphere fluid
with a narrow, attractive, potential well. Unlike the :ref:`squarewell`
model, here a perturbative solution of the Percus-Yevick closure
relationship is used. The strength of the attractive well is described
in terms of "stickiness" as defined below.

The perturbation parameter (perturb), $\tau$, should be fixed between 0.01
and 0.1 and the "stickiness", $\epsilon$, allowed to vary to adjust the
interaction strength. The "stickiness" is defined in the equation below and
is a function of both the perturbation parameter and the interaction
strength. $\epsilon$ and $\tau$ are defined in terms of the hard sphere
diameter $(\sigma = 2 R)$, the width of the square well, $\Delta$ (having the
same units as $R$\ ), and the depth of the well, $U_o$, in units of $kT$.
From the definition, it is clear that smaller $\epsilon$ means a stronger
attraction.

.. math::

    \epsilon     &= \frac{1}{12\tau} \exp(u_o / kT) \\
    \tau &= \Delta / (\sigma + \Delta)

where the interaction potential is

.. math::

    U(r) = \begin{cases}
        \infty & r < \sigma \\
        -U_o   & \sigma \leq r \leq \sigma + \Delta \\
        0      & r > \sigma + \Delta
        \end{cases}

The Percus-Yevick (PY) closure is used for this calculation, and is an
adequate closure for an attractive interparticle potential. The solution
has been compared to Monte Carlo simulations for a square well fluid, with
good agreement.

The true particle volume fraction, $\phi$, is not equal to $h$ which appears
in most of reference [1]. The two are related in equation (24). Reference
[1] also describes the relationship between this perturbative solution and
the original sticky hard sphere (or "adhesive sphere") model of Baxter [2].

.. note::

   The calculation can go haywire for certain combinations of the input
   parameters, producing unphysical solutions. In this case errors are
   reported to the command window and $S(q)$ is set to -1 (so it will
   disappear on a log-log plot!).

   Use tight bounds to keep the parameters to values that you know are
   physical (test them), and keep nudging them until the optimization
   does not hit the constraints.

.. note::

   Earlier versions of SasView did not incorporate the so-called
   $\beta(q)$ ("beta") correction [3] for polydispersity and non-sphericity.
   This is only available in SasView versions 5.0 and higher.

In SasView the effective radius may be calculated from the parameters
used in the form factor $P(q)$ that this $S(q)$ is combined with.

For 2D data the scattering intensity is calculated in the same way
as 1D, where the $q$ vector is defined as

.. math::

    q = \sqrt{q_x^2 + q_y^2}


References
----------

#. S V G Menon, C Manohar, and K S Rao,
   *J. Chem. Phys.*, 95(12) (1991) 9186-9190

#. R J Baxter, *J. Chem. Phys.*, 49 (1968), 2770-2774

#. M Kotlarchyk and S-H Chen, *J. Chem. Phys.*, 79 (1983) 2461-2469

Authorship and Verification
----------------------------

* **Author:**
* **Last Modified by:**
* **Last Reviewed by:** Steve King **Date:** March 27, 2019
"""

# TODO: refactor so that we pull in the old sansmodels.c_extensions

import numpy as np
from numpy import inf

name = "stickyhardsphere"
title = "'Sticky' hard sphere structure factor with Percus-Yevick closure"
description = """\
    [Sticky hard sphere structure factor, with Percus-Yevick closure]
        Interparticle structure factor S(Q) for a hard sphere fluid
    with a narrow attractive well. Fits are prone to deliver non-
    physical parameters; use with care and read the references in
    the model documentation.The "beta(q)" correction is available
    in versions 4.2.2 and higher.
"""
category = "structure-factor"
structure_factor = True

single = False
#             ["name", "units", default, [lower, upper], "type","description"],
parameters = [
    #   [ "name", "units", default, [lower, upper], "type",
    #     "description" ],
    ["radius_effective", "Ang", 50.0, [0, inf], "volume",
     "effective radius of hard sphere"],
    ["volfraction", "", 0.2, [0, 0.74], "",
     "volume fraction of hard spheres"],
    ["perturb", "", 0.05, [0.01, 0.1], "",
     "perturbation parameter, tau"],
    ["stickiness", "", 0.20, [-inf, inf], "",
     "stickiness, epsilon"],
    ]

source = ["stickyhardsphere.c"]    

# No volume normalization despite having a volume parameter
# This should perhaps be volume normalized?
form_volume = """
    return 1.0;
    """

def random():
    """Return a random parameter set for the model."""
    pars = dict(
        scale=1, background=0,
        radius_effective=10**np.random.uniform(1, 4.7),
        volfraction=np.random.uniform(0.00001, 0.74),
        perturb=10**np.random.uniform(-2, -1),
        stickiness=np.random.uniform(0, 1),
    )
    return pars



#
tests = [
    [{'scale': 1.0, 'background': 0.0, 'radius_effective': 50.0,
      'perturb': 0.05, 'stickiness': 0.2, 'volfraction': 0.1,
      'radius_effective_pd': 0},
     [0.001, 0.003], [1.09718, 1.087830]],
    ]
