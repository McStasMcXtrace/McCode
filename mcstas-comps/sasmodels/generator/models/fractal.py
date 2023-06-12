r"""
Definition
----------
This model calculates the scattering from fractal-like aggregates of spherical
building blocks according the following equation:

.. math::

    I(q) = \phi\ V_\text{block} (\rho_\text{block}
          - \rho_\text{solvent})^2 P(q)S(q) + \text{background}

where $\phi$ is The volume fraction of the spherical "building block" particles
of radius $R_0$, $V_{block}$ is the volume of a single building block,
$\rho_{solvent}$ is the scattering length density of the solvent, and
$\rho_{block}$ is the scattering length density of the building blocks, and
P(q), S(q) are the scattering from randomly distributed spherical particles
(the building blocks) and the interference from such building blocks organized
in a fractal-like clusters.  P(q) and S(q) are calculated as:

.. math::

    P(q)&= F(qR_0)^2 \\
    F(q)&= \frac{3 (\sin x - x \cos x)}{x^3} \\
    V_\text{particle} &= \frac{4}{3}\ \pi R_0 \\
    S(q) &= 1 + \frac{D_f\  \Gamma\!(D_f-1)}{[1+1/(q \xi)^2\  ]^{(D_f -1)/2}}
    \frac{\sin[(D_f-1) \tan^{-1}(q \xi) ]}{(q R_0)^{D_f}}

where $\xi$ is the correlation length representing the cluster size and $D_f$
is the fractal dimension, representing the self similarity of the structure.
Note that S(q) here goes negative if $D_f$ is too large, and the Gamma function
diverges at $D_f=0$ and $D_f=1$.

**Polydispersity on the radius is provided for.**

For 2D data: The 2D scattering intensity is calculated in the same way as
1D, where the *q* vector is defined as

.. math::

    q = \sqrt{q_x^2 + q_y^2}


References
----------

#.  J Teixeira, *J. Appl. Cryst.*, 21 (1988) 781-785

Authorship and Verification
----------------------------

* **Author:** NIST IGOR/DANSE **Date:** pre 2010
* **Converted to sasmodels by:** Paul Butler **Date:** March 19, 2016
* **Last Modified by:** Paul Butler **Date:** March 12, 2017
* **Last Reviewed by:** Paul Butler **Date:** March 12, 2017
"""
from __future__ import division

import numpy as np
from numpy import inf

name = "fractal"
title = "Calculates the scattering from fractal-like aggregates of spheres \
following theTexiera reference."
description = """
        The scattering intensity is given by
        I(q) = scale * V * delta^(2) * P(q) * S(q) + background, where
        p(q)= F(q*radius)^(2)
        F(x) = 3*[sin(x)-x cos(x)]/x**3
        delta = sld_block -sld_solv
        scale        =  scale * volfraction
        radius       =  Block radius
        sld_block    =  SDL block
        sld_solv  =  SDL solvent
        background   =  background
        and S(q) is the interference term between building blocks given
        in the full documentation and depending on the parameters
        fractal_dim  =  Fractal dimension
        cor_length  =  Correlation Length    """

category = "shape-independent"

# pylint: disable=bad-whitespace, line-too-long
#             ["name", "units", default, [lower, upper], "type","description"],
parameters = [["volfraction", "", 0.05, [0.0, 1], "",
               "volume fraction of blocks"],
              ["radius",    "Ang",  5.0, [0.0, inf], "volume",
               "radius of particles"],
              ["fractal_dim",      "",  2.0, [0.0, 6.0], "",
               "fractal dimension"],
              ["cor_length", "Ang", 100.0, [0.0, inf], "",
               "cluster correlation length"],
              ["sld_block", "1e-6/Ang^2", 2.0, [-inf, inf], "sld",
               "scattering length density of particles"],
              ["sld_solvent", "1e-6/Ang^2", 6.4, [-inf, inf], "sld",
               "scattering length density of solvent"],
             ]
# pylint: enable=bad-whitespace, line-too-long

source = ["lib/sas_3j1x_x.c", "lib/sas_gamma.c", "lib/fractal_sq.c", "fractal.c"]
valid = "fractal_dim >= 0.0"

def random():
    """Return a random parameter set for the model."""
    radius = 10**np.random.uniform(0.7, 4)
    #radius = 5
    cor_length = 10**np.random.uniform(0.7, 2)*radius
    #cor_length = 20*radius
    volfraction = 10**np.random.uniform(-3, -1)
    #volfraction = 0.05
    fractal_dim = 2*np.random.beta(3, 4) + 1
    #fractal_dim = 2
    pars = dict(
        #background=0, sld_block=1, sld_solvent=0,
        volfraction=volfraction,
        radius=radius,
        cor_length=cor_length,
        fractal_dim=fractal_dim,
    )
    return pars

# NOTE: test results taken from values returned by SasView 3.1.2
tests = [
    [{}, 0.0005, 40.4980069872],
    [{}, 0.234734468938, 0.0947143166058],
    [{}, 0.5, 0.0176878183458],
    ]
