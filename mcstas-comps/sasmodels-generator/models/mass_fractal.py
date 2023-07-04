r"""
Calculates the scattering from fractal-like aggregates based on
the Mildner reference.

Definition
----------

The scattering intensity $I(q)$ is calculated as

.. math::

    I(q) = scale \times P(q)S(q) + background

.. math::

    P(q) = F(qR)^2

.. math::

    F(x) = \frac{3\left[sin(x)-xcos(x)\right]}{x^3}

.. math::

    S(q) = \frac{\Gamma(D_m-1)\zeta^{D_m-1}}{\left[1+(q\zeta)^2
    \right]^{(D_m-1)/2}}
    \frac{sin\left[(D_m - 1) tan^{-1}(q\zeta) \right]}{q}

.. math::

    scale = scale\_factor \times NV^2(\rho_\text{particle} - \rho_\text{solvent})^2

.. math::

    V = \frac{4}{3}\pi R^3

where $R$ is the radius of the building block, $D_m$ is the **mass** fractal
dimension, $\zeta$ is the cut-off length, $\rho_\text{solvent}$ is the
scattering length density of the solvent, and $\rho_\text{particle}$ is the
scattering length density of particles.

.. note::

    The mass fractal dimension ( $D_m$ ) is only
    valid if $1 < mass\_dim < 6$. It is also only valid over a limited
    $q$ range (see the reference for details).


References
----------

#. D Mildner and P Hall,
   *J. Phys. D: Appl. Phys.*, 19 (1986) 1535-1545 Equation(9)

Authorship and Verification
----------------------------

* **Author:**
* **Last Modified by:**
* **Last Reviewed by:**
"""

import numpy as np
from numpy import inf

name = "mass_fractal"
title = "Mass Fractal model"
description = """
        The scattering intensity  I(x) = scale*P(x)*S(x) + background, where
        scale = scale_factor  * V * delta^(2)
        p(x)=  F(x*radius)^(2)
        F(x) = 3*[sin(x)-x cos(x)]/x**3
        S(x) = [(gamma(Dm-1)*colength^(Dm-1)*[1+(x^2*colength^2)]^((1-Dm)/2)
        * sin[(Dm-1)*arctan(x*colength)])/x]
        where delta = sldParticle -sldSolv.
        radius       =  Particle radius
        fractal_dim_mass  =  Mass fractal dimension
        cutoff_length  =  Cut-off length
        background   =  background
        Ref.:Mildner, Hall,J Phys D Appl Phys(1986), 9, 1535-1545
        Note I: This model is valid for 1<fractal_dim_mass<6.
        Note II: This model is not in absolute scale.
        """
category = "shape-independent"

# pylint: disable=bad-whitespace, line-too-long
#   ["name", "units", default, [lower, upper], "type","description"],
parameters = [
    ["radius",           "Ang",  10.0, [0.0, inf], "", "Particle radius"],
    ["fractal_dim_mass", "",      1.9, [1.0, 6.0], "", "Mass fractal dimension"],
    ["cutoff_length",    "Ang", 100.0, [0.0, inf], "", "Cut-off length"],
]
# pylint: enable=bad-whitespace, line-too-long

source = ["lib/sas_3j1x_x.c", "lib/sas_gamma.c", "mass_fractal.c"]
valid = "fractal_dim_mass >= 1.0"

def random():
    """Return a random parameter set for the model."""
    radius = 10**np.random.uniform(0.7, 4)
    cutoff_length = 10**np.random.uniform(0.7, 2)*radius
    # TODO: fractal dimension should range from 1 to 6
    fractal_dim_mass = 2*np.random.beta(3, 4) + 1
    #volfrac = 10**np.random.uniform(-4, -1)
    pars = dict(
        #background=0,
        scale=1, #1e5*volfrac/radius**(fractal_dim_mass),
        radius=radius,
        cutoff_length=cutoff_length,
        fractal_dim_mass=fractal_dim_mass,
    )
    return pars

tests = [

    # Accuracy tests based on content in test/utest_other_models.py
    [{'radius':         10.0,
      'fractal_dim_mass':        1.9,
      'cutoff_length': 100.0,
     }, 0.05, 279.59422],

    # Additional tests with larger range of parameters
    [{'radius':        2.0,
      'fractal_dim_mass':      3.3,
      'cutoff_length': 1.0,
     }, 0.5, 1.29116774904],

    [{'radius':        1.0,
      'fractal_dim_mass':      1.3,
      'cutoff_length': 1.0,
      'background':    0.8,
     }, 0.001, 1.69747015932],

    [{'radius':        1.0,
      'fractal_dim_mass':      2.3,
      'cutoff_length': 1.0,
      'scale':        10.0,
     }, 0.051, 11.6237966145],
    ]
