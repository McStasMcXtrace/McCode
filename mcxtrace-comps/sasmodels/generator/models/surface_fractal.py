r"""
This model calculates the scattering from fractal-like aggregates based
on the Mildner reference.

Definition
----------

The scattering intensity $I(q)$ is calculated as

.. math::
    :nowrap:

    \begin{align*}
    I(q) &= \text{scale} \times P(q)S(q) + \text{background} \\
    P(q) &= F(qR)^2 \\
    F(x) &= \frac{3\left[\sin(x)-x\cos(x)\right]}{x^3} \\
    S(q) &= \Gamma(5-D_S)\xi^{\,5-D_S}\left[1+(q\xi)^2 \right]^{-(5-D_S)/2}
            \sin\left[-(5-D_S) \tan^{-1}(q\xi) \right] q^{-1} \\
    \text{scale} &= \text{scale factor}\,
        N V^1(\rho_\text{particle} - \rho_\text{solvent})^2 \\
    V &= \frac{4}{3}\pi R^3
    \end{align*}

where $R$ is the radius of the building block, $D_S$ is the **surface** fractal
dimension, $\xi$ is the cut-off length, $\rho_\text{solvent}$ is the scattering
length density of the solvent and $\rho_\text{particle}$ is the scattering
length density of particles.

.. note::

    The surface fractal dimension is only valid if $1<D_S<3$. The result is only
    valid over a limited $q$ range, $\tfrac{5}{3-D_S}\xi^{\,-1} < q < R^{-1}$.
    See the reference for details.


References
----------

#.  D Mildner and P Hall, *J. Phys. D: Appl. Phys.*, 19 (1986) 1535-1545

Authorship and Verification
----------------------------

* **Author:**
* **Last Modified by:**
* **Last Reviewed by:**
"""

import numpy as np
from numpy import inf

name = "surface_fractal"
title = "Fractal-like aggregates based on the Mildner reference"
description = """\
    [The scattering intensity  I(x) = scale*P(x)*S(x) + background, where
        scale = scale_factor  * V * delta^(2)
        p(x) = F(x*radius)^(2)
        F(x) = 3*[sin(x)-x cos(x)]/x**3
        S(x) = [(gamma(5-Ds)*colength^(5-Ds)*[1+(x^2*colength^2)]^((Ds-5)/2)
             * sin[(Ds-5)*arctan(x*colength)])/x]
        where
        delta        =  sldParticle -sldSolv.
        radius       =  Particle radius
        fractal_dim_surf  =  Surface fractal dimension (Ds)
        co_length    =  Cut-off length
        background   =  background

        Ref.   :Mildner, Hall,J Phys D Appl Phys(1986), 19, 1535-1545
        Note I : This model is valid for 1<fractal_dim_surf<3 with limited q range.
        Note II: This model is not in absolute scale.
"""
category = "shape-independent"

# pylint: disable=bad-whitespace, line-too-long
#             ["name", "units", default, [lower, upper], "type","description"],
parameters = [["radius",        "Ang", 10.0, [0, inf],   "",
               "Particle radius"],
              ["fractal_dim_surf",   "",    2.0,  [1, 3],   "",
               "Surface fractal dimension"],
              ["cutoff_length", "Ang", 500., [0.0, inf], "",
               "Cut-off Length"],
             ]
# pylint: enable=bad-whitespace, line-too-long

source = ["lib/sas_3j1x_x.c", "lib/sas_gamma.c", "surface_fractal.c"]
# Don't need validity test since fractal_dim_surf is not polydisperse
#valid = "fractal_dim_surf > 1.0 && fractal_dim_surf < 3.0"

def random():
    """Return a random parameter set for the model."""
    radius = 10**np.random.uniform(1, 4)
    fractal_dim_surf = np.random.uniform(1, 3-1e-6)
    cutoff_length = 1e6  # Sets the low q limit; keep it big for sim
    pars = dict(
        #background=0,
        scale=1,
        radius=radius,
        fractal_dim_surf=fractal_dim_surf,
        cutoff_length=cutoff_length,
    )
    return pars

tests = [
    # Accuracy tests based on content in test/utest_other_models.py
    [{'radius': 10.0,
      'fractal_dim_surf': 2.0,
      'cutoff_length': 500.0,
     }, 0.05, 301428.66016],

    # Additional tests with larger range of parameters
    [{'radius': 1.0,
      'fractal_dim_surf': 1.0,
      'cutoff_length': 10.0,
     }, 0.332070182643, 1125.00421004],

    [{'radius': 3.5,
      'fractal_dim_surf': 0.1,
      'cutoff_length': 30.0,
      'background': 0.01,
     }, 5.0, 0.00999998891322],

    [{'radius': 3.0,
      'fractal_dim_surf': 1.0,
      'cutoff_length': 33.0,
      'scale': 0.1,
     }, 0.51, 2.50120147004],
    ]
