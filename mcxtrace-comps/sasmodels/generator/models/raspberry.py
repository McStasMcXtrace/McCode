r"""
Definition
----------

The figure below shows a schematic of a large droplet surrounded by several
smaller particles forming a structure similar to that of Pickering emulsions.

.. figure:: img/raspberry_geometry.jpg

    Schematic of the raspberry model

In order to calculate the form factor of the entire complex, the
self-correlation of the large droplet, the self-correlation of the particles,
the correlation terms between different particles and the cross terms between
large droplet and small particles all need to be calculated.

Consider two infinitely thin shells of radii $R_1$ and $R_2$ separated by
distance $r$. The general structure of the equation is then the form factor
of the two shells multiplied by the phase factor that accounts for the
separation of their centers.

.. math::

    S(q) = \frac{\sin(qR_1)}{qR_1}\frac{\sin(qR_2)}{qR_2}\frac{\sin(qr)}{qr}

In this case, the large droplet and small particles are solid spheres rather
than thin shells. Thus the two terms must be integrated over $R_L$ and $R_S$
respectively using the weighting function of a sphere. We then obtain the
functions for the form of the two spheres:

.. math::

    \Psi_L = \int_0^{R_L}(4\pi R^2_L)\frac{\sin(qR_L)}{qR_L}dR_L =
    \frac{3[\sin(qR_L)-qR_L\cos(qR_L)]}{(qR_L)^2}

.. math::

    \Psi_S = \int_0^{R_S}(4\pi R^2_S)\frac{\sin(qR_S)}{qR_S}dR_S =
    \frac{3[\sin(qR_S)-qR_L\cos(qR_S)]}{(qR_S)^2}

The cross term between the large droplet and small particles is given by:

.. math::
    S_{LS} = \Psi_L\Psi_S\frac{\sin(q(R_L+\delta R_S))}{q(R_L+\delta\ R_S)}

and the self term between small particles is given by:

.. math::
    S_{SS} = \Psi_S^2\biggl[\frac{\sin(q(R_L+\delta R_S))}{q(R_L+\delta\ R_S)}
    \biggr]^2

The number of small particles per large droplet, $N_p$, is given by:

.. math::

    N_p = \frac{\phi_S\phi_\text{surface}V_L}{\phi_L V_S}

where $\phi_S$ is the volume fraction of small particles in the sample,
$\phi_\text{surface}$ is the fraction of the small particles that are adsorbed
to the large droplets, $\phi_L$ is the volume fraction of large droplets in the
sample, and $V_S$ and $V_L$ are the volumes of individual small particles and
large droplets respectively.

The form factor of the entire complex can now be calculated including the excess
scattering length densities of the components $\Delta\rho_L$ and $\Delta\rho_S$,
where $\Delta\rho_x = \left|\rho_x-\rho_\text{solvent}\right|$ :

.. math::

    P_{LS} = \frac{1}{M^2}\bigl[(\Delta\rho_L)^2V_L^2\Psi_L^2
                +N_p(\Delta\rho_S)^2V_S^2\Psi_S^2
                + N_p(1-N_p)(\Delta\rho_S)^2V_S^2S_{SS}
                + 2N_p\Delta\rho_L\Delta\rho_SV_LV_SS_{LS}\bigr]

where M is the total scattering length of the whole complex :

.. math::
    M = \Delta\rho_LV_L + N_p\Delta\rho_SV_S

In a real system, there will ususally be an excess of small particles such that
some fraction remain unbound. Therefore the overall scattering intensity is
given by:

.. math::
    I(Q) = I_{LS}(Q) + I_S(Q) = (\phi_L(\Delta\rho_L)^2V_L +
            \phi_S\phi_\text{surface}N_p(\Delta\rho_S)^2V_S)P_{LS}
            + \phi_S(1-\phi_\text{surface})(\Delta\rho_S)^2V_S\Psi_S^2

A useful parameter to extract is the fraction of the surface area of the large
droplets that is covered by small particles. This can be calculated from the
model parameters as:

.. math::
    \chi = \frac{4\phi_L\phi_\text{surface}(R_L+\delta R_S)}{\phi_LR_S}


References
----------

#. K Larson-Smith, A Jackson, and D C Pozzo, *Small angle scattering model
   for Pickering emulsions and raspberry particles*,
   *Journal of Colloid and Interface Science*, 343(1) (2010) 36-41

Authorship and Verification
----------------------------

* **Author:** Andrew Jackson **Date:** 2008
* **Last Modified by:** Andrew Jackson **Date:** March 20, 2016
* **Last Reviewed by:** Andrew Jackson **Date:** March 20, 2016
"""

import numpy as np
from numpy import inf

name = "raspberry"
title = "Calculates the form factor, *P(q)*, for a 'Raspberry-like' structure \
where there are smaller spheres at the surface of a larger sphere, such as the \
structure of a Pickering emulsion."
description = """
                RaspBerryModel:
                volfraction_lg = volume fraction large spheres
                radius_lg = radius large sphere (A)
                sld_lg = sld large sphere (A-2)
                volfraction_sm = volume fraction small spheres
                radius_sm = radius small sphere (A)
                surface_fraction = fraction of small spheres at surface
                sld_sm = sld small sphere
                penetration = small sphere penetration (A)
                sld_solvent   = sld solvent
                background = background (cm-1)
            Ref: J. coll. inter. sci. (2010) vol. 343 (1) pp. 36-41."""
category = "shape:sphere"


#             [ "name", "units", default, [lower, upper], "type", "description"],
parameters = [["sld_lg", "1e-6/Ang^2", -0.4, [-inf, inf], "sld",
               "large particle scattering length density"],
              ["sld_sm", "1e-6/Ang^2", 3.5, [-inf, inf], "sld",
               "small particle scattering length density"],
              ["sld_solvent", "1e-6/Ang^2", 6.36, [-inf, inf], "sld",
               "solvent scattering length density"],
              ["volfraction_lg", "", 0.05, [-inf, inf], "",
               "volume fraction of large spheres"],
              ["volfraction_sm", "", 0.005, [-inf, inf], "",
               "volume fraction of small spheres"],
              ["surface_fraction", "", 0.4, [-inf, inf], "",
               "fraction of small spheres at surface"],
              ["radius_lg", "Ang", 5000, [0, inf], "volume",
               "radius of large spheres"],
              ["radius_sm", "Ang", 100, [0, inf], "volume",
               "radius of small spheres"],
              ["penetration", "Ang", 0, [-1, 1], "volume",
               "fractional penetration depth of small spheres into large sphere"],
             ]

source = ["lib/sas_3j1x_x.c", "raspberry.c"]
radius_effective_modes = ["radius_large", "radius_outer"]

def random():
    """Return a random parameter set for the model."""
    # Limit volume fraction to 20% each
    volfraction_lg = 10**np.random.uniform(-3, -0.3)
    volfraction_sm = 10**np.random.uniform(-3, -0.3)
    # Prefer most particles attached (peak near 60%), but not all or none
    surface_fraction = np.random.beta(6, 4)
    radius_lg = 10**np.random.uniform(1.7, 4.7)  # 500 - 50000 A
    radius_sm = 10**np.random.uniform(-3, -0.3)*radius_lg  # 0.1% - 20%
    penetration = np.random.beta(1, 10) # up to 20% pen. for 90% of examples
    pars = dict(
        volfraction_lg=volfraction_lg,
        volfraction_sm=volfraction_sm,
        surface_fraction=surface_fraction,
        radius_lg=radius_lg,
        radius_sm=radius_sm,
        penetration=penetration,
    )
    return pars

# TODO: update tests so the parameters correspond to SasView parameters
# The model was re-parameterized so the results have changed.
# NOTE: test results taken from values returned by SasView 3.1.2, with
# 0.001 added for a non-zero default background.
#tests = [[{}, 0.0412755102041, 0.286669115234],
#         [{}, 0.5, 0.00103818393658],
#        ]
