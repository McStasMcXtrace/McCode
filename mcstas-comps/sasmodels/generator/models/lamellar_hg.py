# Note: model title and parameter table are inserted automatically
r"""
This model provides the scattering intensity, $I(q)$, for a lyotropic lamellar
phase where a random distribution in solution are assumed. The SLD of the head
region is taken to be different from the SLD of the tail region.

Definition
----------

The scattering intensity $I(q)$ is

.. math::

   I(q) = 2\pi\frac{\text{scale}}{2(\delta_H + \delta_T)}  P(q) \frac{1}{q^2}

The form factor $P(q)$ is

.. math::

    P(q) = \frac{4}{q^2}
        \left\lbrace
            \Delta \rho_H
            \left[\sin[q(\delta_H + \delta_T)\ - \sin(q\delta_T)\right]
            + \Delta\rho_T\sin(q\delta_T)
        \right\rbrace^2

where $\delta_T$ is *length_tail*, $\delta_H$ is *length_head*,
$\Delta\rho_H$ is the head contrast (*sld_head* $-$ *sld_solvent*),
and $\Delta\rho_T$ is tail contrast (*sld* $-$ *sld_solvent*).

The total thickness of the lamellar sheet is
a_H + \delta_T + \delta_T + \delta_H$. Note that in a non aqueous solvent
the chemical "head" group may be the "Tail region" and vice-versa.

The 2D scattering intensity is calculated in the same way as 1D, where
the $q$ vector is defined as

.. math:: q = \sqrt{q_x^2 + q_y^2}


References
----------

#. F Nallet, R Laversanne, and D Roux, *J. Phys. II France*, 3, (1993) 487-502
#. J Berghausen, J Zipfel, P Lindner, W Richtering,
   *J. Phys. Chem. B*, 105, (2001) 11081-11088

Authorship and Verification
----------------------------

* **Author:**
* **Last Modified by:**
* **Last Reviewed by:** S King and P Butler **Date** April 17, 2014
"""

import numpy as np
from numpy import inf

name = "lamellar_hg"
title = "Random lamellar phase with Head and Tail Groups"
description = """\
    [Random lamellar phase with Head and Tail Groups]
        I(q)= 2*pi*P(q)/(2(H+T)*q^(2)), where
        P(q)= see manual
        layer thickness =(H+T+T+H) = 2(Head+Tail)
        sld = Tail scattering length density
        sld_head = Head scattering length density
        sld_solvent = solvent scattering length density
        background = incoherent background
        scale = scale factor
"""
category = "shape:lamellae"

# pylint: disable=bad-whitespace, line-too-long
#             ["name", "units", default, [lower, upper], "type","description"],
parameters = [["length_tail", "Ang",       15,   [0, inf],  "volume",  "Tail thickness ( total = H+T+T+H)"],
              ["length_head", "Ang",       10,   [0, inf],  "volume",  "Head thickness"],
              ["sld",         "1e-6/Ang^2", 0.4, [-inf,inf], "sld",    "Tail scattering length density"],
              ["sld_head",    "1e-6/Ang^2", 3.0, [-inf,inf], "sld",    "Head scattering length density"],
              ["sld_solvent", "1e-6/Ang^2", 6,   [-inf,inf], "sld",    "Solvent scattering length density"]]
# pylint: enable=bad-whitespace, line-too-long

# No volume normalization despite having a volume parameter
# This should perhaps be volume normalized?
form_volume = """
    return 1.0;
    """

source = ["lamellar_hg.c"]

def random():
    """Return a random parameter set for the model."""
    thickness = 10**np.random.uniform(1, 4)
    length_head = thickness * np.random.uniform(0, 1)
    length_tail = thickness - length_head
    pars = dict(
        length_head=length_head,
        length_tail=length_tail,
    )
    return pars

#
tests = [
    [{'scale': 1.0, 'background': 0.0, 'length_tail': 15.0, 'length_head': 10.0,
      'sld': 0.4, 'sld_head': 3.0, 'sld_solvent': 6.0},
     [0.001], [653143.9209]],
]
# ADDED by: RKH  ON: 18Mar2016  converted from sasview previously, now renaming everything & sorting the docs
