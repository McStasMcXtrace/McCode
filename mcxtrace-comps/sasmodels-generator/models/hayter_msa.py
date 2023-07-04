# Note: model title and parameter table are inserted automatically
r"""
Calculates the interparticle structure factor for a system of charged,
spheroidal, objects in a dielectric medium [1,2]. When combined with an
appropriate form factor $P(q)$, this allows for inclusion of the
interparticle interference effects due to screened Coulombic
repulsion between the charged particles.

.. note::

   This routine only works for charged particles! If the charge is set
   to zero the routine may self-destruct! For uncharged particles use
   the :ref:`hardsphere` $S(q)$ instead. The upper limit for the charge
   is limited to 200e to avoid numerical instabilities.

.. note::

   Earlier versions of SasView did not incorporate the so-called
   $\beta(q)$ ("beta") correction [3] for polydispersity and non-sphericity.
   This is only available in SasView versions 5.0 and higher.

The salt concentration is used to compute the ionic strength of the solution
which in turn is used to compute the Debye screening length. There is no
provision for entering the ionic strength directly. **At present the
counterions are assumed to be monovalent**, though it should be possible
to simulate the effect of multivalent counterions by increasing the salt
concentration.

Over the range 0 - 100 C the dielectric constant $\kappa$ of water may be
approximated with a maximum deviation of 0.01 units by the empirical
formula [4]

.. math::

    \kappa = 87.740 - 0.40008 T + 9.398x10^{-4} T^2 - 1.410x10^{-6} T^3

where $T$ is the temperature in celsius.

In SasView the effective radius may be calculated from the parameters
used in the form factor $P(q)$ that this $S(q)$ is combined with.

The computation uses a Taylor series expansion at very small rescaled $qR$, to
avoid some serious rounding error issues, this may result in a minor artefact
in the transition region under some circumstances.

For 2D data, the scattering intensity is calculated in the same way as 1D,
where the $q$ vector is defined as

.. math::

    q = \sqrt{q_x^2 + q_y^2}


References
----------

#. J B Hayter and J Penfold, *Molecular Physics*, 42 (1981) 109-118

#. J P Hansen and J B Hayter, *Molecular Physics*, 46 (1982) 651-656

#. M Kotlarchyk and S-H Chen, *J. Chem. Phys.*, 79 (1983) 2461-2469

#. C G Malmberg and A A Maryott, *J. Res. Nat. Bureau Standards*, 56 (1956) 2641

Authorship and Verification
----------------------------

* **Author:**
* **Last Modified by:**
* **Last Reviewed by:** Steve King **Date:** March 28, 2019
"""

import numpy as np
from numpy import inf

category = "structure-factor"
structure_factor = True
single = False  # double precision only!

#  dp[0] = 2.0*radius_effective();
#  dp[1] = fabs(charge());
#  dp[2] = volfraction();
#  dp[3] = temperature();
#  dp[4] = concentration_salt();
#  dp[5] = dielectconst();




name = "hayter_msa"
title = "Hayter-Penfold Rescaled Mean Spherical Approximation (RMSA) structure factor for charged spheres"
description = """\
    [Hayter-Penfold RMSA charged sphere interparticle S(Q) structure factor]
        Interparticle structure factor S(Q) for charged hard spheres.
    This routine only works for charged particles! For uncharged particles
    use the hardsphere S(q) instead. The "beta(q)" correction is available
    in versions 4.2.2 and higher.
"""


# pylint: disable=bad-whitespace, line-too-long
#             [ "name", "units", default, [lower, upper], "type", "description" ],
#
# NOTE: SMK, 28Mar19 The upper limit for charge is set to 200 to avoid instabilities noted by PK in
#       Ticket #1152. Also see the thread in Ticket 859. The docs above also note that charge=0 will
#       cause problems, yet the default parameters allowed it! After discussions with PK I have
#       changed it to (an arbitarily) small but non-zero value.  But I haven't changed the low limit
#       in function random() below.
#
parameters = [
    ["radius_effective", "Ang", 20.75,   [0, inf],    "volume", "effective radius of charged sphere"],
    ["volfraction",   "None",     0.0192, [0, 0.74],   "", "volume fraction of spheres"],
    ["charge",        "e",   19.0,    [0.000001, 200],    "", "charge on sphere (in electrons)"],
    ["temperature",   "K",  318.16,   [0, 450],    "", "temperature, in Kelvin, for Debye length calculation"],
    # TODO: demo parameters had concentration_salt=0.05
    ["concentration_salt",      "M",    0.0,    [0, inf], "", "conc of salt, moles/litre, 1:1 electolyte, for Debye length"],
    ["dielectconst",  "None",    71.08,   [-inf, inf], "", "dielectric constant (relative permittivity) of solvent, kappa, default water, for Debye length"]
    ]
# pylint: enable=bad-whitespace, line-too-long

source = ["hayter_msa.c"]
# No volume normalization despite having a volume parameter
# This should perhaps be volume normalized?
form_volume = """
    return 1.0;
    """

def random():
    """Return a random parameter set for the model."""
    # TODO: too many failures for random hayter_msa parameters
    pars = dict(
        scale=1, background=0,
        radius_effective=10**np.random.uniform(1, 4.7),
        volfraction=10**np.random.uniform(-2, 0),  # high volume fraction
        charge=min(int(10**np.random.uniform(0, 1.3)+0.5), 200),
        temperature=10**np.random.uniform(0, np.log10(450)), # max T = 450
        #concentration_salt=10**np.random.uniform(-3, 1),
        dialectconst=10**np.random.uniform(0, 6),
        #charge=10,
        #temperature=318.16,
        concentration_salt=0.0,
        #dielectconst=71.08,
    )
    return pars

# To run the demo use
#   sascomp -midQ -linear conentration_salt=0.05 radius_effective_pd=0.1 \
#       radius_effective_pd_n=40
# Note the calculation varies in different limiting cases so a wide range of
# parameters will be required for a thorough test!
# Odd that the default st has concentration_salt zero
#
# Attempt to use same values as old sasview unit test at Q=.001 was 0.0712928,
# then add lots new ones assuming values from new model are OK, need some
# low Q values to test the small Q Taylor expansion.
tests = [
    [{'scale': 1.0,
      'background': 0.0,
      'radius_effective': 20.75,
      'charge': 19.0,
      'volfraction': 0.0192,
      'temperature': 298.0,
      'concentration_salt': 0,
      'dielectconst': 78.0,
      'radius_effective_pd': 0},
     [0.00001, 0.0010, 0.01, 0.075], [0.0711646, 0.0712928, 0.0847006, 1.07150]],
    [{'scale': 1.0,
      'background': 0.0,
      'radius_effective': 20.75,
      'charge': 19.0,
      'volfraction': 0.0192,
      'temperature': 298.0,
      'concentration_salt': 0.05,
      'dielectconst': 78.0,
      'radius_effective_pd': 0.1,
      'radius_effective_pd_n': 40},
     [0.00001, 0.0010, 0.01, 0.075], [0.450272, 0.450420, 0.465116, 1.039625]]
    ]
# ADDED by:  RKH  ON: 16Mar2016 converted from sasview, new Taylor expansion at smallest rescaled Q
