r"""
.. note:: Please read the Validation section below.

Definition
----------
This model calculates the structure factor of a polyelectrolyte solution with
the RPA expression derived by Borue and Erukhimovich\ [#Borue]_.  Note however
that the fitting procedure here does not follow the notation in that reference
as 's' and 't' are **not** decoupled. Instead the scattering intensity $I(q)$
is calculated as

.. math::

    I(q) &= K\frac{q^2+k^2}{4\pi L_b\alpha ^2}
        \frac{1}{1+r_{0}^4(q^2+k^2)(q^2-12hC_a/b^2)} + \text{background} \\
    k^2 &= 4\pi L_b(2C_s + \alpha C_a) \\
    r_{0}^2 &= \frac{b}{\alpha \sqrt{C_a 48\pi L_b}}

where

$K$ is the contrast factor for the polymer which is defined differently than in
other models and is given in barns where 1 $barn = 10^{-24}$ $cm^2$.  $K$ is
defined as:

.. math::

    K &= a^2 \\
    a &= b_p - (v_p/v_s) b_s

where:

- $b_p$ and $b_s$ are **sum of the scattering lengths of the atoms**
  constituting the polymer monomer and the solvent molecules, respectively.

- $v_p$ and $v_s$ are the partial molar volume of the polymer and the
  solvent, respectively.

- $L_b$ is the Bjerrum length (|Ang|) - **Note:** This parameter needs to be
  kept constant for a given solvent and temperature!

- $h$ is the virial parameter (|Ang^3|) - **Note:** See [#Borue]_ for the
  correct interpretation of this parameter.  It incorporates second and third
  virial coefficients and can be *negative*.

- $b$ is the monomer length (|Ang|).

- $C_s$ is the concentration of monovalent salt(1/|Ang^3| - internally
  converted from mol/L).

- $\alpha$ is the degree of ionization (the ratio of charged monomers to the
  total number of monomers)

- $C_a$ is the polymer molar concentration (1/|Ang^3| - internally converted
  from mol/L)

- $\text{background}$ is the incoherent background.

For 2D data the scattering intensity is calculated in the same way as 1D,
where the $\vec q$ vector is defined as

.. math::

    q = \sqrt{q_x^2 + q_y^2}

Validation
----------

As of the last revision, this code is believed to be correct.  However it
needs further validation and should be used with caution at this time.  The
history of this code goes back to a 1998 implementation. It was recently noted
that in that implementation, while both the polymer concentration and salt
concentration were converted from experimental units of mol/L to more
dimensionally useful units of 1/|Ang^3|, only the converted version of the
polymer concentration was actually being used in the calculation while the
unconverted salt concentration (still in apparent units of mol/L) was being
used.  This was carried through to Sasmodels as used for SasView 4.1 (though
the line of code converting the salt concentration to the new units was removed
somewhere along the line). Simple dimensional analysis of the calculation shows
that the converted salt concentration should be used, which the original code
suggests was the intention, so this has now been corrected (for SasView 4.2).
Once better validation has been performed this note will be removed.

References
----------

For further details, see [#Joanny1990]_, [#Moussaid1993]_, [#Raphael1990]_.

.. [#Borue] V Y Borue, I Y Erukhimovich, *Macromolecules*, 21 (1988) 3240
.. [#Joanny1990] J F Joanny, L Leibler, *Journal de Physique*, 51 (1990) 545
.. [#Moussaid1993] A Moussaid, F Schosseler, J P Munch, S Candau,
   *J. Journal de Physique II France*, 3 (1993) 573
.. [#Raphael1990] E Raphael, J F Joanny, *Europhysics Letters*, 11 (1990) 179

Authorship and Verification
----------------------------

* **Author:** NIST IGOR/DANSE **Date:** pre 2010
* **Last Modified by:** Paul Butler **Date:** September 25, 2018
* **Last Reviewed by:** Paul Butler **Date:** September 25, 2018
"""

import numpy as np
from numpy import inf, pi, sqrt

name = "be_polyelectrolyte"
title = "Polyelectrolyte with the RPA expression derived by Borue and Erukhimovich"
description = """
            Evaluate
            F(x) = K 1/(4 pi Lb (alpha)^(2)) (q^(2)+k2)/(1+(r02)^(2))
                 (q^(2)+k2) (q^(2)-(12 h C/b^(2)))

            has 3 internal parameters :
                   The inverse Debye Length: K2 = 4 pi Lb (2 Cs+alpha C)
                   r02 =1/alpha/Ca^(0.5) (B/(48 pi Lb)^(0.5))
                   Ca = 6.022136e-4 C
            """
category = "shape-independent"

# pylint: disable=bad-whitespace, line-too-long
#   ["name", "units", default, [lower, upper], "type", "description"],
parameters = [
    ["contrast_factor",       "barns",   10.0,  [-inf, inf], "", "Contrast factor of the polymer"],
    ["bjerrum_length",        "Ang",      7.1,  [0, inf],    "", "Bjerrum length"],
    ["virial_param",          "Ang^3", 12.0,  [-inf, inf], "", "Virial parameter"],
    ["monomer_length",        "Ang",     10.0,  [0, inf],    "", "Monomer length"],
    ["salt_concentration",    "mol/L",    0.0,  [-inf, inf], "", "Concentration of monovalent salt"],
    ["ionization_degree",     "",         0.05, [0, inf],    "", "Degree of ionization"],
    ["polymer_concentration", "mol/L",    0.7,  [0, inf],    "", "Polymer molar concentration"],
    ]
# pylint: enable=bad-whitespace, line-too-long


def Iq(q,
       contrast_factor,
       bjerrum_length,
       virial_param,
       monomer_length,
       salt_concentration,
       ionization_degree,
       polymer_concentration):
    """
    :params: see parameter table
    :return: 1-D form factor for polyelectrolytes in low salt

    parameter names, units, default values, and behavior (volume, sld etc) are
    defined in the parameter table.  The concentrations are converted from
    experimental mol/L to dimensionally useful 1/A3 in first two lines
    """

    concentration_pol = polymer_concentration * 6.022136e-4
    concentration_salt = salt_concentration * 6.022136e-4

    k_square = 4.0 * pi * bjerrum_length * (2*concentration_salt +
                                            ionization_degree * concentration_pol)

    r0_square = 1.0/ionization_degree/sqrt(concentration_pol) * \
                (monomer_length/sqrt((48.0*pi*bjerrum_length)))

    term1 = contrast_factor/(4.0 * pi * bjerrum_length *
                             ionization_degree**2) * (q**2 + k_square)

    term2 = 1.0 + r0_square**2 * (q**2 + k_square) * \
        (q**2 - (12.0 * virial_param * concentration_pol/(monomer_length**2)))

    return term1/term2

Iq.vectorized = True  # Iq accepts an array of q values

def random():
    """Return a random parameter set for the model."""
    # TODO: review random be_polyelectrolyte model generation
    pars = dict(
        scale=10000, #background=0,
        #polymer_concentration=0.7,
        polymer_concentration=np.random.beta(5, 3), # around 70%
        #salt_concentration=0.0,
        # keep salt concentration extremely low
        # and use explicit molar to match polymer concentration
        salt_concentration=np.random.beta(1, 100)*6.022136e-4,
        #contrast_factor=10.0,
        contrast_fact=np.random.uniform(1, 100),
        #bjerrum_length=7.1,
        bjerrum_length=np.random.uniform(1, 10),
        #virial_param=12.0,
        virial_param=np.random.uniform(-1000, 30),
        #monomer_length=10.0,
        monomer_length=10.0**(4*np.random.beta(1.5, 3)),
        #ionization_degree=0.05,
        ionization_degree=np.random.beta(1.5, 4),
    )
    return pars

tests = [

    # Accuracy tests based on content in test/utest_other_models.py
    # Note that these should some day be validated beyond this self validation
    # (circular reasoning). -- i.e. the "good value," at least for those with
    # non zero salt concentrations, were obtained by running the current
    # model in SasView and copying the appropriate result here.
    #    PDB -- Sep 26, 2018
    [{'contrast_factor':       10.0,
      'bjerrum_length':         7.1,
      'virial_param':          12.0,
      'monomer_length':        10.0,
      'salt_concentration':     0.0,
      'ionization_degree':      0.05,
      'polymer_concentration':  0.7,
      'background':             0.001,
     }, 0.001, 0.0948379],

    [{'contrast_factor':       10.0,
      'bjerrum_length':       100.0,
      'virial_param':           3.0,
      'monomer_length':         5.0,
      'salt_concentration':     1.0,
      'ionization_degree':      0.1,
      'polymer_concentration':  1.0,
      'background':             0.0,
     }, 0.1, 0.253469484],

    [{'contrast_factor':       10.0,
      'bjerrum_length':       100.0,
      'virial_param':           3.0,
      'monomer_length':         5.0,
      'salt_concentration':     1.0,
      'ionization_degree':      0.1,
      'polymer_concentration':  1.0,
      'background':             1.0,
     }, 0.05, 1.738358122],

    [{'contrast_factor':     100.0,
      'bjerrum_length':       10.0,
      'virial_param':         12.0,
      'monomer_length':       10.0,
      'salt_concentration':    0.1,
      'ionization_degree':     0.5,
      'polymer_concentration': 0.1,
      'background':           0.01,
     }, 0.5, 0.012881893],
    ]
