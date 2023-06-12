r"""
.. warning:: This model is not functioning correctly in SasView and it
             appears it has not done so for some time. Whilst the
             problem is investigated, a workaround for Case 0 below
             (the most common use case) is to use the binary_blend
             model available on the `Model Maketplace 
             <https://marketplace.sasview.org/models/124/>`_ . For further
             information, please email help@sasview.org . *The
             SasView Developers. February 2022.*

Definition
----------

Calculates the macroscopic scattering intensity for a multi-component
homogeneous mixture of polymers using the Random Phase Approximation.
This general formalism contains 10 specific cases

Case 0: C/D binary mixture of homopolymers

Case 1: C-D diblock copolymer

Case 2: B/C/D ternary mixture of homopolymers

Case 3: C/C-D mixture of a homopolymer B and a diblock copolymer C-D

Case 4: B-C-D triblock copolymer

Case 5: A/B/C/D quaternary mixture of homopolymers

Case 6: A/B/C-D mixture of two homopolymers A/B and a diblock C-D

Case 7: A/B-C-D mixture of a homopolymer A and a triblock B-C-D

Case 8: A-B/C-D mixture of two diblock copolymers A-B and C-D

Case 9: A-B-C-D tetra-block copolymer

.. note::
    These case numbers are different from those in the NIST SANS package!

The models are based on the papers by Akcasu *et al.* [1] and by
Hammouda [2] assuming the polymer follows Gaussian statistics such
that $R_g^2 = n b^2/6$ where $b$ is the statistical segment length and $n$ is
the number of statistical segment lengths. A nice tutorial on how these are
constructed and implemented can be found in chapters 28, 31 and 34, and Part H,
of Hammouda's 'SANS Toolbox' [3].

In brief, the macroscopic cross sections are derived from the general forms
for homopolymer scattering and the multiblock cross-terms while the inter,
polymer cross terms are described in the usual way by the $\chi$ parameter.

USAGE NOTES:

* Only one case can be used at any one time.
* The RPA (mean field) formalism only applies only when the multicomponent
  polymer mixture is in the homogeneous mixed-phase region.
* **Component D is assumed to be the "background" component (ie, all contrasts
  are calculated with respect to component D).** So the scattering contrast
  for a C/D blend $\rho_{C/D} = [\rho_C - \rho_D]$\ :sup:`2`.
* Depending on which case is being used, the number of fitting parameters can
  vary.

  .. Note::
    * In general the degrees of polymerization, the volume
      fractions, the molar volumes, and the neutron scattering lengths for each
      component are obtained from other methods and held fixed while The *scale*
      parameter should be held equal to unity.
    * The variables are normally the segment lengths ($b_a$, $b_b$,
      etc.) and $\chi$ parameters ($K_{ab}$, $K_{ac}$, etc).

References
----------

#. A Z Akcasu, R Klein and B Hammouda, *Macromolecules*, 26 (1993) 4136
#. B. Hammouda, *Advances in Polymer Science* 106 (1993) 87
#. B. Hammouda, *SANS Toolbox*
   https://www.ncnr.nist.gov/staff/hammouda/the_sans_toolbox.pdf.

Authorship and Verification
----------------------------

* **Author:** Boualem Hammouda - NIST IGOR/DANSE **Date:** pre 2010
* **Converted to sasmodels by:** Paul Kienzle **Date:** July 18, 2016
* **Last Modified by:** Paul Butler **Date:** March 12, 2017
* **Last Reviewed by:** Steve King **Date:** March 27, 2019
"""

from numpy import inf

name = "rpa"
title = "Random Phase Approximation"
description = """
This formalism applies to multicomponent polymer mixtures in the
homogeneous (mixed) phase region only.
Case 0: C/D binary mixture of homopolymers
Case 1: C-D diblock copolymer
Case 2: B/C/D ternary mixture of homopolymers
Case 3: B/C-D mixture of homopolymer b and diblock copolymer C-D
Case 4: B-C-D triblock copolymer
Case 5: A/B/C/D quaternary mixture of homopolymers
Case 6: A/B/C-D mixture of two homopolymers A/B and a diblock C-D
Case 7: A/B-C-D mixture of a homopolymer A and a triblock B-C-D
Case 8: A-B/C-D mixture of two diblock copolymers A-B and C-D
Case 9: A-B-C-D four-block copolymer
See details in the model function help
"""
category = "shape-independent"

CASES = [
    "C+D binary mixture",
    "C:D diblock copolymer",
    "B+C+D ternary mixture",
    "B+C:D binary mixture",
    "B:C:D triblock copolymer",
    "A+B+C+D quaternary mixture",
    "A+B+C:D ternary mixture",
    "A+B:C:D binary mixture",
    "A:B+C:D binary mixture",
    "A:B:C:D quadblock copolymer",
]

#   ["name", "units", default, [lower, upper], "type","description"],
parameters = [
    ["case_num", "", 1, [CASES], "", "Component organization"],

    ["N[4]", "", 1000.0, [1, inf], "", "Degree of polymerization"],
    ["Phi[4]", "", 0.25, [0, 1], "", "volume fraction"],
    ["v[4]", "mL/mol", 100.0, [0, inf], "", "molar volume"],
    ["L[4]", "fm", 10.0, [-inf, inf], "", "scattering length"],
    ["b[4]", "Ang", 5.0, [0, inf], "", "segment length"],

    ["K12", "", -0.0004, [-inf, inf], "", "A:B interaction parameter"],
    ["K13", "", -0.0004, [-inf, inf], "", "A:C interaction parameter"],
    ["K14", "", -0.0004, [-inf, inf], "", "A:D interaction parameter"],
    ["K23", "", -0.0004, [-inf, inf], "", "B:C interaction parameter"],
    ["K24", "", -0.0004, [-inf, inf], "", "B:D interaction parameter"],
    ["K34", "", -0.0004, [-inf, inf], "", "C:D interaction parameter"],
]


source = ["rpa.c"]
single = False

control = "case_num"
HIDE_ALL = set("Phi4".split())
HIDE_A = set("N1 Phi1 v1 L1 b1 K12 K13 K14".split()).union(HIDE_ALL)
HIDE_AB = set("N2 Phi2 v2 L2 b2 K23 K24".split()).union(HIDE_A)
def hidden(case_num):
    """
    Return a list of parameters to hide depending on the multiplicity parameter.
    """
    case_num = int(case_num+0.5)
    if case_num < 2:
        return HIDE_AB
    elif case_num < 5:
        return HIDE_A
    else:
        return HIDE_ALL

# TODO: no random parameters generated for RPA
