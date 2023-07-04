r"""
For information about polarised and magnetic scattering, see
the :ref:`magnetism` documentation.

Definition
----------

The 1D scattering intensity is calculated in the following way (Guinier, 1955)

.. math::

    I(q) = \frac{\text{scale}}{V} \cdot \left[
        3V(\Delta\rho) \cdot \frac{\sin(qr) - qr\cos(qr))}{(qr)^3}
        \right]^2 + \text{background}

where *scale* is a volume fraction, $V$ is the volume of the scatterer,
$r$ is the radius of the sphere and *background* is the background level.
*sld* and *sld_solvent* are the scattering length densities (SLDs) of the
scatterer and the solvent respectively, whose difference is $\Delta\rho$.

Note that if your data is in absolute scale, the *scale* should represent
the volume fraction (which is unitless) if you have a good fit. If not,
it should represent the volume fraction times a factor (by which your data
might need to be rescaled).

The 2D scattering intensity is the same as above, regardless of the
orientation of $\vec q$.

Validation
----------

Validation of our code was done by comparing the output of the 1D model
to the output of the software provided by the NIST (Kline, 2006).


References
----------

#. A Guinier and G. Fournet, *Small-Angle Scattering of X-Rays*,
   John Wiley and Sons, New York, (1955)

Authorship and Verification
----------------------------

* **Author:**
* **Last Modified by:**
* **Last Reviewed by:** S King and P Parker **Date:** 2013/09/09 and 2014/01/06
"""

import numpy as np
from numpy import inf

name = "sphere"
title = "Spheres with uniform scattering length density"
description = """\
P(q)=(scale/V)*[3V(sld-sld_solvent)*(sin(qr)-qr cos(qr))
                /(qr)^3]^2 + background
    r: radius of sphere
    V: The volume of the scatter
    sld: the SLD of the sphere
    sld_solvent: the SLD of the solvent
"""
category = "shape:sphere"

#             ["name", "units", default, [lower, upper], "type","description"],
parameters = [["sld", "1e-6/Ang^2", 1, [-inf, inf], "sld",
               "Layer scattering length density"],
              ["sld_solvent", "1e-6/Ang^2", 6, [-inf, inf], "sld",
               "Solvent scattering length density"],
              ["radius", "Ang", 50, [0, inf], "volume",
               "Sphere radius"],
             ]

source = ["lib/sas_3j1x_x.c", "sphere.c"]
have_Fq = True
radius_effective_modes = ["radius"]
#single = False

def random():
    """Return a random parameter set for the model."""
    radius = 10**np.random.uniform(1.3, 4)
    pars = dict(
        radius=radius,
    )
    return pars
#2345678901234567890123456789012345678901234567890123456789012345678901234567890
tests = [
    [{}, 0.2, 0.726362],  # each test starts with default parameter values
    #            inside { }, unless modified. Then Q and expected value of I(Q)
    # putting None for an expected result will pass the test if there are no
    # errors from the routine, but without any check on the value of the result
    [{"scale": 1., "background": 0., "sld": 6., "sld_solvent": 1.,
      "radius": 120.},
     [0.01, 0.1, 0.2], [1.34836265e+04, 6.20114062e+00, 1.04733914e-01]],
    [{"scale": 1., "background": 0., "sld": 6., "sld_solvent": 1.,
      #  careful tests here R=120 Pd=.2, then with S(Q) at default Reff=50
      #  (but this gets changed to 120) phi=0,2
      "radius": 120., "radius_pd": 0.2, "radius_pd_n": 45},
     [0.01, 0.1, 0.2], [1.74395295e+04, 3.68016987e+00, 2.28843099e-01]],
    # a list of Q values and list of expected results is also possible
    [{"scale": 1., "background": 0., "sld": 6., "sld_solvent": 1.,
      "radius": 120., "radius_pd": 0.2, "radius_pd_n": 45},
     0.01, 335839.88055473, 1.41045057e+11, 120.0, 8087664.122641933, 1.0],
    # the longer list here checks  F1, F2, R_eff, volume, volume_ratio
    [{"radius": 120., "radius_pd": 0.2, "radius_pd_n": 45},
     0.1, 482.93824329, 29763977.79867414, 120.0, 8087664.122641933, 1.0],
    [{"radius": 120., "radius_pd": 0.2, "radius_pd_n": 45},
     0.2, 1.23330406, 1850806.1197361, 120.0, 8087664.122641933, 1.0],
    #  But note P(Q) = F2/volume
    #  F and F^2 are "unscaled", with for  n <F F*>S(q) or for beta approx
    #          I(q) = n [<F F*> + <F><F*> (S(q) - 1)]
    #  for n the number density and <.> the orientation average, and
    #  F = integral rho(r) exp(i q . r) dr.
    #  The number density is volume fraction divided by particle volume.
    #  Effectively, this leaves F = V drho form, where form is the usual
    #  3 j1(qr)/(qr) or whatever depending on the shape.
    # @S RESULTS using F1 and F2 from the longer test string above:
    #
    # I(Q) = (F2 + F1^2*(S(Q) -1))*volfraction*scale/Volume  + background
    #
    # with by default scale=1.0, background=0.001
    # NOTE currently S(Q) volfraction is also included in scaling
    #  structure_factor_mode 0 = normal decoupling approx,
    #                        1 = beta(Q) approx
    # radius_effective_mode  0 is for free choice,
    #                        1 is use radius from F2(Q)
    #    (sphere only has two choices, other models may have more)
    [{"@S": "hardsphere",
      "radius": 120., "radius_pd": 0.2, "radius_pd_n": 45, "volfraction": 0.2,
      #"radius_effective":50.0,    # hard sphere structure factor
      "structure_factor_mode": 1,  # mode 0 = normal decoupling approx,
      #                                   1 = beta(Q) approx
      "radius_effective_mode": 0   # this used default hardsphere Reff=50
     }, [0.01, 0.1, 0.2], [1.32473756e+03, 7.36633631e-01, 4.67686201e-02]],
    [{"@S": "hardsphere",
      "radius": 120., "radius_pd": 0.2, "radius_pd_n": 45,
      "volfraction": 0.2,
      "radius_effective": 45.0,     # explicit Reff over rides either 50 or 120
      "structure_factor_mode": 1,  # beta approx
      "radius_effective_mode": 0   #
      }, 0.01, 1316.2990966463444],
    [{"@S": "hardsphere",
      "radius": 120., "radius_pd": 0.2, "radius_pd_n": 45,
      "volfraction": 0.2,
      "radius_effective": 120.0,    # over ride Reff
      "structure_factor_mode": 1,  # beta approx
      "radius_effective_mode": 0   # (mode=1 here also uses 120)
     }, [0.01, 0.1, 0.2], [1.57928589e+03, 7.37067923e-01, 4.67686197e-02]],
    [{"@S": "hardsphere",
      "radius": 120., "radius_pd": 0.2, "radius_pd_n": 45,
      "volfraction": 0.2,
      #"radius_effective": 120.0,   # hard sphere structure factor
      "structure_factor_mode": 0,  # normal decoupling approximation
      "radius_effective_mode": 1   # this uses 120 from the form factor
     }, [0.01, 0.1, 0.2], [1.10112335e+03, 7.41366536e-01, 4.66630207e-02]],
    [{"@S": "hardsphere",
      "radius": 120., "radius_pd": 0.2, "radius_pd_n": 45,
      "volfraction": 0.2,
      #"radius_effective": 50.0,    # hard sphere structure factor
      "structure_factor_mode": 0,  # normal decoupling approximation
      "radius_effective_mode": 0   # this used 50 the default for hardsphere
     }, [0.01, 0.1, 0.2], [7.82803598e+02, 6.85943611e-01, 4.71586457e-02]],


    # Check returned intermediate results.
    # Note: Target values come from double precision dll calculation.
    # TODO: Cross check results against other software.
    [{"@S": "hardsphere",
      "radius": 120., "radius_pd": 0.2, "radius_pd_n": 45,
      "volfraction": 0.2,
      "radius_effective": 120.0,   # hard sphere structure factor
      "structure_factor_mode": 1,  # normal decoupling approximation
      "radius_effective_mode": 0,  # mode 0 says ignore Reff from P
     }, [0.01, 0.1, 0.2], [1.57928589e+03, 7.37067923e-01, 4.67686197e-02],
     {"P(Q)": [3487.905895219423, 0.7360339734027279, 0.04576861975646704],
      "S(Q)": [0.31569726516764035, 1.005886362143737, 0.9976927625183415],
      #"beta(Q)": [0.7996623765645325, 0.007835960247334845, 8.218250904154009e-07],
      "beta(Q)": None,  # Single precision not good enough for 5 digits of beta
      "S_eff(Q)": [0.4527888487743462, 1.0000461252997597, 0.9999999981038543],
      "volume": 8087664.122641933,
      "volume_ratio": 1.0,
      "radius_effective": 0.0,  # zero since mode is 0, and Reff isn't computed
     }],
    ]
