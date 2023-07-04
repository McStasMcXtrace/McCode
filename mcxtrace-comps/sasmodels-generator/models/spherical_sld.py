r"""
Definition
----------

Similarly to the onion, this model provides the form factor, $P(q)$, for
a multi-shell sphere, where the interface between the each neighboring
shells can be described by the error function, power-law, or exponential
functions.  The scattering intensity is computed by building a continuous
custom SLD profile along the radius of the particle. The SLD profile is
composed of a number of uniform shells with interfacial shells between them.

.. figure:: img/spherical_sld_profile.png

    Example SLD profile

Unlike the :ref:`onion` model (using an analytical integration), the interfacial
shells here are sub-divided and numerically integrated assuming each
sub-shell is described by a line function, with *n_steps* sub-shells per
interface. The form factor is normalized by the total volume of the sphere.

.. note::

   *n_shells* must be an integer. *n_steps* must be an ODD integer.

Interface shapes are as follows:

    0: erf($\nu z$)

    1: Rpow($z^\nu$)

    2: Lpow($z^\nu$)

    3: Rexp($-\nu z$)

    4: Lexp($-\nu z$)

    5: Boucher ($(1-z^2)^(\nu/2-2)$)

The form factor $P(q)$ in 1D is calculated by [#Feigin1987]_:

.. math::

    P(q) = \frac{f^2}{V_\text{particle}} \text{ where }
    f = f_\text{core} + \sum_{\text{inter}_i=0}^N f_{\text{inter}_i} +
    \sum_{\text{flat}_i=0}^N f_{\text{flat}_i} +f_\text{solvent}

For a spherically symmetric particle with a particle density $\rho_x(r)$
the sld function can be defined as:

.. math::

    f_x = 4 \pi \int_{0}^{\infty} \rho_x(r)  \frac{\sin(qr)} {qr^2} r^2 dr


so that individual terms can be calculated as follows:

.. math::

    f_\text{core}
        &= 4 \pi \int_{0}^{r_\text{core}} \rho_\text{core}
            \frac{\sin(qr)} {qr} r^2 dr \\
        &= 3 \rho_\text{core} V(r_\text{core})
          \left[ \frac{\sin(qr_\text{core}) - qr_\text{core} \cos(qr_\text{core})}
                {qr_\text{core}^3} \right] \\
    f_{\text{inter}_i}
        &= 4 \pi \int_{\Delta t_{ \text{inter}_i } } \rho_{ \text{inter}_i }
            \frac{\sin(qr)} {qr} r^2 dr \\
    f_{\text{shell}_i}
        &= 4 \pi \int_{\Delta t_{ \text{inter}_i } } \rho_{ \text{flat}_i }
            \frac{\sin(qr)} {qr} r^2 dr \\
        &= 3 \rho_{\text{flat}_i} V (r_{\text{inter}_i}
                                       + \Delta t_{\text{inter}_i})
            \left[
                \frac{\sin(qr_{\text{inter}_i} + \Delta t_{\text{inter}_i})
                    - q (r_{\text{inter}_i} + \Delta t_{ \text{inter}_i })
                    \cos(q(r_{\text{inter}_i} + \Delta t_{\text{inter}_i}))}
                {q ( r_{\text{inter}_i} + \Delta t_{\text{inter}_i} )^3 }
            \right] \\
        &\quad {} - 3 \rho_{ \text{flat}_i } V (r_{\text{inter}_i})
            \left[
                \frac{\sin(qr_{\text{inter}_i})
                    - qr_{\text{flat}_i} \cos(qr_{\text{inter}_i})}
                {qr_{\text{inter}_i}^3}
            \right] \\
    f_\text{solvent}
        &= 4 \pi \int_{r_N}^{\infty} \rho_\text{solvent}
            \frac{\sin(qr)} {qr} r^2 dr \\
        &= 3 \rho_\text{solvent} V(r_N)
            \left[ \frac{\sin(qr_N) - qr_N \cos(qr_N)} {qr_N^3} \right]

Here we assumed that the SLDs of the core and solvent are constant in $r$.
The SLD at the interface between shells, $\rho_{\text {inter}_i}$
is calculated with a function chosen by an user, where the functions are

Exp:

.. math::

    \rho_{{inter}_i}(r) &=
    \begin{cases}
        B\, \exp\left(
            \frac{\pm A(r - r_{\text{flat}_i})}{\Delta t_{\text{inter}_i}}
        \right) + C  & \mbox{for } A \neq 0 \\
        B\, \left(
            \frac{(r - r_{\text{flat}_i})}{\Delta t_{\text{inter}_i}}
        \right) + C  & \mbox{for } A = 0 \\
    \end{cases}

Power-Law:

.. math::

    \rho_{{inter}_i}(r) &=
    \begin{cases}
        \pm B\, \left(
            \frac{(r - r_{\text{flat}_i})}{\Delta t_{ \text{inter}_i }}
            \right) ^A  + C  & \mbox{for } A \neq 0 \\
        \rho_{\text{flat}_{i+1}}  & \mbox{for } A = 0 \\
    \end{cases}

Erf:

.. math::

    \rho_{{inter}_i}(r) =
    \begin{cases}
        B\, \text{erf} \left(
            \frac{A(r - r_{\text{flat}_i})}{\sqrt{2} \Delta t_{\text{inter}_i}}
            \right) + C  & \mbox{for } A \neq 0 \\
        B\, \left(
            \frac{(r - r_{\text{flat}_i})}{\Delta t_{\text{inter}_i}}
            \right)  +C  & \mbox{for } A = 0 \\
    \end{cases}


Boucher[#Boucher1983]_:

.. math::

    \rho_{{inter}_i}(r) =
    \begin{cases}
        \pm B\, \left(1-
            (\frac{(r - r_{\text{flat}_i})}{\Delta t_{ \text{inter}_i }})^2
            \right) ^(A/2-2)  + C  & \mbox{for } A \neq 0 \\
        \rho_{\text{flat}_{i+1}}  & \mbox{for } A = 0 \\
    \end{cases}  

The functions are normalized so that they vary between 0 and 1, and they are
constrained such that the SLD is continuous at the boundaries of the interface
as well as each sub-shell. Thus B and C are determined.

Once $\rho_{\text{inter}_i}$ is found at the boundary of the sub-shell of the
interface, we can find its contribution to the form factor $P(q)$

.. math::

    f_{\text{inter}_i}
        &= 4 \pi \int_{\Delta t_{\text{inter}_i} } \rho_{\text{inter}_i}
            \frac{\sin(qr)}{qr} r^2 dr \\
        &= 4 \pi \sum_{j=1}^{n_\text{steps}}
            \int_{r_j}^{r_{j+1}} \rho_{\text{inter}_i}(r_j)
                \frac{\sin(qr)}{qr} r^2 dr \\
        &\approx 4 \pi \sum_{j=1}^{n_\text{steps}}
        \Biggl[
             3 (\rho_{\text{inter}_i}(r_{j+1}) - \rho_{\text{inter}_i}(r_{j})) V (r_j)
            \left[
                \frac{r_j^2 \beta_\text{out}^2 \sin(\beta_\text{out})
                    - (\beta_\text{out}^2-2) \cos(\beta_\text{out})}
                {\beta_\text{out}^4}
            \right] \\
        &\quad {} - 3 (\rho_{\text{inter}_i}(r_{j+1}) - \rho_{\text{inter}_i}(r_{j})) V(r_{j-1})
            \left[
                \frac{r_{j-1}^2 \sin(\beta_\text{in})
                    - (\beta_\text{in}^2-2) \cos(\beta_\text{in})}
                {\beta_\text{in}^4}
            \right] \\
        &\quad {} + 3 \rho_{\text{inter}_i}(r_{j+1})  V(r_j)
            \left[
                \frac{\sin(\beta_\text{out}) - \cos(\beta_\text{out})}
                {\beta_\text{out}^4}
            \right] \\
        &\quad {} - 3 \rho_{\text{inter}_i}(r_{j})  V(r_j)
            \left[
                \frac{\sin(\beta_\text{in}) - \cos(\beta_\text{in})}
                {\beta_\text{in}^4}
            \right]
        \Biggr]

where

.. math::
    :nowrap:

    \begin{align*}
    V(a) &= \frac {4\pi}{3}a^3
        & {} & {} \\
    a_\text{in} &\sim \frac{r_j}{r_{j+1} -r_j}
        & a_\text{out} &\sim \frac{r_{j+1}}{r_{j+1} -r_j} \\
    \beta_\text{in} &= qr_j
        & \beta_\text{out} &= qr_{j+1}
    \end{align*}

We assume $\rho_{\text{inter}_j} (r)$ is approximately linear
within the sub-shell $j$.

Finally the form factor can be calculated by

.. math::

    P(q) = \frac{[f]^2} {V_\text{particle}} \mbox{ where } V_\text{particle}
    = V(r_{\text{shell}_N})

For 2D data the scattering intensity is calculated in the same way as 1D,
where the $q$ vector is defined as

.. math::

    q = \sqrt{q_x^2 + q_y^2}

.. note::

    The outer most radius is used as the effective radius for $S(Q)$
    when $P(Q) * S(Q)$ is applied.

References
----------

.. [#Feigin1987] L A Feigin and D I Svergun, Structure Analysis by Small-Angle X-Ray
   and Neutron Scattering, Plenum Press, New York, (1987)
.. [#Boucher1983] B Boucher, P Chieux, P Convert, and M Tournarie,
   *Metal Physics*, 13,1339 (1983).
  


Authorship and Verification
---------------------------

* **Author:** Jae-Hie Cho **Date:** Nov 1, 2010
* **Last Modified by:** Paul Kienzle **Date:** Dec 20, 2016
* **Last Reviewed by:** Steve King **Date:** March 29, 2019
"""

import numpy as np
from numpy import inf, expm1, sqrt
from scipy.special import erf

name = "spherical_sld"
title = "Spherical SLD intensity calculation"
description = """
            I(q) =
               background = Incoherent background [1/cm]
        """
category = "shape:sphere"

SHAPES = ["erf(|nu|*z)", "Rpow(z^|nu|)", "Lpow(z^|nu|)",
          "Rexp(-|nu|z)", "Lexp(-|nu|z)", "Boucher((1-z^2)^(1/2*nu-2))",]

# pylint: disable=bad-whitespace, line-too-long
#            ["name", "units", default, [lower, upper], "type", "description"],
parameters = [["n_shells",             "",           1,      [1, 10],        "volume", "number of shells (must be integer)"],
              ["sld_solvent",          "1e-6/Ang^2", 1.0,    [-inf, inf],    "sld", "solvent sld"],
              ["sld[n_shells]",        "1e-6/Ang^2", 4.06,   [-inf, inf],    "sld", "sld of the shell"],
              ["thickness[n_shells]",  "Ang",        100.0,  [0, inf],       "volume", "thickness shell"],
              ["interface[n_shells]",  "Ang",        50.0,   [0, inf],       "volume", "thickness of the interface"],
              ["shape[n_shells]",      "",           0,      [SHAPES],       "", "interface shape"],
              ["nu[n_shells]",         "",           2.5,    [1, inf],       "", "interface shape exponent"],
              ["n_steps",              "",           35,     [0, inf],       "", "number of steps in each interface (must be an odd integer)"],
             ]
# pylint: enable=bad-whitespace, line-too-long
source = ["lib/polevl.c", "lib/sas_erf.c", "lib/sas_3j1x_x.c", "spherical_sld.c"]
single = False  # TODO: fix low q behaviour
have_Fq = True
radius_effective_modes = ["outer radius"]

profile_axes = ['Radius (A)', 'SLD (1e-6/A^2)']

SHAPE_FUNCTIONS = [
    lambda z, nu: erf(nu/sqrt(2)*(2*z-1))/(2*erf(nu/sqrt(2))) + 0.5,  # erf
    lambda z, nu: z**nu,                    # Rpow
    lambda z, nu: 1 - (1-z)**nu,            # Lpow
    lambda z, nu: expm1(-nu*z)/expm1(-nu),  # Rexp
    lambda z, nu: expm1(nu*z)/expm1(nu),    # Lexp
    lambda z, nu: 1 - (1 - z**2)**(0.5*nu-2.0), # Boucher    
]

def profile(n_shells, sld_solvent, sld, thickness,
            interface, shape, nu, n_steps):
    """
    Returns shape profile with x=radius, y=SLD.
    """

    n_shells = int(n_shells + 0.5)
    n_steps = int(n_steps + 0.5)
    z = []
    rho = []
    z_next = 0
    # two sld points for core
    z.append(z_next)
    rho.append(sld[0])

    for i in range(0, n_shells):
        z_next += thickness[i]
        z.append(z_next)
        rho.append(sld[i])
        dz = interface[i]/n_steps
        sld_l = sld[i]
        sld_r = sld[i+1] if i < n_shells-1 else sld_solvent
        fun = SHAPE_FUNCTIONS[int(np.clip(shape[i], 0, len(SHAPE_FUNCTIONS)-1))]
        for step in range(1, n_steps+1):
            portion = fun(float(step)/n_steps, max(abs(nu[i]), 1e-14))
            z_next += dz
            z.append(z_next)
            rho.append((sld_r - sld_l)*portion + sld_l)
    z.append(z_next*1.2)
    rho.append(sld_solvent)
    # return sld profile (r, beta)
    return np.asarray(z), np.asarray(rho)

# TODO: no random parameter generator for spherical SLD.

# Another interesting demo case, again because the default function is boring.
demo = {
    "n_shells": 5,
    "n_steps": 35.0,
    "sld_solvent": 1.0,
    "sld": [2.07, 4.0, 3.5, 4.0, 3.5],
    "thickness": [50.0, 100.0, 100.0, 100.0, 100.0],
    "interface": [50.0]*5,
    "shape": [0]*5,
    "nu": [2.5]*5,
    }

tests = [
    # Results checked against sasview 3.1
    [{"n_shells": 5,
      "n_steps": 35,
      "sld_solvent": 1.0,
      "sld": [2.07, 4.0, 3.5, 4.0, 3.5],
      "thickness": [50.0, 100.0, 100.0, 100.0, 100.0],
      "interface": [50]*5,
      "shape": [0]*5,
      "nu": [2.5]*5,
     }, 0.001, 750697.238],
]
