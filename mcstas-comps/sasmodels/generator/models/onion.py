r"""
This model provides the form factor, $P(q)$, for a multi-shell sphere where
the scattering length density (SLD) of each shell is described by an
exponential, linear, or constant function. The form factor is normalized by
the volume of the sphere where the SLD is not identical to the SLD of the
solvent. We currently provide up to 9 shells with this model.

.. note::

    *radius* represents the core radius $r_0$ and *thickness[k]* represents
    the thickness of the shell, $r_{k+1} - r_k$.

Definition
----------

The 1D scattering intensity is calculated in the following way

.. math::

    P(q) = [f]^2 / V_\text{particle}

where

.. math::
    :nowrap:

    \begin{align*}
    f &= f_\text{core}
            + \left(\sum_{\text{shell}=1}^N f_\text{shell}\right)
            + f_\text{solvent}
    \end{align*}

The shells are spherically symmetric with particle density $\rho(r)$ and
constant SLD within the core and solvent, so

.. math::
    :nowrap:

    \begin{align*}
    f_\text{core}
        &= 4\pi\int_0^{r_\text{core}} \rho_\text{core}
            \frac{\sin(qr)}{qr}\, r^2\,\mathrm{d}r
        &= 3\rho_\text{core} V(r_\text{core})
            \frac{j_1(qr_\text{core})}{qr_\text{core}} \\
    f_\text{shell}
        &= 4\pi\int_{r_{\text{shell}-1}}^{r_\text{shell}}
            \rho_\text{shell}(r)\frac{\sin(qr)}{qr}\,r^2\,\mathrm{d}r \\
    f_\text{solvent}
        &= 4\pi\int_{r_N}^\infty
            \rho_\text{solvent}\frac{\sin(qr)}{qr}\,r^2\,\mathrm{d}r
        &= -3\rho_\text{solvent}V(r_N)\frac{j_1(q r_N)}{q r_N}
    \end{align*}

where the spherical Bessel function $j_1$ is

.. math::

    j_1(x) = \frac{\sin(x)}{x^2} - \frac{\cos(x)}{x}

and the volume is $V(r) = \frac{4\pi}{3}r^3$.

The volume of the particle is determined by the radius of the outer
shell, so $V_\text{particle} = V(r_N)$.

Now consider the SLD of a shell defined by

.. math::

    \rho_\text{shell}(r) = \begin{cases}
        B\exp\left(A(r-r_{\text{shell}-1})/\Delta t_\text{shell}\right)
            + C & \mbox{for } A \neq 0 \\
        \rho_\text{in} = \text{constant} & \mbox{for } A = 0
    \end{cases}

An example of a possible SLD profile is shown below where
$\rho_\text{in}$ and $\Delta t_\text{shell}$ stand for the
SLD of the inner side of the $k^\text{th}$ shell and the
thickness of the $k^\text{th}$ shell in the equation above, respectively.

.. figure:: img/onion_geometry.png

    Example of an onion model profile.


**Exponential SLD profiles** ($A > 0$ or $A < 0$):

.. math::

    f_\text{shell} &= 4 \pi \int_{r_{\text{shell}-1}}^{r_\text{shell}}
        \left[ B\exp
            \left(A (r - r_{\text{shell}-1}) / \Delta t_\text{shell} \right) + C
        \right] \frac{\sin(qr)}{qr}\,r^2\,\mathrm{d}r \\
    &= 3BV(r_\text{shell}) e^A h(\alpha_\text{out},\beta_\text{out})
        - 3BV(r_{\text{shell}-1}) h(\alpha_\text{in},\beta_\text{in})
        + 3CV(r_{\text{shell}}) \frac{j_1(\beta_\text{out})}{\beta_\text{out}}
        - 3CV(r_{\text{shell}-1}) \frac{j_1(\beta_\text{in})}{\beta_\text{in}}

where

.. math::
    :nowrap:

    \begin{align*}
    B&=\frac{\rho_\text{out} - \rho_\text{in}}{e^A-1}
         & C &= \frac{\rho_\text{in}e^A - \rho_\text{out}}{e^A-1} \\
    \alpha_\text{in} &= A\frac{r_{\text{shell}-1}}{\Delta t_\text{shell}}
         & \alpha_\text{out} &= A\frac{r_\text{shell}}{\Delta t_\text{shell}} \\
    \beta_\text{in} &= qr_{\text{shell}-1}
        & \beta_\text{out} &= qr_\text{shell}
    \end{align*}

and

 .. math::

     h(x,y) = \frac{x \sin(y) - y\cos(y)}{(x^2+y^2)y}
               - \frac{(x^2-y^2)\sin(y) - 2xy\cos(y)}{(x^2+y^2)^2y}



**Linear SLD profile** ($A \sim 0$):

For small $A$, say, $A = -0.0001$, the function converges to that of of a linear
SLD profile with

     $\rho_\text{shell}(r) \approx A(r-r_{\text{shell}-1})/\Delta t_\text{shell})+B$,

which is equivalent to

.. math::
    :nowrap:

    \begin{align*}
    f_\text{shell}
    &=
      3 V(r_\text{shell}) \frac{\Delta\rho_\text{shell}}{\Delta t_\text{shell}}
        \left[\frac{
                2 \cos(qr_\text{out})
                    + qr_\text{out} \sin(qr_\text{out})
            }{
                (qr_\text{out})^4
            }\right] \\
     &{}
      -3 V(r_\text{shell}) \frac{\Delta\rho_\text{shell}}{\Delta t_\text{shell}}
        \left[\frac{
                    2\cos(qr_\text{in})
                +qr_\text{in}\sin(qr_\text{in})
            }{
                (qr_\text{in})^4
            }\right] \\
    &{}
      +3\rho_\text{out}V(r_\text{shell}) \frac{j_1(qr_\text{out})}{qr_\text{out}}
      -3\rho_\text{in}V(r_{\text{shell}-1}) \frac{j_1(qr_\text{in})}{qr_\text{in}}
    \end{align*}


**Constant SLD** ($A = 0$):

When $A = 0$ the exponential function has no dependence on the radius (meaning
$\rho_\text{out}$ is ignored in this case) and becomes flat. We set the constant
to $\rho_\text{in}$ for convenience, and thus the form factor contributed by
the shells is

.. math::

    f_\text{shell} =
        3\rho_\text{in}V(r_\text{shell})
           \frac{j_1(qr_\text{out})}{qr_\text{out}}
        - 3\rho_\text{in}V(r_{\text{shell}-1})
            \frac{j_1(qr_\text{in})}{qr_\text{in}}

The 2D scattering intensity is the same as $P(q)$ above, regardless of the
orientation of the $q$ vector which is defined as

.. math::

    q = \sqrt{q_x^2 + q_y^2}

NB: The outer most radius is used as the effective radius for $S(q)$
when $P(q) S(q)$ is applied.

References
----------

#. L A Feigin and D I Svergun, *Structure Analysis by Small-Angle X-Ray and
   Neutron Scattering*, Plenum Press, New York, 1987.

Authorship and Verification
----------------------------

* **Author:**
* **Last Modified by:**
* **Last Reviewed by:** Steve King **Date:** March 28, 2019
"""

#
# Give a polynomial $\rho(r) = Ar^3 + Br^2 + Cr + D$ for density,
#
# .. math::
#
#    f = 4 \pi \int_a^b \rho(r) \sin(qr)/(qr) \mathrm{d}r  = h(b) - h(a)
#
# where
#
# .. math::
#
#    h(r) = \frac{4 \pi}{q^6}\left[
#        (q^3(4Ar^3 + 3Br^2 + 2Cr + D) - q(24Ar + 6B)) \sin(qr)
#      - (q^4(Ar^4 + Br^3 + Cr^2 + Dr) - q^2(12Ar^2 + 6Br + 2C) + 24A) \cos(qr)
#    \right]
#
# Use the monotonic spline to get the polynomial coefficients for each shell.
#
# Order 0
#
# .. math::
#
#    h(r) = \frac{4 \pi}{q^3} \left[
#       - \cos(qr) (Ar) q
#       + \sin(qr) (A)
#    \right]
#
# Order 1
#
# .. math::
#
#   h(r) = \frac{4 \pi}{q^4} \left[
#       - \cos(qr) ( Ar^2 + Br) q^2
#       + \sin(qr) ( Ar   + B ) q
#       + \cos(qr) (2A        )
#   \right]
#
# Order 2
#
# .. math::
#  h(r) = \frac{4 \pi}{q^5} \left[
#        - \cos(qr) ( Ar^3 +  Br^2 + Cr) q^3
#        + \sin(qr) (3Ar^2 + 2Br   + C ) q^2
#        + \cos(qr) (6Ar   + 2B        ) q
#        - \sin(qr) (6A                )
#
# Order 3
#
#    h(r) = \frac{4 \pi}{q^6}\left[
#      - \cos(qr) (  Ar^4 +  Br^3 +  Cr^2 + Dr) q^4
#      + \sin(qr) ( 4Ar^3 + 3Br^2 + 2Cr   + D ) q^3
#      + \cos(qr) (12Ar^2 + 6Br   + 2C        ) q^2
#      - \sin(qr) (24Ar   + 6B                ) q
#      - \cos(qr) (24A                        )
#    \right]
#
# Order p
#
#    h(r) = \frac{4 \pi}{q^{2}}
#      \sum_{k=0}^p -\frac{d^k\cos(qr)}{dr^k} \frac{d^k r\rho(r)}{dr^k} (qr)^{-k}
#
# Given the equation
#
#    f = sum_(k=0)^(n-1) h_k(r_(k+1)) - h_k(r_k)
#
# we can rearrange the terms so that
#
#    f = sum_0^(n-1) h_k(r_(k+1)) - sum_0^(n-1) h_k(r_k)
#      = sum_1^n h_(k-1)(r_k) - sum_0^(n-1) h_k(r_k)
#      = h_(n-1)(r_n) - h_0(r_0) + sum_1^(n-1) [h_(k-1)(r_k) - h_k(r_k)]
#      = h_(n-1)(r_n) - h_0(r_0) - sum_1^(n-1) h_(Delta k)(r_k)
#
# where
#
#    h_(Delta k)(r) = h(Delta rho_k, r)
#
# for
#
#    Delta rho_k = (A_k-A_(k-1)) r^p + (B_k-B_(k-1)) r^(p-1) + ...
#
# Using l'H\^opital's Rule 6 times on the order 3 polynomial,
#
#   lim_(q->0) h(r) = (140D r^3 + 180C r^4 + 144B r^5 + 120A r^6)/720
#

from __future__ import division

from math import fabs, exp, expm1

import numpy as np
from numpy import inf, nan

name = "onion"
title = "Onion shell model with constant, linear or exponential density"

description = """\
Form factor of multishells normalized by the volume. Here each shell is
described by an exponential function;

	I) For A_shell != 0,
		f(r) = B*exp(A_shell*(r-r_in)/thick_shell)+C
	where
		B=(sld_out-sld_in)/(exp(A_shell)-1)
		C=sld_in-B.
	Note that in the above case, the function becomes a linear function
	as A_shell --> 0+ or 0-.

	II) For the exact point of A_shell == 0,
		f(r) = sld_in ,i.e., it crosses over flat function
	Note that the 'sld_out' becomes NULL in this case.

	background:background,
	rad_core0: radius of sphere(core)
	thick_shell#:the thickness of the shell#
	sld_core0: the SLD of the sphere
	sld_solv: the SLD of the solvent
	sld_shell: the SLD of the shell#
	A_shell#: the coefficient in the exponential function
"""

category = "shape:sphere"

# TODO: n is a volume parameter that is not polydisperse

# NOTE: Joachim Wuttke has suggested an alternative parameterisation
#       in Ticket #1107

# pylint: disable=bad-whitespace, line-too-long
#   ["name", "units", default, [lower, upper], "type","description"],
parameters = [
    ["sld_core", "1e-6/Ang^2", 1.0, [-inf, inf], "sld", "Core scattering length density"],
    ["radius_core", "Ang", 200., [0, inf], "volume", "Radius of the core"],
    ["sld_solvent", "1e-6/Ang^2", 6.4, [-inf, inf], "sld", "Solvent scattering length density"],
    ["n_shells", "", 1, [0, 10], "volume", "number of shells (must be integer)"],
    ["sld_in[n_shells]", "1e-6/Ang^2", 1.7, [-inf, inf], "sld", "scattering length density at the inner radius of shell k"],
    ["sld_out[n_shells]", "1e-6/Ang^2", 2.0, [-inf, inf], "sld", "scattering length density at the outer radius of shell k"],
    ["thickness[n_shells]", "Ang", 40., [0, inf], "volume", "Thickness of shell k"],
    ["A[n_shells]", "", 1.0, [-inf, inf], "", "Decay rate of shell k"],
    ]
# pylint: enable=bad-whitespace, line-too-long

source = ["lib/sas_3j1x_x.c", "onion.c"]
single = False
have_Fq = True
radius_effective_modes = ["outer radius"]

profile_axes = ['Radius (A)', 'SLD (1e-6/A^2)']
def profile(sld_core, radius_core, sld_solvent, n_shells,
            sld_in, sld_out, thickness, A):
    """
    Returns shape profile with x=radius, y=SLD.
    """
    n_shells = int(n_shells+0.5)
    total_radius = 1.25*(sum(thickness[:n_shells]) + radius_core + 1)
    dz = total_radius/400  # 400 points for a smooth plot

    z = []
    rho = []

    # add in the core
    z.append(0)
    rho.append(sld_core)
    z.append(radius_core)
    rho.append(sld_core)

    # add in the shells
    for k in range(int(n_shells)):
        # Left side of each shells
        z_current = z[-1]
        z.append(z_current)
        rho.append(sld_in[k])

        if fabs(A[k]) < 1.0e-16:
            # flat shell
            z.append(z_current + thickness[k])
            rho.append(sld_in[k])
        else:
            # exponential shell
            # num_steps in the profile must be at least 1, so use 1+truncation
            # to guess the number of bins rather than rounding or ceiling,
            # even when the layer has zero thickness.  Also, num_steps must
            # be an integer rather than a float for the linspace function.
            num_steps = int(thickness[k]/dz) + 1
            slope = (sld_out[k] - sld_in[k]) / expm1(A[k])
            const = (sld_in[k] - slope)
            for z_shell in np.linspace(0, thickness[k], num_steps+1):
                z.append(z_current+z_shell)
                rho.append(slope*exp(A[k]*z_shell/thickness[k]) + const)

    # add in the solvent
    z.append(z[-1])
    rho.append(sld_solvent)
    z.append(total_radius)
    rho.append(sld_solvent)

    return np.asarray(z), np.asarray(rho)

# TODO: no random parameter function for onion model

# One of the few cases where demo values are useful because the default
# model is really boring. Leave these here for now even though they are
# never used.
demo = {
    "sld_solvent": 2.2,
    "sld_core": 1.0,
    "radius_core": 100,
    "n_shells": 4,
    "sld_in": [0.5, 1.5, 0.9, 2.0],
    "sld_out": [nan, 0.9, 1.2, 1.6],
    "thickness": [50, 75, 150, 75],
    "A": [0, -1, 1e-4, 1],
    # Could also specify them individually as
    # "A1": 0, "A2": -1, "A3": 1e-4, "A4": 1,
    #"radius_core_pd_n": 10,
    #"radius_core_pd": 0.4,
    #"thickness4_pd_n": 10,
    #"thickness4_pd": 0.4,
    }
