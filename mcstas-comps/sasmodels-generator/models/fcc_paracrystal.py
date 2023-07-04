#fcc paracrystal model
#note model title and parameter table are automatically inserted
#note - calculation requires double precision
r"""
Definition
----------

Calculates the scattering from a **face-centered cubic lattice** with
paracrystalline distortion. Thermal vibrations are considered to be
negligible, and the size of the paracrystal is infinitely large.
Paracrystalline distortion is assumed to be isotropic and characterized by
a Gaussian distribution.

The scattering intensity $I(q)$ is calculated as

.. math::

    I(q) = \frac{\text{scale}}{V_p} V_\text{lattice} P(q) Z(q) + \text{background}

where *scale* is the volume fraction of crystal in the sample volume,
$V_\text{lattice}$ is the volume fraction of spheres in the crystal, $V_p$ is
the volume of the primary particle, $P(q)$ is the form factor of the sphere
(normalized), and $Z(q)$ is the paracrystalline structure factor for a
face-centered cubic structure.

.. note::
    At this point the GUI does not return $V_\text{lattice}$ separately so that
    the user will need to calculate it from the equation given and the
    appropriate returned parameters.

.. warning::
    As per the equations below, this model will return I(q)=0 for all q if the
    distortion factor is equal to 0. The model is not meant to support perfect
    crystals.

.. figure:: img/fcc_geometry.jpg

    Face-centered cubic (FCC) lattice taken from reference [#Matsuoka1987]_.

Following the derivation from reference [#Matsuoka1987]_, as corrected in
reference [#Matsuoka1990]_, and based on the above figure, the
primitive unit cell vectors $\vec{a_1},\vec{a_2}$, and $\vec{a_3}$, which
enclose the smallest possible unit cell for the fcc lattice, are defined below:

.. math::
    \vec{a_1} &= \frac{1}{2} (\vec{b_2} + \vec{b_3}) \\
    \vec{a_2} &= \frac{1}{2} (\vec{b_1} + \vec{b_3}) \\
    \vec{a_3} &= \frac{1}{2} (\vec{b_1} + \vec{b_2}).


Where the $\vec{b_1},\vec{b_2}$, and $\vec{b_3}$ are the conventional unit cell
vectors. As defined by reference [#Matsuoka1987]_, the constant $a$ is the
lattice parameter of the conventional unit cell with
$|\vec{b_1}|=|\vec{b_2}|=|\vec{b_3}|=a$. Using this definition, the
nearest-neighbor distance ($D$) is given by
$D=|\vec{a_1}|=|\vec{a_2}|=|\vec{a_3}|=\sqrt{(a/2)^2+(a/2)^2}=\frac{a}{\sqrt2}$.

The volume of the primitive unit cell $V_u$ is given by:

.. math::
    V_u &= |(\vec{a_1}\times \vec{a_2})\cdot\vec{a_3}| \\
    &= (\frac{a^2}{4},\frac{a^2}{4},-\frac{a^2}{4})\cdot(\frac{a}{2},\frac{a}{2},0)\\
    &=a^3/4

In this case, the volume fraction ($V_{lattice}$) of spherical particles with
radius $R$ sitting on the fcc lattice is given by:

.. math::
    V_{lattice} &= \frac{4/3 \pi R^3}{a^3/4}\\
    &= \frac{16\pi R^3}{3a^3}\\
    &= \frac{8 \pi R^3}{3\sqrt{2}D^3}.

Now, continuing to follow [#Matsuoka1987]_, the structure (lattice)
factor $Z(\vec{q})$ for a 3D paracrystal can be written as:

.. math::
    Z(\vec{q}) = \prod_{k=1}^{3}Z_k(\vec{q})

with

.. math::
    Z_k(\vec{q}) = \frac{1-|F_k|^2}{1-2|F_k|\cos(\vec{a_k}\cdot\vec{q})+|F_k|^2}

and where $F_k(\vec{q})$ is the structure factor of the primitive unit cell
defined as:

.. math::
    F_k(\vec{q}) = e^{-\frac{1}{2} \Delta a^2_k q^2} \times e^{-i\vec{q}\cdot\vec{a_k}}.

Here, $\vec{a_k}$ are the primitive unit cell vectors $\vec{a_1}$, $\vec{a_2}$,
and $\vec{a_3}$. Furthermore, $\Delta a_k$ is the isotropic distortion of the
lattice point from its ideal position and can be defined by a constant factor
$g=\Delta a / |\vec{a_1}| = \Delta a / |\vec{a_2}| = \Delta a / |\vec{a_3}|=\Delta a/D$.

Finally, assuming the definitions presented in this document, the authors of
reference [#Matsuoka1987]_ have derived the lattice factors which are given by:

.. math::
    Z_1(q,\theta,\phi)&=[1-e^{-q^2\Delta a^2}]/\{1-2e^{-\frac{1}{2}q^2\Delta a^2}\cos[\frac{qa}{2}(\sin\theta \cos\phi + \sin\theta \sin\phi + \cos\theta)] + e^{-q^2\Delta a^2}\}\\
    Z_2(q,\theta,\phi)&=[1-e^{-q^2\Delta a^2}]/\{1-2e^{-\frac{1}{2}q^2\Delta a^2}\cos[\frac{qa}{2}(-\sin\theta \cos\phi - \sin\theta \sin\phi + \cos\theta)] + e^{-q^2\Delta a^2}\}\\
    Z_3(q,\theta,\phi)&=[1-e^{-q^2\Delta a^2}]/\{1-2e^{-\frac{1}{2}q^2\Delta a^2}\cos[\frac{qa}{2}(-\sin\theta \cos\phi + \sin\theta \sin\phi - \cos\theta)] + e^{-q^2\Delta a^2}\}\\

Note that Sasview is using the nearest-neighbor parameter ($D$) as an input
instead of the conventional unit cell parameter $a$. In this case, Using
$a=\sqrt{2}D$, we rewrite $Z_1(q)$, $Z_2(q)$, and $Z_3(q)$ in terms
of $D$ instead of $a$, which leads to:

.. math::
    Z_1(q,\theta,\phi)&=[1-e^{-q^2\Delta a^2}]/\{1-2e^{-\frac{1}{2}q^2\Delta a^2}\cos[\frac{qD}{\sqrt{2}}(\sin\theta \cos\phi + \sin\theta \sin\phi + \cos\theta)] + e^{-q^2\Delta a^2}\}\\
    Z_2(q,\theta,\phi)&=[1-e^{-q^2\Delta a^2}]/\{1-2e^{-\frac{1}{2}q^2\Delta a^2}\cos[\frac{qD}{\sqrt{2}}(-\sin\theta \cos\phi - \sin\theta \sin\phi + \cos\theta)] + e^{-q^2\Delta a^2}\}\\
    Z_3(q,\theta,\phi)&=[1-e^{-q^2\Delta a^2}]/\{1-2e^{-\frac{1}{2}q^2\Delta a^2}\cos[\frac{qD}{\sqrt{2}}(-\sin\theta \cos\phi + \sin\theta \sin\phi - \cos\theta)] + e^{-q^2\Delta a^2}\}\\

Finally, the position of the Bragg peaks for the fcc lattice are indexed by (reduced q-values):

.. math::
    \frac{qa}{2\pi}=\frac{qD}{\sqrt{2}\pi}=\sqrt{h^2+k^2+l^2}.

where the scattering condition imposes that h,k, and l are all odd or all even.
Thus the peak positions correspond to (just the first 5)

.. math::

    \begin{array}{cccccc}
    q/q_0 & 1 & \sqrt{4/3} & \sqrt{8/3} & \sqrt{11/3} & \sqrt{4} \\
    \text{Indices} & (111)  & (200) & (220) & (311) & (222)
    \end{array}

.. note::

  The calculation of $Z(q)$ is a double numerical integral that must be
  carried out with a high density of points to properly capture the sharp
  peaks of the paracrystalline scattering. So be warned that the calculation
  is slow. Fitting of any experimental data must be resolution smeared for
  any meaningful fit. This makes a triple integral which may be very slow.
  If a double-precision GPU with OpenCL support is available this may improve
  the speed of the calculation.

The 2D (Anisotropic model) is based on the reference below where $I(q)$ is
approximated for 1d scattering. Thus the scattering pattern for 2D may not
be accurate particularly at low $q$. For general details of the calculation
and angular dispersions for oriented particles see :ref:`orientation`.
Note that we are not responsible for any incorrectness of the
2D model computation.

.. figure:: img/parallelepiped_angle_definition.png

    Orientation of the crystal with respect to the scattering plane, when
    $\theta = \phi = 0$ the $c$ axis is along the beam direction (the $z$ axis).

References
----------

.. [#Matsuoka1987] Hideki Matsuoka et. al. *Physical Review B*, 36 (1987)
   1754-1765 (Original Paper)
.. [#Matsuoka1990] Hideki Matsuoka et. al. *Physical Review B*, 41 (1990)
   3854-3856 (Corrections to FCC and BCC lattice structure calculation)

Authorship and Verification
---------------------------

* **Author:** NIST IGOR/DANSE **Date:** pre 2010
* **Last Modified by:** Jonathan Gaudet **Date:** September 26, 2022
* **Last Reviewed by:** Paul Butler **Date:** November 2, 2022
"""

import numpy as np
from numpy import inf, pi

name = "fcc_paracrystal"
title = "Face-centred cubic lattic with paracrystalline distortion"
description = """
    Calculates the scattering from a **face-centered cubic lattice** with
    paracrystalline distortion. Thermal vibrations are considered to be
    negligible, and the size of the paracrystal is infinitely large.
    Paracrystalline distortion is assumed to be isotropic and characterized
    by a Gaussian distribution.
    """
category = "shape:paracrystal"

single = False

# pylint: disable=bad-whitespace, line-too-long
#             ["name", "units", default, [lower, upper], "type","description"],
parameters = [["dnn", "Ang", 220, [-inf, inf], "", "Nearest neighbour distance"],
              ["d_factor", "", 0.06, [-inf, inf], "", "Paracrystal distortion factor"],
              ["radius", "Ang", 40, [0, inf], "volume", "Particle radius"],
              ["sld", "1e-6/Ang^2", 4, [-inf, inf], "sld", "Particle scattering length density"],
              ["sld_solvent", "1e-6/Ang^2", 1, [-inf, inf], "sld", "Solvent scattering length density"],
              ["theta",       "degrees",    60,    [-360, 360], "orientation", "c axis to beam angle"],
              ["phi",         "degrees",    60,    [-360, 360], "orientation", "rotation about beam"],
              ["psi",         "degrees",    60,    [-360, 360], "orientation", "rotation about c axis"]
             ]
# pylint: enable=bad-whitespace, line-too-long

source = ["lib/sas_3j1x_x.c", "lib/gauss150.c", "lib/sphere_form.c", "fcc_paracrystal.c"]

def random():
    """Return a random parameter set for the model."""
    # copied from bcc_paracrystal
    radius = 10**np.random.uniform(1.3, 4)
    d_factor = 10**np.random.uniform(-2, -0.7)  # sigma_d in 0.01-0.7
    dnn_fraction = np.random.beta(a=10, b=1)
    dnn = radius*4/np.sqrt(2)/dnn_fraction
    pars = dict(
        #sld=1, sld_solvent=0, scale=1, background=1e-32,
        dnn=dnn,
        d_factor=d_factor,
        radius=radius,
    )
    return pars

# april 10 2017, rkh add unit tests, NOT compared with any other calc method, assume correct!
#
# October 26, 2022, PDB fixed unit tests to conform to new maths
# TODO: fix the 2d tests
q = 4.*pi/220.
tests = [
    [{}, [0.001, q, 0.25], [1.9839734995338474, 0.3352457353010224, 0.005804136688760973]],
    #[{}, (-0.047, -0.007), 238.103096286],
    #[{}, (0.053, 0.063), 0.863609587796],
]
