r"""
Definition
----------

This model provides the form factor, $P(q)$, for stacked discs (tactoids)
with a core/layer structure which is constructed itself as $P(q) S(Q)$
multiplying a $P(q)$ for individual core/layer disks by a structure factor
$S(q)$ proposed by Kratky and Porod in 1949\ [#Kratky1949]_ assuming the next
neighbor distance (d-spacing) in the stack of parallel discs obeys a Gaussian
distribution. As such the normalization of this "composite" form factor is
relative to the individual disk volume, not the volume of the stack of disks.
This model is appropriate for example for non non exfoliated clay particles
such as Laponite.

.. figure:: img/stacked_disks_geometry.png

   Geometry of a single core/layer disk

The scattered intensity $I(q)$ is calculated as

.. math::

    I(q) = N\int_{0}^{\pi /2}\left[ \Delta \rho_t
    \left( V_t f_t(q,\alpha) - V_c f_c(q,\alpha)\right) + \Delta
    \rho_c V_c f_c(q,\alpha)\right]^2 S(q,\alpha)\sin{\alpha}\ d\alpha
    + \text{background}

where the contrast

.. math::

    \Delta \rho_i = \rho_i - \rho_\text{solvent}

and $N$ is the number of individual (single) discs per unit volume, $\alpha$
is the angle between the axis of the disc and $q$, and $V_t$ and $V_c$ are the
total volume and the core volume of a single disc, respectively, and

.. math::

    f_t(q,\alpha) =
    \left(\frac{\sin(q(d+h)\cos{\alpha})}{q(d+h)\cos{\alpha}}\right)
    \left(\frac{2J_1(qR\sin{\alpha})}{qR\sin{\alpha}} \right)

    f_c(q,\alpha) =
    \left(\frac{\sin(qh)\cos{\alpha})}{qh\cos{\alpha}}\right)
    \left(\frac{2J_1(qR\sin{\alpha})}{qR\sin{\alpha}}\right)

where $d$ = thickness of the layer (*thick_layer*),
$2h$ = core thickness (*thick_core*), and $R$ = radius of the disc (*radius*).

.. math::

    S(q,\alpha) = 1 + \frac{1}{2}\sum_{k=1}^n(n-k)\cos{(kDq\cos{\alpha})}
    \exp\left[ -k(q)^2(D\cos{\alpha}~\sigma_d)^2/2\right]

where $n$ is the total number of the disc stacked (*n_stacking*),
$D = 2(d+h)$ is the next neighbor center-to-center distance (d-spacing),
and $\sigma_d$ = the Gaussian standard deviation of the d-spacing (*sigma_d*).
Note that $D\cos(\alpha)$ is the component of $D$ parallel to $q$ and the last
term in the equation above is effectively a Debye-Waller factor term.

.. note::

    1. Each assembly in the stack is layer/core/layer, so the spacing of the
    cores is core plus two layers. The 2nd virial coefficient of the cylinder
    is calculated based on the *radius* and *length*
    = *n_stacking* * (*thick_core* + 2 * *thick_layer*)
    values, and used as the effective radius for $S(Q)$ when $P(Q) * S(Q)$
    is applied.

    2. the resolution smearing calculation uses 76 Gaussian quadrature points
    to properly smear the model since the function is HIGHLY oscillatory,
    especially around the q-values that correspond to the repeat distance of
    the layers.

2d scattering from oriented stacks is calculated in the same way as for
cylinders, for further details of the calculation and angular dispersions
see :ref:`orientation`.

.. figure:: img/cylinder_angle_definition.png

    Angles $\theta$ and $\phi$ orient the stack of discs relative
    to the beam line coordinates, where the beam is along the $z$ axis.
    Rotation $\theta$, initially in the $xz$ plane, is carried out first,
    then rotation $\phi$ about the $z$ axis. Orientation distributions are
    described as rotations about two perpendicular axes $\delta_1$ and
    $\delta_2$ in the frame of the cylinder itself, which when
    $\theta = \phi = 0$ are parallel to the $Y$ and $X$ axes.


Our model is derived from the form factor calculations implemented in a
C-library provided by the NIST Center for Neutron Research\ [#Kline2006]_

References
----------

See also Higgins and Benoit [#Higgins1994]_ and Guinier and
Fournet [#Guinier1955]_.

.. [#Kratky1949] O Kratky and G Porod, *J. Colloid Science*, 4, (1949) 35
.. [#Kline2006] S R Kline, *J Appl. Cryst.*, 39 (2006) 895
.. [#Higgins1994] J S Higgins and H C Benoit, *Polymers and Neutron Scattering*,
   Clarendon, Oxford, 1994
.. [#Guinier1955] A Guinier and G Fournet, *Small-Angle Scattering of X-Rays*,
   John Wiley and Sons, New York, 1955

Authorship and Verification
----------------------------

* **Author:** NIST IGOR/DANSE **Date:** pre 2010
* **Last Modified by:** Paul Butler and Paul Kienzle **Date:** November 26, 2016
* **Last Reviewed by:** Paul Butler and Paul Kienzle **Date:** November 26, 2016
"""

import numpy as np
from numpy import inf, sin, cos, pi

name = "stacked_disks"
title = "Form factor for a stacked set of non exfoliated core/shell disks"
description = """\
    One layer of disk consists of a core, a top layer, and a bottom layer.
    radius =  the radius of the disk
    thick_core = thickness of the core
    thick_layer = thickness of a layer
    sld_core = the SLD of the core
    sld_layer = the SLD of the layers
    n_stacking = the number of the disks
    sigma_d =  Gaussian STD of d-spacing
    sld_solvent = the SLD of the solvent
    """
category = "shape:cylinder"

# pylint: disable=bad-whitespace, line-too-long
#   ["name", "units", default, [lower, upper], "type","description"],
parameters = [
    ["thick_core",  "Ang",        10.0, [0, inf],    "volume",      "Thickness of the core disk"],
    ["thick_layer", "Ang",        10.0, [0, inf],    "volume",      "Thickness of layer each side of core"],
    ["radius",      "Ang",        15.0, [0, inf],    "volume",      "Radius of the stacked disk"],
    ["n_stacking",  "",            1.0, [1, inf],    "volume",      "Number of stacked layer/core/layer disks"],
    ["sigma_d",     "Ang",         0,   [0, inf],    "",            "Sigma of nearest neighbor spacing"],
    ["sld_core",    "1e-6/Ang^2",  4,   [-inf, inf], "sld",         "Core scattering length density"],
    ["sld_layer",   "1e-6/Ang^2",  0.0, [-inf, inf], "sld",         "Layer scattering length density"],
    ["sld_solvent", "1e-6/Ang^2",  5.0, [-inf, inf], "sld",         "Solvent scattering length density"],
    ["theta",       "degrees",     0,   [-360, 360], "orientation", "Orientation of the stacked disk axis w/respect incoming beam"],
    ["phi",         "degrees",     0,   [-360, 360], "orientation", "Rotation about beam"],
    ]
# pylint: enable=bad-whitespace, line-too-long

source = ["lib/polevl.c", "lib/sas_J1.c", "lib/gauss76.c", "stacked_disks.c"]

def random():
    """Return a random parameter set for the model."""
    radius = 10**np.random.uniform(1, 4.7)
    total_stack = 10**np.random.uniform(1, 4.7)
    n_stacking = int(10**np.random.uniform(0, np.log10(total_stack)-1) + 0.5)
    d = total_stack/n_stacking
    thick_core = np.random.uniform(0, d-2)  # at least 1 A for each layer
    thick_layer = (d - thick_core)/2
    # Let polydispersity peak around 15%; 95% < 0.4; max=100%
    sigma_d = d * np.random.beta(1.5, 7)
    pars = dict(
        thick_core=thick_core,
        thick_layer=thick_layer,
        radius=radius,
        n_stacking=n_stacking,
        sigma_d=sigma_d,
    )
    return pars

# After redefinition of spherical coordinates -
# tests had in old coords theta=0, phi=0; new coords theta=90, phi=0
q = 0.1
# april 6 2017, rkh added a 2d unit test, assume correct!
qx = q*cos(pi/6.0)
qy = q*sin(pi/6.0)
# Accuracy tests based on content in test/utest_extra_models.py.
# Added 2 tests with n_stacked = 5 using SasView 3.1.2 - PDB;
# for which alas q=0.001 values seem closer to n_stacked=1 not 5,
# changed assuming my 4.1 code OK, RKH
tests = [
    [{'thick_core': 10.0,
      'thick_layer': 15.0,
      'radius': 3000.0,
      'n_stacking': 1.0,
      'sigma_d': 0.0,
      'sld_core': 4.0,
      'sld_layer': -0.4,
      'sld_solvent': 5.0,
      'theta': 90.0,
      'phi': 0.0,
      'scale': 0.01,
      'background': 0.001,
     }, 0.001, 5075.12],
    [{'thick_core': 10.0,
      'thick_layer': 15.0,
      'radius': 3000.0,
      'n_stacking': 5,
      'sigma_d': 0.0,
      'sld_core': 4.0,
      'sld_layer': -0.4,
      'sld_solvent': 5.0,
      'theta': 90.0,
      'phi': 0.0,
      'scale': 0.01,
      'background': 0.001,
      # n_stacking=1 not 5 ? slight change in value here 11jan2017,
      # check other cpu types
      #}, 0.001, 5065.12793824],
      #}, 0.001, 5075.11570493],
     }, 0.001, 25325.635693],
    [{'thick_core': 10.0,
      'thick_layer': 15.0,
      'radius': 100.0,
      'n_stacking': 5,
      'sigma_d': 0.0,
      'sld_core': 4.0,
      'sld_layer': -0.4,
      'sld_solvent': 5.0,
      'theta': 90.0,
      'phi': 20.0,
      'scale': 0.01,
      'background': 0.001,
     }, (qx, qy), 0.0491167089952],
    [{'thick_core': 10.0,
      'thick_layer': 15.0,
      'radius': 3000.0,
      'n_stacking': 5,
      'sigma_d': 0.0,
      'sld_core': 4.0,
      'sld_layer': -0.4,
      'sld_solvent': 5.0,
      'theta': 90.0,
      'phi': 0.0,
      'scale': 0.01,
      'background': 0.001,
      # n_stacking=1 not 5 ?  slight change in value here 11jan2017,
      # check other cpu types
      #}, 0.164, 0.0127673597265],
      #}, 0.164, 0.01480812968],
     }, 0.164, 0.0598367986509],

    [{'thick_core': 10.0,
      'thick_layer': 15.0,
      'radius': 3000.0,
      'n_stacking': 1.0,
      'sigma_d': 0.0,
      'sld_core': 4.0,
      'sld_layer': -0.4,
      'sld_solvent': 5.0,
      'theta': 90.0,
      'phi': 0.0,
      'scale': 0.01,
      'background': 0.001,
      # second test here was at q=90, changed it to q=5,
      # note I(q) is then just value of flat background
     }, [0.001, 5.0], [5075.12, 0.001]],

    [{'thick_core': 10.0,
      'thick_layer': 15.0,
      'radius': 3000.0,
      'n_stacking': 1.0,
      'sigma_d': 0.0,
      'sld_core': 4.0,
      'sld_layer': -0.4,
      'sld_solvent': 5.0,
      'theta': 90.0,
      'phi': 0.0,
      'scale': 0.01,
      'background': 0.001,
     }, ([0.4, 0.5]), [0.00105074, 0.00121761]],
    #[{'thick_core': 10.0,
    #  'thick_layer': 15.0,
    #  'radius': 3000.0,
    #  'n_stacking': 1.0,
    #  'sigma_d': 0.0,
    #  'sld_core': 4.0,
    #  'sld_layer': -0.4,
    #  'sld_solvent': 5.0,
    #  'theta': 90.0,
    #  'phi': 20.0,
    #  'scale': 0.01,
    #  'background': 0.001,
    # 2017-05-18 PAK temporarily suppress output of qx,qy test; j1 is
    #     not accurate for large qr
    # }, (qx, qy), 0.0341738733124],
    # }, (qx, qy), None],

    [{'thick_core': 10.0,
      'thick_layer': 15.0,
      'radius': 3000.0,
      'n_stacking': 1.0,
      'sigma_d': 0.0,
      'sld_core': 4.0,
      'sld_layer': -0.4,
      'sld_solvent': 5.0,
      'theta': 90.0,
      'phi': 0.0,
      'scale': 0.01,
      'background': 0.001,
     }, ([1.3, 1.57]), [0.0010039, 0.0010038]],
    ]
# 11Jan2017   RKH checking unit test again, note they are all 1D, no 2D
