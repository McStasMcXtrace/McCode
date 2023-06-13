r"""
Definition
----------

This model is a trivial extension of the CoreShell function to a larger number
of shells. The scattering length density profile for the default sld values
(w/ 4 shells).

.. figure:: img/core_multi_shell_sld_default_profile.jpg

    SLD profile of the core_multi_shell object from the center of sphere out
    for the default SLDs.*

The 2D scattering intensity is the same as $P(q)$ above, regardless of the
orientation of the $\vec q$ vector which is defined as

.. math::

    q = \sqrt{q_x^2 + q_y^2}

.. note:: **Be careful!** The SLDs and scale can be highly correlated. Hold as
         many of these parameters fixed as possible.

.. note:: The outer most radius (= *radius* + *thickness*) is used as the
          effective radius for $S(Q)$ when $P(Q)*S(Q)$ is applied.

For information about polarised and magnetic scattering, see
the :ref:`magnetism` documentation.

Our model uses the form factor calculations implemented in a C-library provided
by the NIST Center for Neutron Research [#Kline2006]_.

References
----------

Also see the :ref:`core-shell-sphere` model documentation and [#Feigin1987]_

.. [#Kline2006] S R Kline, *J Appl. Cryst.*, 39 (2006) 895

.. [#Feigin1987] L A Feigin and D I Svergun, *Structure Analysis by
   Small-Angle X-Ray and Neutron Scattering*, Plenum Press, New York, 1987.

Authorship and Verification
----------------------------

* **Author:** NIST IGOR/DANSE **Date:** pre 2010
* **Last Modified by:** Paul Kienzle **Date:** September 12, 2016
* **Last Reviewed by:** Paul Kienzle **Date:** September 12, 2016
"""
from __future__ import division

import numpy as np
from numpy import inf

name = "core_multi_shell"
title = "This model provides the scattering from a spherical core with 1 to 10 \
 concentric shell structures. The SLDs of the core and each shell are \
 individually specified."

description = """\
Form factor for a core muti-shell sphere normalized by the volume.
Each of up to 10 shells can have a unique thickness and sld.

	background:background,
	rad_core0: radius of sphere(core)
	thick_shell#:the thickness of the shell#
	sld_core0: the SLD of the sphere
	sld_solv: the SLD of the solvent
	sld_shell: the SLD of the shell#
	A_shell#: the coefficient in the exponential function


    scale: 1.0 if data is on absolute scale
    volfraction: volume fraction of spheres
    radius: the radius of the core
    sld: the SLD of the core
    thick_shelli: the thickness of the i'th shell from the core
    sld_shelli: the SLD of the i'th shell from the core
    sld_solvent: the SLD of the solvent
    background: incoherent background

"""

category = "shape:sphere"


#             ["name", "units", default, [lower, upper], "type","description"],
parameters = [["sld_core", "1e-6/Ang^2", 1.0, [-inf, inf], "sld",
               "Core scattering length density"],
              ["radius", "Ang", 200., [0, inf], "volume",
               "Radius of the core"],
              ["sld_solvent", "1e-6/Ang^2", 6.4, [-inf, inf], "sld",
               "Solvent scattering length density"],
              ["n", "", 1, [0, 10], "volume",
               "number of shells"],
              ["sld[n]", "1e-6/Ang^2", 1.7, [-inf, inf], "sld",
               "scattering length density of shell k"],
              ["thickness[n]", "Ang", 40., [0, inf], "volume",
               "Thickness of shell k"],
             ]

source = ["lib/sas_3j1x_x.c", "core_multi_shell.c"]
have_Fq = True
radius_effective_modes = ["outer radius", "core radius"]

def random():
    """Return a random parameter set for the model."""
    num_shells = np.minimum(np.random.poisson(3)+1, 10)
    total_radius = 10**np.random.uniform(1.7, 4)
    thickness = np.random.exponential(size=num_shells+1)
    thickness *= total_radius/np.sum(thickness)
    pars = dict(
        #background=0,
        n=num_shells,
        radius=thickness[0],
    )
    for k, v in enumerate(thickness[1:]):
        pars['thickness%d'%(k+1)] = v
    return pars

def profile(sld_core, radius, sld_solvent, n, sld, thickness):
    """
    Returns the SLD profile *r* (Ang), and *rho* (1e-6/Ang^2).
    """
    n = int(n+0.5)
    z = []
    rho = []

    # add in the core
    z.append(0)
    rho.append(sld_core)
    z.append(radius)
    rho.append(sld_core)

    # add in the shells
    for k in range(int(n)):
        # Left side of each shells
        z.append(z[-1])
        rho.append(sld[k])
        z.append(z[-1] + thickness[k])
        rho.append(sld[k])
    # add in the solvent
    z.append(z[-1])
    rho.append(sld_solvent)
    z.append(z[-1]*1.25)
    rho.append(sld_solvent)

    return np.asarray(z), np.asarray(rho)
