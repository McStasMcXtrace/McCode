#!/usr/bin/env python
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#                                   Jiao Lin
#                      California Institute of Technology
#                        (C) 2009 All Rights Reserved  
#
# {LicenseText}
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#


'''
This test combines I(Q,E) monitor and a phonon kernel.
This test makes sure that the phonon kernel does not do something strange
like missing some (Q,E) points. You should see an almost evenly-distributed
intensities in the region which is physically allowed by momentum transfer
and energy transfer constraints.
'''


import unittestX as unittest
import journal


componentname = 'IQE_monitor'
category = 'monitors'

class TestCase(unittest.TestCase):

    def test(self):
        "wrap IQE_monitor"
        
        from mcstas2 import componentfactory
        factory = componentfactory( category, componentname )

        Qmin=0; Qmax=13.; nQ=130
        Emin=-50; Emax=50.; nE=100
        
        component = factory(
            'component',
            Ei=Ei,
            Qmin=Qmin, Qmax=Qmax, nQ=nQ,
            Emin=Emin, Emax=Emax, nE=nE,
            max_angle_out_of_plane=30, min_angle_out_of_plane=-30,
            max_angle_in_plane=120, min_angle_in_plane=-30,
            )

        kernel = makeKernel()
        
        import mcni
        N = 10000
        neutrons = mcni.neutron_buffer( N )
        for i in range(N):
            neutron = mcni.neutron(r=(0,0,0), v=(0,0,vi), time=0, prob=1)
            kernel.scatter(neutron)
            neutrons[i] = neutron
            #print neutrons[i]
            continue
        
        component.process( neutrons )
        
        from mcstas2.pyre_support._component_interfaces.monitors.IQE_monitor import get_histogram
        hist = get_histogram(component)
        
        from histogram.plotter import defaultPlotter
        defaultPlotter.plot(hist)
        return

    pass  # end of TestCase



mass = 50
temperature = 300
Ei = 70
from mcni.utils import conversion as C
vi = C.e2v(Ei)


def makeKernel():
    max_omega = 50
    max_Q = 13
    nMCsteps_to_calc_RARV = 1000
    return b.phonon_coherentinelastic_polyxtal_kernel(
        makeDispersion(), makeDW(),
        makeUnitcell(),
        temperature=temperature, Ei=Ei, max_omega=max_omega, max_Q=max_Q,
        nMCsteps_to_calc_RARV=nMCsteps_to_calc_RARV)


def makeUnitcell():
    from crystal.UnitCell import create_unitcell
    from crystal.Atom import atom
    atoms = [atom('Fe'), atom('Al')]
    positions = [(0,0,0), (0.5,0.5,0.5)]
    cellvectors = [ (1,0,0), (0,1,0), (0,0,1) ]
    return create_unitcell(cellvectors, atoms, positions)


def mkDOS():
    e0 = 0
    de = 0.5
    n = 100
    Z = N.arange(0,1,0.01)
    return b.linearlyinterpolateddos(e0, de, n, Z)


def makeDW():
    nsampling = 100
    return b.dwfromDOS(mkDOS(), mass, temperature, nsampling)
    

def makeDispersion():
    nAtoms = 2
    nBranches = 3 * nAtoms
    nQx = 10; nQy = 12; nQz = 14
    Qaxes = [ ([1,0,0], nQx),
              ([0,1,0], nQy),
              ([0,0,1], nQz),
              ]

    import histogram as H
    qx = H.axis('qx', H.arange(0, 1+1e-10, 1./(nQx-1)))
    qy = H.axis('qy', H.arange(0, 1+1e-10, 1./(nQy-1)))
    qz = H.axis('qz', H.arange(0, 1+1e-10, 1./(nQz-1)))
    br = H.axis('branchId', range(nBranches))
    atoms = H.axis('atomId', range(nAtoms))
    pols = H.axis('polId', range(3))
    realimags = H.axis('realimagId', range(2))

    eps = H.histogram('eps', [qx,qy,qz,br,atoms,pols,realimags])
    eps.I[:] = 1
    e = H.histogram('e', [qx,qy,qz,br],
                    fromfunction = lambda qx,qy,qz,br: (qx*qx+qy*qy+qz*qz)/3.*50,
                    )

    disp = b.linearlyinterpolateddispersion_3d(nAtoms, Qaxes, eps.I, e.I)
    disp = b.periodicdispersion(disp, ((1,0,0), (0,1,0), (0,0,1)))
    return disp


import mccomponents.sample.phonon.bindings as bindings
b = bindings.get('BoostPython')

import numpy as N


def pysuite():
    suite1 = unittest.makeSuite(TestCase)
    return unittest.TestSuite( (suite1,) )


def main():
    #debug.activate()
    #journal.debug("CompositeNeutronScatterer_Impl").activate()
    #journal.debug('phonon_coherent_inelastic_polyxtal_kernel').activate()
    pytests = pysuite()
    alltests = unittest.TestSuite( (pytests, ) )
    unittest.TextTestRunner(verbosity=2).run(alltests)
    
    
if __name__ == "__main__":
    main()
    
# version
__id__ = "$Id$"

# End of file 
