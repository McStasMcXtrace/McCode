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
This test makes sure S(Q,E) monitor is reasonable.
It should show a evely distributed intensities in the S(Q,E) plot.
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

        Ei = 70
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
        
        import mcni
        from mcni.utils import conversion as C
        neutrons = mcni.neutron_buffer( nQ*nE )
        import numpy as N
        count = 0
        for Q in N.arange(Qmin, Qmax, (Qmax-Qmin)/nQ):
            for E in N.arange(Emin,Emax,(Emax-Emin)/nE):
                Ef = Ei-E
                cosphi = (Ei+Ef-C.k2e(Q))/(2*N.sqrt(Ei)*N.sqrt(Ef))
                vf = C.e2v(Ef)
                vfz = vf*cosphi
                sinphi = N.sqrt(1-cosphi*cosphi)
                vfx = vf*sinphi
                neutrons[count] = mcni.neutron(r=(0,0,0), v=(vfx,0,vfz), time = 0, prob = 1)
                count += 1
                continue
        component.process( neutrons )
        
        from mcstas2.pyre_support._component_interfaces.monitors.IQE_monitor import get_histogram
        hist = get_histogram(component)

        from histogram.plotter import defaultPlotter
        defaultPlotter.plot(hist)
        return

    pass  # end of TestCase




def pysuite():
    suite1 = unittest.makeSuite(TestCase)
    return unittest.TestSuite( (suite1,) )


def main():
    #debug.activate()
    #journal.debug("CompositeNeutronScatterer_Impl").activate()
    pytests = pysuite()
    alltests = unittest.TestSuite( (pytests, ) )
    unittest.TextTestRunner(verbosity=2).run(alltests)
    
    
if __name__ == "__main__":
    main()
    
# version
__id__ = "$Id$"

# End of file 
