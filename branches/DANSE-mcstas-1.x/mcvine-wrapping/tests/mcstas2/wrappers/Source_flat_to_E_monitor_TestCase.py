#!/usr/bin/env python
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#                                   Jiao Lin
#                      California Institute of Technology
#                        (C) 2007 All Rights Reserved  
#
# {LicenseText}
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#



import unittestX as unittest
import journal


class TestCase(unittest.TestCase):

    def test(self):
        "Source_flat --> E_monitor"
        from mcstas2 import componentfactory
        sflatfac = componentfactory( 'sources', 'Source_flat' )
        sflat = sflatfac(
            'sflat',
            radius=0.1, dist=2, xw=0.1, yh=0.1, E0=55, dE=2)
        
        from mcstas2.wrappers import wrap
        wrap( 'E_monitor.comp', 'monitors' ) 
        emonfac = componentfactory( 'monitors', 'E_monitor' )
        emon = emonfac(
            'emon',
            nchan=20, filename="e.dat",
            xmin=-0.2, xmax=0.2,
            ymin=-0.2, ymax=0.2,
            Emin=50, Emax=60)

        import mcni
        instrument = mcni.instrument( [sflat, emon] )

        geometer = mcni.geometer()
        geometer.register( sflat, (0,0,0), (0,0,0) )
        geometer.register( emon, (0,0,1), (0,0,0) )

        neutrons = mcni.neutron_buffer( 100 )

        mcni.simulate( instrument, geometer, neutrons )

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
