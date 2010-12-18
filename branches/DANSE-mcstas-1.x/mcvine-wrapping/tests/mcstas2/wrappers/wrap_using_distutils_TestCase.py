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


componentname = 'E_monitor'
componentfile = '%s.comp' % componentname
category = 'monitors'

class wrap_TestCase(unittest.TestCase):

    def test(self):
        "wrap E_monitor"
        from mcstas2.wrappers import wrap
        wrap( componentfile, category, buildername = 'distutils' )
        from mcstas2.components import componentfactory
        emonfac = componentfactory( category, componentname )
        emon = emonfac(
            'emon',
            nchan=20, filename="e.dat",
            xmin=-0.2, xmax=0.2,
            ymin=-0.2, ymax=0.2,
            Emin=50, Emax=60)
        return

    pass  # end of wrap_TestCase



def pysuite():
    suite1 = unittest.makeSuite(wrap_TestCase)
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
