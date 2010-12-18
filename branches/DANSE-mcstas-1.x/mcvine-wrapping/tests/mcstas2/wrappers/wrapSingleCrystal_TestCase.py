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


componentname = 'Single_crystal'
componentfile = '%s.comp' % componentname
category = 'samples'

class wrap_TestCase(unittest.TestCase):

    def test(self):
        "wrap Single_crystal"
        from mcstas2.wrappers import wrap
        wrap( componentfile, category )
        from mcstas2.components import componentfactory
        factory = componentfactory( category, componentname )
        component = factory(
            'component',
            )
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
