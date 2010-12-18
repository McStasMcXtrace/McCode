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


componentname = 'Slit'
category = 'optics'

class TestCase(unittest.TestCase):

    def test(self):
        "wrap Slit"
        
        from mcstas2 import componentfactory
        factory = componentfactory( category, componentname )
        
        component = factory(
            'component',
            xmin=-0.01, xmax=0.01, ymin=-0.01, ymax=0.01,
            )
        
        import mcni
        neutrons = mcni.neutron_buffer( 5 )
        for i in range(5):
            neutrons[i] = mcni.neutron(r=(0,0,-1), v=(0,0,3000), time = 0, prob = 1)
            continue
        component.process( neutrons )
        print neutrons
        return

    def test2(self):
        "wrap Slit: ctor using radius and cut"
        
        from mcstas2 import componentfactory
        factory = componentfactory( category, componentname )
        
        component = factory(
            'component',
            radius=0.01, cut=1e-10)
        
        
        import mcni
        neutrons = mcni.neutron_buffer( 5 )
        for i in range(5):
            neutrons[i] = mcni.neutron(r=(0,0,-1), v=(0,0,3000), time = 0, prob = 1)
            continue
        component.process( neutrons )
        print neutrons
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
