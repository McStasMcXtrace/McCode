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


from mcni.pyre_support.Instrument import Instrument as base
class Instrument(base):

    class Inventory( base.Inventory ):

        from mcni.pyre_support import facility
        source = facility(name='source')
        monitor = facility(name='monitor')
        pass # end of Inventory


    def __init__(self, name = 'componentfactory_TestCase_Instrument'):
        base.__init__(self, name)
        return
    

    def _defaults(self):
        base._defaults(self)
        self.inventory.sequence = ['source', 'monitor']

        from mcstas2.pyre_support import componentfactory as factory
        self.inventory.source = factory('sources', 'Source_simple')('source')
        self.inventory.monitor = factory('monitors', 'E_monitor')('monitor')

        print self.inventory.source
        
        geometer = self.inventory.geometer

        geometer.inventory.monitor = (0,0,1), (0,0,0)

        return
    
    pass # end of Instrument




class TestCase(unittest.TestCase):

    def test1(self):
        instrument = Instrument()

        import sys
        save = sys.argv
        sys.argv = [
            '',
            '--ncount=10',
            '--buffer_size=5',
            '--output-dir=componentfactory_testcase_out',
            '--overwrite-datafiles',
            ]

        instrument.run()
        sys.argv = save
        return
    
    pass  # end of TestCase



def pysuite():
    suite1 = unittest.makeSuite(TestCase)
    return unittest.TestSuite( (suite1,) )


def main():
    #debug.activate()
    #journal.debug("CompositeNeutronScatterer_Impl").activate()
    journal.warning('mcstas2.parsers.ComponentInfo').deactivate()
    journal.debug("mcstas2.pyre_support").activate()
    import mcstas2.mcstas2bp
    pytests = pysuite()
    alltests = unittest.TestSuite( (pytests, ) )
    unittest.TextTestRunner(verbosity=2).run(alltests)
    
    
if __name__ == "__main__":
    main()
    
# version
__id__ = "$Id$"

# End of file 
