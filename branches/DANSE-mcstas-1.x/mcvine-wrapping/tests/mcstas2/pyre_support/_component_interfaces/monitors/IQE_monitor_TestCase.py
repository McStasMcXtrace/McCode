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

    def test1(self):
        from TestInstrument1 import Instrument
        instrument = Instrument('IQE_monitor_TestCase')

        import sys
        save = sys.argv
        sys.argv = [
            '',
            '--ncount=10',
            '--buffer_size=5',
            '--output-dir=IQE_monitor_TestCase-out',
            '--overwrite-datafiles',
            '--monitor=iqemonitor',
            ]

        instrument.run()
        sys.argv = save
        return


    def test2(self):
        from ssd import App
        app = App('IQE_monitor_TestCase-test2')

        import sys
        save = sys.argv
        sys.argv = [
            '',
            '--ncount=1e5',
            '--buffer_size=100000',
            '--output-dir=IQE_monitor_TestCase-test2-out',
            '--overwrite-datafiles',
            ]

        app.run()
        
        sys.argv = save
        return
    
    
    pass  # end of TestCase



def pysuite():
    suite1 = unittest.makeSuite(TestCase)
    return unittest.TestSuite( (suite1,) )


def main():
    #debug.activate()
    #journal.debug("CompositeNeutronScatterer_Impl").activate()
    #journal.debug("ElementaryComponentGenerator").activate()
    pytests = pysuite()
    alltests = unittest.TestSuite( (pytests, ) )
    unittest.TextTestRunner(verbosity=2).run(alltests)
    
    
if __name__ == "__main__":
    main()
    
# version
__id__ = "$Id$"

# End of file 
