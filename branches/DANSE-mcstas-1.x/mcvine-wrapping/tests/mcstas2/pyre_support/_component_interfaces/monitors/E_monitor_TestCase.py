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


# test instrument
from TestInstrument1 import Instrument as base
class Instrument(base):

    def __init__(self, name='E_monitor_TestCase'):
        base.__init__(self, name)
        return
    

from mcni.pyre_support.MpiApplication import usempi
outputdir = 'E_monitor_TestCase-out'
if usempi: 
    import mpi
    outputdir += '-worker-%s' % mpi.world().rank


class TestCase(unittest.TestCase):

    def test1(self):
        if usempi:
            import sys
            sys.argv += ['--mpirun.nodes=2']

        instrument = Instrument()
        instrument.run()

        import time
        ctime = time.time()

        #check output directory exists
        self.assert_( os.path.exists( outputdir ) )
        
        #make sure files were just created
        for item in os.listdir( outputdir ):
            path = os.path.join( outputdir, item )
            self.assert_( os.path.exists( path ) )

            mtime = os.path.getmtime( path )
            self.assert_( ctime - mtime >= 0 )
            #print "path:", path, "timediff:", ctime - mtime 
            self.assert_( ctime - mtime < 10 )
            continue
        
        return
    
    pass  # end of TestCase


import os


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
