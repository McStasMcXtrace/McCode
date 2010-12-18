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

        from mcstas2.pyre_support import facility
        source = facility( 'sources', 'Source_simple', 'source' )
        monitor = facility( 'monitors', 'E_monitor', 'monitor' ) 
        pass # end of Inventory


    def __init__(self, name = 'test1'):
        base.__init__(self, name)
        return
    

    def _defaults(self):
        base._defaults(self)
        self.inventory.sequence = ['source', 'monitor']
        geometer = self.inventory.geometer
        geometer.inventory.monitor = (0,0,1), (0,0,0)
        return
    
    pass # end of Instrument



def main():
    import journal
    journal.warning('mcstas2.parsers.ComponentInfo').deactivate()
    Instrument('test1').run()
    return    
    
if __name__ == "__main__":
    main()
    
# version
__id__ = "$Id$"

# End of file 
