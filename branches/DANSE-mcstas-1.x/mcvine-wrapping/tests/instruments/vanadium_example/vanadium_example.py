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



import journal


from mcni.pyre_support.Instrument import Instrument as base
class Instrument(base):

    class Inventory( base.Inventory ):

        from mcstas2.pyre_support import facility
        source = facility( 'sources', 'Source_simple', 'source' )
        sample = facility( 'samples', 'V_sample', 'sample')
        monitor = facility( 'monitors', 'PSD_monitor_4PI', 'monitor' ) 
        pass # end of Inventory


    def __init__(self, name = 'test1'):
        base.__init__(self, name)
        return
    

    def _defaults(self):
        base._defaults(self)
        self.inventory.sequence = ['source', 'sample', 'monitor']
        geometer = self.inventory.geometer
        geometer.inventory.sample = (0,0,1), (0,0,0)
        geometer.inventory.monitor = (0,0,1), (0,0,0)
        return
    

    def _init(self):
        base._init(self)
        return


    pass # end of Instrument



def main():
    Instrument('vanadium_example').run()
    return    
    
if __name__ == "__main__":
    main()
    
# version
__id__ = "$Id$"

# End of file 
