#!/usr/bin/env python
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#                                   Jiao Lin
#                      California Institute of Technology
#                      (C) 2008-2009  All Rights Reserved
#
# {LicenseText}
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#


# this is not a good implementation
# this assumes that all _fini steps will be performed
# in the _outputdir directory.
# for now this seems to be no problem, but this
# might be vulnerable.

from mcni.pyre_support.AbstractComponent import AbstractComponent


class ComponentInterface(AbstractComponent):

    
    def process(self, neutrons):
        return self.engine.process(neutrons)


    def _fini(self):
        # make sure to run _fini under my output directory
        def _(): return super(ComponentInterface, self)._fini()
        self._run_in_myoutputdir(_)
        return
    
    
    def _run_in_myoutputdir(self, func):
        dir = self._outputdir
        if not dir:
            import os
            dir = os.curdir
        return self._run_in_dir(func, dir)


    def _run_in_dir(self, func, dir):
        return run_in_dir(func, dir)
    


def run_in_dir(func, dir):
    import os
    savedir = os.path.abspath( os.curdir )
    os.chdir(dir)
    ret = func()
    os.chdir( savedir )
    return ret


# version
__id__ = "$Id$"

# End of file 
