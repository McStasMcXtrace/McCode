#!/usr/bin/env python
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#                                   Jiao Lin
#                      California Institute of Technology
#                        (C) 2005  All Rights Reserved
#
# {LicenseText}
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#


from pyre.weaver.mills.CxxMill import CxxMill
from CxxClassRenderer import CxxClassRenderer


class CxxClassMillBase(CxxClassRenderer, CxxMill):


    def render(self, klass):
        document = self.weave(klass)
        return document


    def __init__(self):
        CxxMill.__init__(self)
        return


    def _renderDocument(self, document):
        return document.identify(self)

    pass # end of CxxClassMill

    
# version
__id__ = "$Id$"

# End of file 
