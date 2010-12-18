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


class Method:

    def __init__(self, name, args, body, type = None):
        self.name = name
        self.args = args
        self.body = body
        self.type = type
        return


    def identify(self, visitor): return visitor.onMethod( self )

    pass # end of Method


# version
__id__ = "$Id$"

# End of file 
