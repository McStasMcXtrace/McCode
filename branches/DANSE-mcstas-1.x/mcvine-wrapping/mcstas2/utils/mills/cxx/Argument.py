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


class Argument:

    def __init__(self, type, name, default = None):
        self.type = type
        self.name = name
        self.default = default
        return


    def identify(self, visitor): return visitor.onArgument(self)
    

    def __str__(self):
        return "%s %s = %s" % (self.type, self.name, self.default)


    def __repr(self): return str(self)

    pass # end of Argument


# version
__id__ = "$Id$"

# End of file 
