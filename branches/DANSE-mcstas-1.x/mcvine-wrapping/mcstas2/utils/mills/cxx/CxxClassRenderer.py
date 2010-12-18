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


class CxxClassRenderer(object):

    def onClass(self, klass): self._nie("onClass")
    def onMember(self, member): self._nie("onMember")
    def onMethod(self, method): self._nie("onMethod")
    def onArgument(self, argument): self._nie("onArgument")

    def _nie(self, name):
        raise NotImplementedError, "%s must provide method '%s'" % (
            self.__class__.__name__, name)

    pass # end of CxxClassRenderer


# version
__id__ = "$Id$"

# End of file 
