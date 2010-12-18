#!/usr/bin/env python
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#                                   Jiao Lin
#                      California Institute of Technology
#                        (C) 2007  All Rights Reserved
#
# {LicenseText}
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#


all = []

default = 'mcstas2.components'
all.append(default)

from mcstas2.release import type as releasetype
if releasetype == 'user':
    from mcstas2.mcstascomponentspythontreeathome import packagename
    all.append( packagename )
    pass



# version
__id__ = "$Id$"

# End of file 
