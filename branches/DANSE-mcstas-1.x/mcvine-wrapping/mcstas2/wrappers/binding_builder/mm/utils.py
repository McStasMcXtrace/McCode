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


def callmm( path ):
    import os
    curdir = os.path.abspath( os.curdir )
    os.chdir( path )
    if os.system( 'mm' ): raise Exception("mm failed in %s" % path)
    os.chdir( curdir )
    return
    

# version
__id__ = "$Id$"

# End of file 
