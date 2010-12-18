#!/usr/bin/env python
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#                                   Jiao Lin
#                      California Institute of Technology
#                        (C) 2005 All Rights Reserved  
#
# {LicenseText}
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#


## for users, mcstas components are installed to ~/.mcstas2/python/mcstas2_components


packagename = 'mcstas2_components'

from pythonexportathome import path as pythonexportathome, init_package as _init_package
import os
packagepath = os.path.join( pythonexportathome, packagename )

def init():
    '''init the package'''
    _init_package( packagename )
    return


def init_category( category ):
    '''init the python package of the given category
    init_category( 'monitors' ) will init package mcstas2_components.monitors
    '''
    _init_package( '.'.join( [packagename, category] ) )
    return

    
# version
__id__ = "$Id$"

# End of file 
