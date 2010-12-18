#!/usr/bin/env python
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#                                   Jiao Lin
#                      California Institute of Technology
#                        (C) 2008  All Rights Reserved
#
# {LicenseText}
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#



from default import ComponentInterface as base

class ComponentInterface(base):
    
    def _get_histogram(self):
        return get_histogram(self)


def get_histogram( monitor ):
    from mcstas2.utils.carray import bpptr2npyarr
    core = monitor.core()

    nx = core.nx; ny =core.ny
    n = nx * ny
    shape = nx, ny

    Iarr = bpptr2npyarr( core.getPSD_p_00( ), 'double', n ).copy()
    E2arr = bpptr2npyarr( core.getPSD_p2_00( ), 'double', n ).copy()
    Iarr.shape = E2arr.shape = shape

    from histogram import histogram, axis, arange
    dx = 360./nx
    xaxis = axis( 'x', arange( 0, 360, dx ), unit = 'deg' )

    dy = 180/ny
    yaxis = axis( 'y', arange( -90, 90, dy ), unit = 'deg' )

    h = histogram( 'I(x,y)', [xaxis,yaxis], data = Iarr, errors = E2arr )
    return h


# version
__id__ = "$Id$"

# End of file 
