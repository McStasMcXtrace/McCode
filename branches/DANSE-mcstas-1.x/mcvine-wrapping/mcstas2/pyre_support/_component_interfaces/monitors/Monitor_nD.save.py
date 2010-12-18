# -*- Python -*-
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#                               Alex Dementsov
#                      California Institute of Technology
#                        (C) 2010  All Rights Reserved
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

    xmin = 0; xmax = 1
    ymin = 0; ymax = 1
    dx = 0.1
    dy = 0.1


#    xmin = core.xmin; xmax = core.xmax
#    ymin = core.ymin; ymax = core.ymax
#    dx = xmax - xmin
#    dy = ymax - ymin

    # OUTPUT PARAMETERS (DEFS, Vars)
    #Iarr = bpptr2npyarr( core.getDEFS( ), 'double', n ).copy()  #?
    #E2arr = bpptr2npyarr( core.getPSD_p2_00( ), 'double', n ).copy()
    #Iarr.shape = E2arr.shape = shape

    from histogram import histogram, axis, arange
    xaxis = axis( 'x', arange( xmin, xmax, dx ) )
    yaxis = axis( 'y', arange( ymin, ymax, dy ) )

    h = histogram( 'I(x,y)', [xaxis,yaxis], data = [2,yaxis], errors=[2,yaxis])    #Iarr)    #, errors = E2arr )
    return h

__date__ = "$Nov 19, 2010 6:24:56 PM$"


