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

    nx = core.nxchan; ny =core.nychan; nb = core.nbchan
    n = nx * ny * nb
    shape = nx, ny, nb

    xmin = -core.xwidth/2; xmax = core.xwidth/2
    ymin = -core.yheight/2; ymax = core.yheight/2
    dx = (xmax - xmin)/nx
    dy = (ymax - ymin)/ny

    if core.bmax!=0:
        bmax=core.bmax
        bmin=core.bmin
        db=(bmax-bmin)/nb
    else :
        db = core.deltab
        bmin=0;
        bmax=nb*db+bmin

    Iarr = bpptr2npyarr( core.getTOF_p_00( ), 'double', n ).copy()
    E2arr = bpptr2npyarr( core.getTOF_p2_00( ), 'double', n ).copy()
    Iarr.shape = E2arr.shape = shape

    from histogram import histogram, axis, arange
    xaxis = axis( 'x', boundaries=arange( xmin, xmax+dx/10, dx ) )
    yaxis = axis( 'y', boundaries=arange( ymin, ymax+dy/10, dy ) )
    baxis = axis( 'b', boundaries=arange( bmin, bmax+db/10, db ) )

    h = histogram(
        'I(x,y,b)', [xaxis,yaxis,baxis],
        data = Iarr, errors = E2arr )
    return h


# version
__id__ = "$Id$"

# End of file 
