#!/usr/bin/env python
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#                                   Jiao Lin
#                      California Institute of Technology
#                        (C) 2008 All Rights Reserved  
#
# {LicenseText}
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#


def readMcStasData( filename ):
    lines = open(filename).readlines()
    for lineno, line in enumerate(lines):
        if line.startswith( '# variables' ): Istart = lineno
        elif line.startswith( '# Errors' ): Estart = lineno
        elif line.startswith( '# Events' ): Nstart = lineno
        continue
    Ilines = lines[ Istart+1: Estart ]
    Elines = lines[ Estart+1: Nstart ]
    Iarr = lines2arr( Ilines )
    Earr = lines2arr( Elines )
    return Iarr, Earr


def lines2arr( lines ):
    import numpy as N
    def numbers(line):
        return [ eval(t) for t in line.split() ]
    lines = [ numbers(line) for line in lines ]
    return N.array( lines )


# version
__id__ = "$Id$"

# End of file 
