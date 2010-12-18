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


## methods to create a python module to accompany the wrapped mcstas component


def generate( componentinfo, bindingname, path ):
    from factorymethod_py import generate
    factorymethod_py = generate( componentinfo, bindingname, path )
    return [factorymethod_py]


# version
__id__ = "$Id$"

# End of file 
