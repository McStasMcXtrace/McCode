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

def registered( category, type ):
    global _registry
    return _registry.registered( category, type )


def categoriesInRegistry( ):
    global _registry
    return _registry.types.keys()


def registeredComponentsInCategory( category ):
    global _registry
    return _registry.types.get( category ) or []


def componentfactory( category, type ):
    global _registry
    return _registry.getFactory( category, type )


def componentinfo( category, type ):
    global _registry
    return _registry.getInfo( category, type )


def registercomponent( category, type, componentmodule ):
    global _registry
    _registry.register( category, type, componentmodule )
    return


from Registry import Registry
_registry = Registry()
del Registry



# version
__id__ = "$Id$"

# End of file 
