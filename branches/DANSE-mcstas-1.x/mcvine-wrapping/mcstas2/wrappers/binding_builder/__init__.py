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


## toolsets to create python bindings.
## A binding is described as an instance of Binding.Binding class.
## A subpackage here describes a toolset to build bindings.
## Every subpackage must define a method "build", which
## takes a binding instance as the only argument.
##


def builder( name ):
    exec 'import %s as package' % name
    #package = __import__(name, {}, {},  [])
    return package


def binding( *args, **kwds ):
    deps = kwds.get( 'dependencies' )
    del kwds['dependencies']
    from Binding import Binding
    ret = Binding( *args, **kwds )
    if deps:
        for dep in deps: _addDep( dep, ret )
        pass
    return ret


def _addDep( dep, binding ):
    import softwareinstallationinfodb
    dep = softwareinstallationinfodb.info( dep )
    include_dir = dep.include
    binding.c_includes.append( include_dir )

    library_dir = dep.lib
    binding.c_libdirs.append( library_dir )
    return


# version
__id__ = "$Id$"

# End of file 
