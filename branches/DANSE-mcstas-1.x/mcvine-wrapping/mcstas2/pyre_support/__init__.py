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


def componentfactory( category, type ):
    '''factory method for pyre component class of the
given mcstas component category and type.

  Example:
    componentfactory( 'sources', 'Source_simple' )
    
    '''
    factoryattributename = 'pyrecomponentfactory'
    
    import mcstas2
    module = mcstas2.componentmodule( category, type )
    
    try: return getattr(module, factoryattributename)
    except: pass
    
    factory = module.factory
    info = module.info
    factory.arguments = info.input_parameters
    from mcstas2.utils.pyre_support import elementaryComponentClassGenerator as generator

    from _component_interfaces import getInterface
    interface = getInterface(category, type)

    f = generator( ctor_takes_name = True, baseclass = interface )( factory )

    setattr( module, factoryattributename, f )
    return f


def facility( category, type, name ):
    '''create a neutron instrument component facility, and set
the default to a mcstas component of given category and type.
'''
    from mcni.pyre_support import facility
    default = componentfactory( category, type )( name )
    return facility( name, default = default)


# version
__id__ = "$Id$"

# End of file 
