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


#libraries to be linked with
libstolink = ['boost_python']

define_macros = [
    ('BOOST_PYTHON_MAX_ARITY', 32),
    ]


def generate_binding_sources( bindingname, klass, path ):
    '''generate source codes for a binding

    bindingname: name of the binding module
    klass: the c++ class to bind. instance of mcstas2.utils.mills.cxx.Class
    path: the path in which generated sources will be stored

    return: paths to generated sources
    '''
    wrapmethodname = 'wrap_' + klass.name
    
    from module_cc import generate
    module_cc = generate( bindingname, wrapmethodname, path )
    
    from wrap_cc import generate
    wrap_cc = generate( klass, path, wrapmethodname )

    return {
        'c' : [module_cc, wrap_cc],
        }


# version
__id__ = "$Id$"


# End of file 
