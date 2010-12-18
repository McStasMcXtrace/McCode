#!/usr/bin/env python
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#                                   Jiao Lin
#                      California Institute of Technology
#                        (C) 2009  All Rights Reserved
#
# {LicenseText}
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#



def getInterface(category, type):
    thispackage = 'mcstas2.pyre_support._component_interfaces'
    package = '.'.join([thispackage, category, type])
    try:
        package = _import(package)
    except ImportError:
        import journal
        debug = journal.debug('mcstas2.pyre_support')
        import traceback
        debug.log(traceback.format_exc())
        import default as package
    return getattr(package, 'ComponentInterface')


def _import(package):
    return __import__(package, {}, {}, [''])


# version
__id__ = "$Id$"

# End of file 
