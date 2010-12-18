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


def build( binding, site_package_path = None ):
    if site_package_path is not None:
        import journal
        warning = journal.warning( 'binding_builder.mm' )
        warning.log( 'mm can only export python modules to predefined $EXPORT_ROOT/modules' )
        pass
    from builder import build
    build( binding )
    return


# version
__id__ = "$Id$"

# End of file 
