#!/usr/bin/env python
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#                                   Jiao Lin
#                      California Institute of Technology
#                        (C) 2005 All Rights Reserved  
#
# {LicenseText}
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#


import os
dvdir = os.environ.get('DV_DIR')
exportroot = os.environ.get('EXPORT_ROOT')
pyredir = os.environ.get("PYRE_DIR")
mcvinedir = os.environ.get("MCVINE_DIR")


if dvdir and exportroot: type = 'developer'
else: 
    type = 'user'
    mcvinedir = mcvinedir or pyredir or exportroot
    if not mcvinedir:
        raise RuntimeError, "environment variable MCVINE_DIR was not defined. please define it to the path of the mcvine installation"


component_library_dir = os.environ.get('MCSTAS_COMPONENT_LIBDIR', None)
if component_library_dir is None:
    component_library_dir = os.path.join(mcvinedir, 'share', 'mcstas2', 'McStas-Components')

    
# version
__id__ = "$Id$"

# End of file 
