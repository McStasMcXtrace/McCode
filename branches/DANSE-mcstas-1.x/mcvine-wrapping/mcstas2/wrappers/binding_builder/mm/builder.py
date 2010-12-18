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


import binding_mk
import pythonpackage_mk
import local_def
import os

from utils import callmm
from temporaryfiles import temporarydir
from shutil import rmtree



def build( binding ):
    build_binding( binding )
    export_pythonmodules( binding )
    return


def export_pythonmodules(binding):
    tmpdir = temporarydir()

    from shutil import copy

    #copy python sources to temp dir
    pysources = []
    for module in binding.python_sources:
        copy( module, tmpdir)
        pysources.append( os.path.basename( module ) )
        continue

    #make a Make.mm and local.def in the temp dir
    makemm = pythonpackage_mk.Generator(
        binding.python_package, pysources )
    makemm.generate( tmpdir )
    
    localdef = local_def.Generator(
        includes = binding.c_includes )
    localdef.generate( tmpdir )

    #run mm
    callmm( tmpdir )

    #clean up
    from mcstas2 import DEBUG
    if not DEBUG:
        rmtree( tmpdir )
    return
    

def build_binding(binding):
    tmpdir = temporarydir()

    from shutil import copy

    #copy c souces to temp dir
    csources = []
    for csrc in binding.c_sources + binding.c_headers:
        copy( csrc, tmpdir )
        csources.append( os.path.basename( csrc ) )
        continue

    #make a Make.mm and local.def there
    makemm = binding_mk.Generator(
        binding.python_package, binding.binding_module,
        csources, binding.c_libs, binding.c_libdirs )
    makemm.generate( tmpdir )

    localdef = local_def.Generator(
        includes = binding.c_includes,
        defines = binding.c_defines )
    localdef.generate( tmpdir )

    #run mm
    callmm( tmpdir )

    #clean up
    from mcstas2 import DEBUG
    if not DEBUG:
        rmtree( tmpdir )
    return
    

# version
__id__ = "$Id$"

# End of file 
