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


import journal
debug = journal.debug('binding_builder.distutils')


def build( binding, site_package_path = None ):
    '''
    binding: the binding to build
    site_package_path: the path to which the built binding will be installed.
      if None, installed to /.../site-pacakges
    '''
    python_package = binding.python_package
    binding_module = binding.binding_module
    c_headers = binding.c_headers
    c_sources = binding.c_sources
    python_sources = binding.python_sources
    c_libs = binding.c_libs
    c_libdirs = binding.c_libdirs
    c_includes = binding.c_includes
    c_defines = binding.c_defines
    
    from distutils.core import setup, Extension
    ext = Extension(
        '%s.%s' % (python_package, binding_module ),
        c_sources,# + c_headers,
        include_dirs = c_includes,
        libraries = c_libs,
        library_dirs = c_libdirs,
        define_macros = c_defines,
        )

    if len(python_sources):
        # need to make sure all python sources come from the same directory
        import os
        pysrcdir = os.path.split( python_sources[0] ) [0]
        for module in python_sources:
            if os.path.split( module )[0] != pysrcdir:
                raise "not all python sources in the same directory: %s" %(
                    python_sources, )
            continue

        package_dir = { python_package: pysrcdir }
    else:
        package_dir = {}
        pass


    import sys
    save = sys.argv
    sys.argv = ['', 'install']
    if site_package_path: sys.argv.append( '--install-lib=%s' % site_package_path )


    name = python_package.split( '.' )[0]
    packages = [python_package]
    ext_modules = [ext]
    debug.log('name=%s, packages=%s, package_dir=%s, ext_modules=%s' % (
        name, packages, package_dir, ext_modules, )
              )
    setup(
        name = name,
        packages = packages,
        package_dir = package_dir,
        ext_modules = ext_modules )
    sys.argv = save
    return



# version
__id__ = "$Id$"

# End of file 
