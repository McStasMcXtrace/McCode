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


## create Make.mm for exporting binding


template = '''
PROJECT = %(pythonpackage)s
PACKAGE = %(bindingmodule)smodule
MODULE = projectname

include std-pythonmodule.def
include local.def

PROJ_CXX_SRCLIB = %(libs)s

PROJ_SRCS = \\
%(srcs)s
''' 


class Generator:

    target = 'Make.mm'

    def __init__(self, package, bindingmodule, srcs, libs, libdirs):
        """Generator( 'mcstas2.components.monitors', 'E_monitorbp', ['E_monitor.cc'], ['mcstas2', 'mcni'] )
        """
        self.package = package
        self.bindingmodule = bindingmodule
        self.srcs = srcs
        self.libs = libs
        self.libdirs = libdirs
        return
    
    def generate(self, path, target = None):
        '''generate( "E_monitor", "binding.mk" )
        '''
        if not target: target = self.target
        
        srcs_str = '\n'.join( [ '\t%s \\' % src for src in self.srcs ] )
        libs_str = ' '.join( [ '-l %s' % lib for lib in self.libs ] )
        libdirs_str = ' '.join( [ '-L%s' % libdir for libdir in self.libdirs ] )
        libs_str += ' ' + libdirs_str
        package_str = self.package.replace( '.', '/' )
        content = template % {
            'pythonpackage' : package_str,
            'bindingmodule' : self.bindingmodule,
            'libs': libs_str,
            'srcs': srcs_str,
            }
        import os
        filename = os.path.join( path, target )

        open(filename, 'w').write( content )
        return filename

    pass # end of Generate
    

# version
__id__ = "$Id$"

# End of file 
