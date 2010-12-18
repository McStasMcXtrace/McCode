#!/usr/bin/env python
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#                                   Jiao Lin
#                      California Institute of Technology
#                        (C) 2007 All Rights Reserved  
#
# {LicenseText}
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#



import unittestX as unittest
import journal, os


class build_TestCase(unittest.TestCase):

    def test(self):
        "binding builder using distutils"
        from mcstas2.wrappers.binding_builder.distutils import build
        from mcstas2.wrappers.binding_builder.Binding import Binding
        binding = Binding(
            python_package = 'projectname', binding_module = 'projectname',
            c_headers = [
            'src/bindings.h',
            'src/exceptions.h',
            'src/misc.h',
            ],
            c_sources = [
            'src/bindings.cc',
            'src/exceptions.cc',
            'src/misc.cc',
            'src/projectnamemodule.cc',
            ],
            python_sources = [
            'src/__init__.py',
            ],
            c_libs = [
            ],
            c_includes = [
            os.environ[ 'BLD_INCDIR' ],
            os.environ[ 'CONFIG_PLATFORM_INCDIR' ],
            os.environ[ 'CONFIG_COMPILER_INCDIR' ],
            os.environ[ 'CONFIG_TARGET_INCDIR' ],
            ],
            )
        build(binding, site_package_path = os.environ['EXPORT_MODULES'])

        import projectname.projectname as p
        self.assertEqual( p.hello(), 'hello' )
        return

    pass  # end of build_TestCase



def pysuite():
    suite1 = unittest.makeSuite(build_TestCase)
    return unittest.TestSuite( (suite1,) )


def main():
    #debug.activate()
    #journal.debug("CompositeNeutronScatterer_Impl").activate()
    pytests = pysuite()
    alltests = unittest.TestSuite( (pytests, ) )
    unittest.TextTestRunner(verbosity=2).run(alltests)
    
    
if __name__ == "__main__":
    main()
    
# version
__id__ = "$Id$"

# End of file 
