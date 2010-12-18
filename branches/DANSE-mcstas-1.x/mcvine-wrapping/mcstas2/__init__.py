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


DEBUG = False
# DEBUG = True


def srandom( seed ):
    '''change the seed for random number generator
    '''
    from bindings.boostpython import binding
    return binding.srandom( seed )

from mcni.seeder import register
register( srandom )
del register



def componentfactory( category, type ):
    '''obtain component factory method of given category and type
Examples:
  componentfactory( 'monitors', 'E_monitor' )
  '''
    from components import componentfactory
    from components.Registry import NotRegisteredError
    try: f = componentfactory( category, type )
    except NotRegisteredError: f = defaultcomponentfactory( category, type )
    return f


def componentmodule( category, type ):
    '''obtain the python module for the given category and type
Examples:
  componentmodule( 'monitors', 'E_monitor' )
  '''
    f = componentfactory( category, type )
    return __import__( f.__module__, {}, {}, [''] )


def printcomponentinfo( category, type ):
    '''print info about the component of given category and type
Examples:
  printcomponentinfo( 'monitors', 'E_monitor' )
  '''
    print componentinfo( category, type )
    return


def componentinfo( category, type ):
    '''obtain component info of given category and type
Examples:
  componentinfo( 'monitors', 'E_monitor' )
  '''    
    import components
    if components.registered( category, type ):
        from components import componentinfo
        info = componentinfo( category, type )
    else:
        from utils.parsers import parseComponent
        path = defaultcomponentpath( category, type )
        info = parseComponent( path )
    return info


def wrapcomponent( componentfilename, componentcategory, **kwds ):
    from release import type as releasetype
    if releasetype == 'user':
        from mcstascomponentspythontreeathome import init_category
        init_category( componentcategory )
        pass

    #set appropriate builder
    builders = {
        'developer': 'mm',
        'user': 'distutils',
        }
    if kwds.get('buildername') is None: kwds['buildername'] = builders[releasetype]

    #set appropriate python export path
    from pythonexportathome import path as pytreeathome
    pythontrees = {
        'developer': None,
        'user': pytreeathome,
        }
    if kwds.get('pythonexportroot') is None: kwds['pythonexportroot'] = pythontrees[releasetype]

    #set appropriate python package
    if releasetype == 'user':
        from mcstascomponentspythontreeathome import packagename
        pythonpackage = '%s.%s' % (packagename, componentcategory )
    else:
        pythonpackage = None
    if kwds.get('pythonpackage') is None: kwds['pythonpackage'] = pythonpackage
    
    from wrappers import wrap
    return wrap( componentfilename, componentcategory, **kwds )


def listallcomponentcategories( ):
    '''list all component categories'''
    defaultcategories = listalldefaultcomponentcategories()
    import components
    categoriesinregistry = components.categoriesInRegistry()
    return uniquelist( defaultcategories + categoriesinregistry )


def listcomponentsincategory( category ):
    categories = listalldefaultcomponentcategories()
    if category not in categories: return []
    
    defaultcomponents = listdefaultcomponentsincategory( category )
    import components 
    registered = components.registeredComponentsInCategory( category )
    return uniquelist( defaultcomponents + registered )


def defaultcomponentfactory( category, type ):
    path = defaultcomponentpath( category, type )
    wrapcomponent( path, category )
    from components import componentfactory
    return componentfactory( category, type )


def listalldefaultcomponentcategories( ):
    libdir = defaultcomponentlibrarypath()
    import os
    from os.path import isdir, join
    excluded = ['CVS', 'data']
    items = os.listdir( libdir )
    items = filter(
        lambda item:
        not item.startswith( '.' ) and item not in excluded and isdir( join(libdir, item) ),
        items )
    return items


def listdefaultcomponentsincategory( category ):
    path = defaultcategorypath( category )
    import os
    from os.path import isfile, join
    excluded = ['CVS']
    items = os.listdir( path )
    postfix = '.comp'
    items = filter(
        lambda item:
        not item.startswith( '.' ) and item not in excluded and isfile( join(path, item) ) \
        and item.endswith( postfix ),
        items )
    return [item[: -len(postfix) ] for item in items]


def defaultcategorypath( category ):
    libdir = defaultcomponentlibrarypath()
    import os
    path = os.path.join( libdir, category )
    if not os.path.exists( path ) or not os.path.isdir(path):
        raise RuntimeError, "default component category %s does not exist. Cannot find %s" % (
            category, path )
    return path


def defaultcomponentpath( category, type ):
    libdir = defaultcomponentlibrarypath()
    import os
    path = os.path.join( libdir, category, '%s.comp' % type )
    if not os.path.exists( path ) or not os.path.isfile(path):
        raise RuntimeError, "default component (%s, %s) does not exist. Cannot find %s" % (
            category, type, path )
    return path


def defaultcomponentlibrarypath( ):
    from release import component_library_dir as dir
    return dir



from sys import version_info
if version_info[0] <=2 and version_info[1] <= 3:
    def uniquelist( l ):
        u = {}
        for i in l: u[i] = 1
        return u.keys()
    
else:
    def uniquelist( l ):
        return [ u for u in l if u not in locals()['_[1]'] ]

del version_info


def _init():
    from release import type as releasetype
    if releasetype == 'user':
        #add user python tree to path if necessary
        from pythonexportathome import path as pytreeathome
        import sys
        sys.path = [pytreeathome] + sys.path
        
        #init mcstas components python tree if necessary
        from mcstascomponentspythontreeathome import init 
        init()
        pass
    return


_init()

    
# version
__id__ = "$Id$"

# End of file 
