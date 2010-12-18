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

#Generate <name>module.cc

template = '''
#include <boost/python.hpp>


void %(wrapmethodname)s();


BOOST_PYTHON_MODULE(%(bindingname)s)
{
  using namespace boost::python;
  %(wrapmethodname)s();
}


'''

def generate( name, wrapmethodname, path ):
    subs = {
        'bindingname': name,
        'wrapmethodname': wrapmethodname,
        }
    content = template % subs
    import os
    filename = os.path.join( path, "%smodule.cc" % name )
    open(filename, 'w').write( content )
    return filename


# version
__id__ = "$Id$"


# End of file 
