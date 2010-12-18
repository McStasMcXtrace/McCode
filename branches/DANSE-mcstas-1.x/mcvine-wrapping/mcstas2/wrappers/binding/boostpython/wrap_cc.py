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

#Generate wrap.cc, which wraps a mcstas component using boost python

template = '''
#include "mcstas2/boostpython_binding/wrap_component.h"
#include "%(headername)s.h"
#include "bpext/bpext.h"

namespace mcstas2 {
using namespace bpext;
%(ptrmethods)s
}

void %(wrapmethodname)s() 
{
  using namespace mcstas2;
  using namespace mcstas2::boostpython_binding;
  using namespace boost::python;

  // alias
  typedef mcstas2::%(classname)s w_t;

  // wrap
  component_wrapper<w_t>::wrap
    ("%(classname)s", init<%(ctor_args)s>()
    // ctor policies
    %(ctor_policies)s
    )
    // expose data members
    %(expose_datamembers)s
    // expose pointer methods
    %(expose_ptrmethods)s
    ;
}
'''

def generate( klass, path, wrapmethodname, headername = None ):
    classname = klass.name
    
    ctor = klass.constructors() [0]
    ctor_args = ctor.args

    #default header name is the same as class name
    if not headername: headername = classname

    #convert args to a string
    ctor_args_str = _build_args_str( ctor_args )
    ctor_policies_str = _build_policies( ctor_args )
    datamembers = filter( lambda m: not _ispointer(m) and _isofbasictype(m),
                          klass.public_members )
    expose_datamembers_str = _build_expose_datamembers( datamembers )
    ptr_members = _ptr_members( klass.public_members )
    ptrmethods = _build_ptrmethods( ptr_members, klass )
    expose_ptrmethods = _build_expose_ptrmethods( ptr_members )
    
    content = template % {
        'headername': headername,
        'classname': classname,
        'ctor_args': ctor_args_str,
        'ctor_policies': ctor_policies_str,
        'expose_datamembers': expose_datamembers_str,
        'ptrmethods': ptrmethods,
        'expose_ptrmethods': expose_ptrmethods,
        'wrapmethodname': wrapmethodname,
        }
    import os
    filename = os.path.join( path, "wrap.cc" )
    open(filename, 'w').write( content )
    return filename


def _ispointer( argument ):
    type = argument.type
    return type[-1] == '*' and not type.startswith( 'char' )


def _isofbasictype( argument ):
    type = argument.type
    return type in [ 'double', 'int', 'float', 'char' ]


def _build_args_str( args ):
    return ",".join( [ _arg_str( arg ) for arg in args] )


def _arg_str( arg ):
    return "%s" % (arg.type,)


def _build_expose_datamembers( args ):
    '''return
.def_readonly( name, &w_t::$name)
...
'''
    return '\n'.join(
        [ '    .def_readonly( "%s", &w_t::%s )' % (arg.name, arg.name)
          for arg in args ] )


def _build_policies( args ):
    '''return
with_custodian_and_ward<1, 2,
with_custodian_and_ward<1, 5,
....
> > ()
'''
    types = ['int', 'float', 'double', 'char *', 'const char *']
    pointertypes = [ 'char *', 'const char *']
    
    indexes_of_args_need_wards = []
    for index, arg in enumerate(args):
        if arg.type not in types: raise NotImplementedError , \
           "type not supported: %s" % (arg.type, )
        if arg.type in pointertypes:
            indexes_of_args_need_wards.append( index + 2 )
            continue
        continue

    if len(indexes_of_args_need_wards) == 0: return ''
    
    ward = 'with_custodian_and_ward'
    policies = ',\n'.join(
        ['%s<1, %d' % (ward, index) for index in indexes_of_args_need_wards]) \
        + '> ' * len(indexes_of_args_need_wards) \
        + '()'
    return '[' + policies + ']'


def _ptr_members( members ):
    return filter( _ispointer, members )


def _build_ptrmethods( ptrs, klass ):
    ptrs = _ptrs_having_accessor_methods( ptrs )
    return '\n'.join( [ _build_ptrmethod( ptr, klass ) for ptr in ptrs ] )


def _build_ptrmethod( ptr, klass ):
    return 'WrappedPointer get%s( %s & obj ) { void *ptr = (void *)(obj.%s); WrappedPointer ret = {ptr}; return ret; }' % (
        ptr.name, klass.name, ptr.name )


def _build_expose_ptrmethods( ptrs ):
    ptrs = _ptrs_having_accessor_methods( ptrs )
    return '\n'.join( [ _build_expose_ptrmethod( ptr ) for ptr in ptrs ] )


def _build_expose_ptrmethod( ptr ):
    return '    .def( "get%s", get%s)' % (
        ptr.name, ptr.name )



def _ptrs_having_accessor_methods( ptrs ):
    """some pointers we don't know to build accessor methods, we need
    to filter thme out. this method return a list of
    pointers that can have accessor methods"""
    # pointer to pointer (double **, for example) is not yet handled
    return filter( lambda p: not p.type[-2:] == '**', ptrs)


# version
__id__ = "$Id$"


# End of file 
