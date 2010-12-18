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

#Generate <component_name>.py, which contains a "factory" method
#to create instance of the boost python binding of mcstas component 

template = '''
def factory( %(ctor_kwds)s ):
    from mcstas2.bindings import boostpython
    from mcstas2.mcstas2bp import McStasComponentAsMcniComponent as component
    from %(bindingmodulename)s import %(component)s as f
    return component( f( %(ctor_args)s ) )

from mcstas2.utils.parsers.ComponentInfo import ComponentInfo, Parameter
%(component_info)s

factory.info = info
factory.__doc__ = str(info)
''' 

def generate( compinfo, bindingmodulename, path ):
    ctorargs = compinfo.input_parameters
    compname = compinfo.name
    ctorargs_str = _build_args_str( ctorargs )
    ctorkwds_str = _build_kwds_str( ctorargs )
    content = template % {
        'ctor_args': ctorargs_str,
        'ctor_kwds': ctorkwds_str,
        'component': compname,
        'bindingmodulename': bindingmodulename,
        'component_info': inst2str( 'info', compinfo ),
        }
    import os
    filename = os.path.join( path, "%s.py" % compname)
    open(filename, 'w').write( content )
    return filename



class struct :
    def __str__(self):
        attrs = dir(self)
        attrs = filter( lambda a: not a.startswith('_'), attrs )
        return '(%s)' % (
            ', '.join( [ '%s=%r' % (attr, getattr(self, attr)) for attr in attrs] ), )

    __repr__ = __str__
    
def inst2str( name, inst ):
    """return a string of python commands to reconstruct inst

for example:
    class A:
        def __init__(self, name): self.name = name
    a = A( 'a' )

inst2str( 'a', a ) -->

'''
a = struct()
a.name = 'a'
'''
"""
    ret = []
    if isinstance(inst, int) or isinstance(inst, float):
        ret.append( '%s = %r' % (name, inst) )
    elif isinstance(inst, basestring ):
        ret.append( '%s = """%s"""' % (name, inst) )
    elif isinstance(inst, list) or isinstance(inst, tuple):
        for index, item in enumerate(inst):
            ret.append( inst2str( 'item%s'%index, item ) )
            continue
        ret.append( '%s = [ %s ]' % ( name, 
            ', '.join( ['item%s' % i for i in range(len(inst))] ) ) )
    else:
        ret = ['%s = %s()' % (name, inst.__class__.__name__) ]
        attrs = dir(inst)

        for attr in attrs:
            if attr.startswith('_'): continue
        
            value = getattr(inst, attr)
            tempname = '%s_%s' % (name, attr)
            ret.append( inst2str( tempname, value ) )
            ret.append( '%s.%s = %s' % (name, attr, tempname) )
            continue
        pass
    return '\n'.join(ret)


def _build_kwds_str( args ):
    return ",".join( [ _kwd_str( arg ) for arg in args] )
def _kwd_str( arg ):
    return "%s=%r" % (arg.name, arg.default)

def _build_args_str( args ):
    return ",".join( [ _arg_str( arg ) for arg in args] )
def _arg_str( arg ):
    return "%s" % (arg.name, )



def test_inst2str( ):
    a = struct()
    a.a = 1
    a.b = 'hello'
    a.c = struct()
    a.c.a = 1
    d = struct()
    d.d = struct()
    a.d = [ d ]
    print inst2str( 'a', a )
    return


def main():
    test_inst2str()
    return


if __name__ == '__main__': main()


# version
__id__ = "$Id$"


# End of file 
