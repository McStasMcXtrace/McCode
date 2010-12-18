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


## Generate a pyre component class for a non-pyre class.
## The non-pyre class is elementary, meaning it is not a composite.
## Its ctor only takes simple argument types like int, float, string.
## The ctor must take kwds.
## The non-pyre class has an attribute "arguments" which is a list
## of Argument instances.
## see the test at the end of this module for a better idea of what
## we are aiming for.

class Argument:

    def __init__(self, name='name', type='', default='', description = '' ):
        self.name = name
        self.type = type
        self.default = default
        self.description = description
        
    pass # end of Parameter



class Generator:

    def __init__(self, ctor_takes_name = True, baseclass = None):
        '''
        baseclass: base component class that the new component class will derive from
        ctor_takes_name: the ctor of the class to wrap take "name" as an argument or not.
        '''
        if baseclass is None:
            from pyre.components.Component import Component
            baseclass = Component
            pass
        baseclassmodule = baseclass.__module__
        self.baseclassmodule = __import__( baseclassmodule, {}, {}, [''] )
        self.baseclass = baseclass
        self.ctor_takes_name = ctor_takes_name
        return

    def __call__(self, klass):
        arguments = klass.arguments
        base = self.baseclass
        class _( base ):

            Engine = klass
            __doc__ = klass.__doc__
            
            class Inventory(base.Inventory):
                import pyre.inventory as pinv
                for arg in arguments:
                    exec _trait_str( arg ) in locals()
                    continue
                del arg
                pass

            
            def processM(self, neutrons):
                # for mcstas components, processM = process
                return self.process(neutrons)

            def __getattribute__(self, name):
                try: return base.__getattribute__(self, name)
                except AttributeError:
                    import traceback
                    import journal
                    jrnltag = 'ElementaryComponentGenerator'
                    debug = journal.debug( jrnltag )
                    debug.log( traceback.format_exc() )
                    engine = self.__dict__.get( 'engine' )
                    if engine is None:
                        raise RuntimeError, "engine not established"
                    return getattr( engine, name )
                raise RuntimeError , "Should not reach here"

            def _init(self):
                base._init(self)
                if self._showHelpOnly: return
                kwds = self._argumentsFromInventory()
                self.engine = klass( **kwds )
                return

            def _fini(self):
                base._fini(self)
                engine = self.__dict__.get('engine')
                if engine: del self.engine
                return

            pass # end of _


        if self.ctor_takes_name:

            def _argumentsFromInventory(self):
                kwds = {}
                for arg in self.arguments:
                    name = arg.name
                    kwds[name] = eval( 'self.inventory.%s' % name )
                    continue
                kwds['name'] = self.name
                return kwds

        else:

            def _argumentsFromInventory(self):
                kwds = {}
                for arg in self.arguments:
                    name = arg.name
                    kwds[name] = eval( 'self.inventory.%s' % name )
                    continue
                return kwds

        _._argumentsFromInventory = _argumentsFromInventory

        _.arguments = arguments
        _.full_description = klass.info.full_description
        _.simple_description = klass.info.simple_description
        
        return _

    pass # end of %(newclass)s


def _trait_str( arg ):
    typestr = _trait_type_str( arg.type )
    indent = ''
    line1 = indent+ '%s = pinv.%s( "%s", default = %r )' % (
        arg.name, typestr, arg.name, arg.default )
    lines = [line1]
    try:
        description = arg.description
        lines.append( indent + '%s.meta["tip"] = %r' % (arg.name, description) )
    except Exception, msg:
        pass
    return '\n'.join( lines )

_translation = {
    'const char *': 'str',
    'char *': 'str',
    'float': 'float',
    'double': 'float',
    'int': 'int',
    }
def _trait_type_str( type ):
    return _translation[ type ]




def test():

    # a simple python class that does some boring math
    class T:
        #ctor
        def __init__(self, a=1, b=3):
            self.a = a
            self.b = b
            return
        #main computation interface
        def calc(self):
            return self.a*self.b
        #parameters
        arguments = [
            Argument( 'a', 'float', 2 ),
            Argument( 'b', 'float', 5 ),
            ]
        pass # end of T

    # pyre component class generator
    generator = Generator( ctor_takes_name = False )

    # generate pyre component class
    TC = generator( T )

    # instantiate pyre component
    tc = TC( 'tc', 'tc' )

    # make sure inventory was built correctly
    assert tc.inventory.a == tc.arguments[0].default
    assert tc.inventory.b == tc.arguments[1].default

    # set inputs for pyre component
    tc.inventory.a = 3 
    tc.inventory.b = 4

    # try to run the pyre component
    tc._init()
    assert tc.calc(  ) == 12
    tc._fini()
    return


if __name__ == '__main__': test()

# version
__id__ = "$Id$"

# End of file 
