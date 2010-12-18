#!/usr/bin/env python
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#                                   Jiao Lin
#                      California Institute of Technology
#                        (C) 2005  All Rights Reserved
#
# {LicenseText}
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#


from CxxClassMillBase import CxxClassMillBase


class CCMill(CxxClassMillBase):

    # handlers

    def onClass(self, klass):

        self._klassName = klass.name

        self._write('')
        self._write("#include \"%s.h\"" % klass.name)
        self._write('')

        for line in klass.helpers_implementation: self._write( line )
        self._write('')

        if klass.namespace :
            # DANSE::simulation --> DANSE, simulation
            namespaces = klass.namespace.split('::') 
            self._write( ''.join([ "namespace %s {" % namespace for namespace in namespaces]))
            self._indent()
            pass
        
        for method in klass.public_methods: method.identify(self)
        for method in klass.private_methods: method.identify(self)

        if klass.namespace:
            self._write('')
            self._outdent()
            closings = ['} /* namespace %s */'% namespace for namespace in namespaces]
            closings.reverse()
            self._write( ''.join(closings) )
            pass

        self._klassName = None
        return

    
    def onMethod(self, method):
        self._write( method.type )
        signature = "%s::%s" % ( self._klassName, method.name ) 
        self._write( signature )
        self._write( "(%s)" % _build_args_str( method.args ) )
        self._write( '{' )
        self._indent()
        for l in method.body: self._write( l )
        self._outdent()
        self._write( '}' )
        self._write( '' )
        return

    pass # end of CCMill


def _build_args_str( args ):
    return ",".join( [ _arg_str( arg ) for arg in args] )


def _arg_str( arg ):
    return "%s %s" % (arg.type, arg.name)


def test():
    from Class import example
    klass = example()

    from pyre.applications.Script import Script
    class App(Script):

        def main(self):
            weaver = self.weaver
            weaver.renderer = CCMill()
            r = weaver.render( klass )
            print "\n".join(r)
            return

        pass # end of App

    app = App('app')
    app.run()
    return


if __name__ == "__main__": test()


# version
__id__ = "$Id$"

# End of file 
