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


class HHMill(CxxClassMillBase):

    # handlers

    def onClass(self, klass):
        self._write('')

        if len(klass.headers_dependent_on):
            for header in klass.headers_dependent_on:
                self._write( "#include \"%s\""% header)
                continue
            pass

        self._write('')
        for line in klass.helpers_header: self._write( line )
        self._write('')

        if klass.namespace:
            # DANSE::simulation --> DANSE, simulation
            namespaces = klass.namespace.split('::') 
            self._write( ''.join([ "namespace %s {" % namespace for namespace in namespaces]))
            self._indent()
            pass
        
        parentsRep = ','.join( [ "public %s" % parent.name for parent in klass.parents ] )
        
        if len(klass.parents) == 0: line1 = 'class %s {' % klass.name
        else : line1 = 'class %s: %s {' % (klass.name, parentsRep)

        self._write( "" )
        self._write( line1 )

        self._write('public:')
        
        self._indent()
        for item in klass.public: self._write( item );
        for method in klass.public_methods: method.identify(self)
        for member in klass.public_members: member.identify(self)
        self._outdent()
        
        self._write('private:')
        self._indent()
        for item in klass.private: self._write( item );
        for method in klass.private_methods: method.identify(self)
        for member in klass.private_members: member.identify(self)
        self._outdent()
        
        self._write('}; // class %s' % klass.name)

        if klass.namespace:
            self._write('')
            self._outdent()
            closings = ['} /* namespace %s */'% namespace for namespace in namespaces]
            closings.reverse()
            self._write( ''.join(closings) )
            pass
        return

    
    def onMethod(self, method):
        if method.type is None: l = "%s(%s);" % (method.name, _build_args_str( method.args ) )
        else:
            l = "%s %s(%s);" % (method.type, method.name, _build_args_str( method.args ) )
            pass
        self._write( l )
        return


    def onMember(self, member):
        l = "%s %s;" % (member.type, member.name)
        self._write( l )

    pass # end of HHMill


def _build_args_str( args ):
    return ",".join( [ _arg_str( arg ) for arg in args] )


def _arg_str( arg ):
    if arg.default is None:
        return "%s %s" % (arg.type, arg.name)
    elif isinstance( arg.default, basestring ):
        return "%s %s=\"%s\"" % (arg.type, arg.name, arg.default)
    else:
        return "%s %s=%s" % (arg.type, arg.name, arg.default)
    pass


def test():
    from Class import example
    klass = example()
    
    from pyre.applications.Script import Script
    class App(Script):

        def main(self):
            weaver = self.weaver
            weaver.renderer = HHMill()
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
