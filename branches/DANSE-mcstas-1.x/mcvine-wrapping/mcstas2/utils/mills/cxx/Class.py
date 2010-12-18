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


class Class:

    def __init__(self, name, namespace = None,
                 headers_dependent_on = [],
                 parents = [],
                 public = [], private = [], 
                 public_methods = [], public_members = [],
                 private_methods = [], private_members = [],
                 # this is not really a good structure. but we are in a hurry right now...
                 helpers_header = [], helpers_implementation = [],
                 ):
        self.name = name
        self.namespace = namespace
        self.headers_dependent_on = headers_dependent_on
        self.parents = parents
        self.public = public
        self.private = private
        self.public_methods = public_methods
        self.public_members = public_members
        self.private_methods = private_methods
        self.private_members = private_members
        self.helpers_header = helpers_header
        self.helpers_implementation = helpers_implementation
        return


    def constructors(self):
        ctorname = self.name
        methods = self.public_methods
        return filter( lambda x: x.name == ctorname, methods )
    

    def destructors(self):
        dtorname = '~%s' % self.name
        methods = self.public_methods
        return filter( lambda x: x.name == dtorname, methods )
    

    def identify(self, visitor): return visitor.onClass(self)

    pass # end of Class


from Method import Method
from Member import Member
from Argument import Argument


def argument2Member( arg ):
    return Member( arg.type, arg.name )


def example():
    name = "Hello"
    ctor = Method( "Hello",
                   [Argument("double", "dummy"),
                    Argument("double", "x", 1.0),
                    Argument("int", "j", 1),
                    Argument("char *", "name", "hee"),
                    Argument("char *", "password", 0),
                    ],
                   ["std::cout << x << std::endl;",
                    "m_x = x;",
                    "m_arr = new double[10];"])
    
    dtor = Method( "~Hello", [],
                   ["delete [] m_arr;"])

    call = Method( "operator()", [Argument("char *", "someone")],
                   ['''std::cout << "hello " << someone << std::endl;'''],
                   type = "void")

    m_x = Member( "double", "m_x" )
    m_arr = Member( "double *", "m_arr" )

    parentClass = Class( 'Parent' )
    
    klass = Class( name, public_methods = [ctor, dtor, call], private_members = [m_x, m_arr],
                   namespace = "Greeting::Goodwill",
                   headers_dependent_on = ["iostream"],
                   parents = [parentClass] )
    assert klass.constructors() == [ctor]
    return klass


if __name__ == '__main__': example()

# version
__id__ = "$Id$"

# End of file 
