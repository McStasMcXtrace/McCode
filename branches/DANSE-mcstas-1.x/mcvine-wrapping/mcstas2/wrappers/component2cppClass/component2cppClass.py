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


## This module converts component info parsed from a McStas component
## and create a c++ class for that component.
## Please run corresponding test in the "tests" directory to see
## what does it do.

def component2HHandCC( component_filename, pathToSave ):
    from mcstas2.utils.mills.cxx.factory import createHHandCC
    return createHHandCC( component2cppClass( component_filename ), pathToSave )


def component2cppClass( comp_filename ):
    #parse the mcstas component and get infos we want
    from mcstas2.utils.parsers import parseComponent
    compInfo = parseComponent( comp_filename )
    return componentInfo2cppClass( compInfo )


def componentInfo2cppClass( compInfo ):
    #massage those info and add some additional info and make the class
    class_name = compInfo.name
    ctor_args = _arguments(compInfo.input_parameters) 
    output_params = compInfo.output_parameters
    trace_method_args = compInfo.state_parameters

    # additional member declaration is declared in DECLARE section
    additional_member_declaration = compInfo.declare[1:-1]
    additional_members = _parse( additional_member_declaration )
    # some of them are in the "output parameters" section
    # and should be declared as public
    output_parameter_names = [p.name for p in output_params]
    additional_public_members = [
        t for t in additional_members 
        if t.name in output_parameter_names
        ]
    # and others should be declared as private
    additional_public_member_names = [t.name for t in additional_public_members]
    additional_private_members = [
        m for m in additional_members 
        if m.name not in additional_public_member_names
        ]

    #
    ctor_body = compInfo.initialize
    trace_method_body = compInfo.trace
    save_method_body = compInfo.save
    finalize_method_body = compInfo.finalize

    helpers_h, helpers_cc = compInfo.share
    helpers_h = helpers_h.split( '\n' )
    helpers_cc = helpers_cc.split( '\n' )

    ##     print class_name
    ##     for arg in ctor_args: print arg
    ##     print additional_member_declaration
    ##     print ctor_body
    ##     print finalize_body

    ##     print compInfo.header
    ##     print compInfo.save
    namespace = "mcstas2"
    baseclass = Class( 'Component' )
    headers_dependent_on = ['mcstas2/mcstas2.h']
    return createCppClass( class_name,
                           namespace, baseclass,
                           ctor_args, ctor_body,
                           #additional_member_declaration,
                           additional_public_members,
                           additional_private_members,
                           trace_method_args,
                           trace_method_body,
                           save_method_body,
                           finalize_method_body,
                           headers_dependent_on,
                           helpers_h, helpers_cc,
                           )


from mcstas2.utils.mills.cxx.Class import Argument, Method, Member, Class, argument2Member

def createCppClass( name,
                    namespace, baseclass,
                    ctor_args, ctor_body,
                    #additional_member_declaration,
                    additional_public_members,
                    additional_private_members,
                    trace_method_args,
                    trace_method_body,
                    save_method_body,
                    finalize_body,
                    headers_dependent_on,
                    helpers_h, helpers_cc):

    #ctor arguments become private members.
    # E_monitor( int nchan ) --> E_mointor( int in_nchan ) { nchan = in_nchan; }
    #
    # 'name' is not a private member. it is a member of base class.
    name_arg = ctor_args[0]
    assert name_arg.name == 'name'
    args = ctor_args[1:]
    # 
    members = [ argument2Member(arg) for arg in args ]

    # meta-methods
    #   argument "name" is necessary for the component c++ class.
    #   its default would be the lower
    #   case conversion of component name
    ctor_body_name_assignment = 'setName( name );'
    
    #   other ctor arguments 
    ctor_args = [
        Argument( arg.type,  "in_%s" % arg.name, arg.default ) for arg in args ]
    #
    #   transfer inputs to private members
    ctor_getInputs = [ "%s = %s;" % (member.name, arg.name) for member, arg in \
                       zip( members, ctor_args ) ]
    #   ctor body
    ctor_body = ctor_body.split("\n")
    ctor_body = [ctor_body_name_assignment] + ctor_getInputs + ctor_body

    #   add name arg back to the ctor arg list
    ctor_args = [ name_arg ] + ctor_args
    ctor = Method( name, ctor_args, ctor_body )

    # dtor
    dtor = Method( '~'+name, [], ['save();', 'finalize();'] )
    
    #   finalize
    finalize = Method( 'finalize', [], finalize_body.split("\n"), type = 'void' )

    # methods
    trace_body = trace_method_body.split('\n')
    trace_arguments = [ Argument( "double &", arg ) for arg in trace_method_args ]
    trace = Method( "trace", trace_arguments, trace_body, type = "void" )

    save_body = save_method_body.split( '\n' )
    save_body = [ 'mcuse_format("McStas");' ] + save_body # need to call mcuse_format before calling detector output methods
    save = Method( 'save', [], save_body, type = 'void' )
    
    methods = [ctor, dtor, trace, save, finalize, ]

    # data
    #private = additional_member_declaration.split("\n")

    # the class
    klass = Class(
        name,
        namespace = namespace,
        parents = [ baseclass ],
        public_methods = methods,
        #private = private,
        public_members = members + additional_public_members,
        private_members = additional_private_members,
        headers_dependent_on = headers_dependent_on,
        helpers_header = helpers_h, helpers_implementation = helpers_cc,
        )
    
    return klass



def _argument( param ):
    """Create an argument.
    input: parsed mcstas parameter. 
    note: mcstas parameters without type are doubles
    """
    return Argument( param.type, param.name, param.default )


def _arguments( params ):
    if params == "": return []
    return [ _argument(param) for param in params ]



def _parse( declarations ):
    declarations = declarations.split( ';' )
    members = []
    for declaration in declarations:
        members += _parse_declaration( declaration.strip() )
        continue
    return members


def _parse_declaration( declaration ):
    if len(declaration) == 0: return []
    n1 = declaration.find( ' ' )
    if n1 == -1: 
        raise RuntimeError , "%r is not a declaration" % declaration
    typestr = declaration[:n1].strip();
    vars = declaration[n1+1:].split( ',' )
    members = []

    from mcstas2.utils.mills.cxx.Member import Member
    for var in vars:
        var = var.strip()
        if var.startswith( '*' ):
            stars = var[ : var.rfind( '*' ) + 1 ]
            for star in stars: assert star == '*', "Don't know how to parse %r" % var
            type = typestr + stars
            var = var[len(stars):].strip()
        else:
            type = typestr
            pass
        member = Member( type, var )
        members.append( member )
        continue
    return members

# version
__id__ = "$Id$"

# End of file 
