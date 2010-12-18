#!/usr/bin/env python
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#                            Jiao Lin, Alex Dementsov
#                      California Institute of Technology
#                        (C) 2007  All Rights Reserved
#
# {LicenseText}
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#


## Parsers to parse Riseo McStas files such as
##  - McStas component file

from ComponentInfo import ComponentInfo, Parameter
from McStasComponentParser import McStasComponentParser

def parseComponent( component_file ):
    "Parses component"
    parser      = McStasComponentParser(filename=component_file)
    header      = parser.header()
    defs        = parser.definitions()
    sections    = parser.sections()

    name        = defs["name"]
    assert header["componentname"] == defs["name"]

    inputParamDescs         = header["input_parameters"]
    outputParamDescs        = header["output_parameters"]
    definition_parameters   = _addDescription( defs["definition_parameters"], inputParamDescs )
    setting_parameters      = _addDescription( defs["setting_parameters"], inputParamDescs )
    name_parameter          = Parameter( 'name', 'char *', name.lower(), 'component name' )
    input_parameters        = [name_parameter] + definition_parameters + setting_parameters
    output_parameters       = _addDescription( defs["output_parameters"], outputParamDescs )
    state_parameters        = [ str(p) for p in defs["state_parameters"] ]

    # the original McStas component does not need a "name" argument, but
    # the auto-generated c++ class and python component need a "name"
    # argument. So we better add this to the description, which will
    # become the documentation.
    full_description    = header["full_description"].replace(name+'(', name+'(name, ')
    share               = _format_share_str(sections["share"])

    return ComponentInfo(
                        name,
                        header["copyright"],
                        header["simple_description"],
                        full_description,
                        input_parameters,
                        output_parameters,
                        state_parameters,
                        '%s' % sections["declare"],
                        '%s' % sections["initialize"],
                        '%s' % sections["trace"],
                        '%s' % sections["save"],
                        '%s' % sections["finalize"],
                        share)


def _format_share_str( share ):
    from ShareIncludeParser import include
    lines = share.split( '\n' )
    if len(lines) == 0: return '', ''
    if len(lines) == 1: return '', ''
    #lines = lines[1:-1] # strip { and } # Don't need it

    start_of_header = 0
    start_of_implementation = -1

    # go through all lines and do the follinwg
    # 1. find '%include' and replace that with correct c syntax
    # 2. find section separators for header (.h) and implementation (.c) sections
    token = '%include'
    for i, line in enumerate(lines):
        line = line.strip()
        if line.startswith( token ):
            header = include().parseString( line ).header
            # If header is a quoted string, remove the quotes
            if header[0] == '"':
                header = eval(header)
            line = r'#include "mcstas2/share/%s.h"' % header
            lines[ i ] = line
            pass
        if line == header_start_signature : start_of_header = i
        elif line == implementation_start_signature: start_of_implementation = i
        continue

    if start_of_implementation == -1:
        # did not find separator
        raise RuntimeError, "invalid component share section. No separator to separate implementation code from header code: %s" % share

    return ('\n'.join( lines[:start_of_implementation] ),
            '\n'.join( lines[start_of_implementation:] ) )
header_start_signature = '// ----------  header for SHARE ----------'
implementation_start_signature = '// ----------  implementation for SHARE ----------'


def _addDescription( parameters, descriptions ):
    ret = []
    for param in parameters:
        name    = param["name"]
        d       = descriptions.get(name) or ''
        type    = param["type"]
        value   = param["value"]
        p       = Parameter( name, type, value, d )
        ret.append( p )

    return ret


__date__ = "$Sep 15, 2010 3:07:32 PM$"


