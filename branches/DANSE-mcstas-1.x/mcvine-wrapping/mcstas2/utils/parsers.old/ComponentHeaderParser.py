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
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# modified by Alta Fang 

"""ComponentHeaderParser.py parses the header portion of mcstas component files."""

from pyparsing.pyparsing import *

class Header:
    """class Header holds the parsed information from the headers of mcstas 
       component files."""
    def __init__(self, componentname, copyright, simple_description,
                 full_description, input_parameters, output_parameters,
                 ):
        self.componentname = componentname
        self.copyright = copyright
        self.simple_description =simple_description
        self.full_description = full_description
        self.input_parameters = input_parameters
        self.output_parameters = output_parameters
        return

    pass # end of Header

def removeStars( text ):
    """Takes a string and returns it with the *'s removed from the beginning 
       of each line."""
    starlesstext = ""
    aline = Optional(Suppress("*") + restOfLine.setResultsName("restofline"))
    for l in text.splitlines():
        if l.find("*") != -1:
            starlesstext += aline.parseString( l ).restofline + "\n"
        else: 
            starlesstext += l + "\n"
    return starlesstext

def parse( header_text ):
    """Uses pyparsing to extract information from the header of a mcstas 
    component file. Takes a string of the general form: 
    
    * Component: componentname
    * 
    * %I
    * copyright information
    *
    * simple description
    * 
    * %D
    * full description
    * 
    * %P
    * INPUT PARAMETERS:
    * input parameters
    * 
    * OUTPUT PARAMETERS: 
    * output parameters
    * %E

    and returns a Header object with the parsed information."""

    # First, remove the stars
    starlesstext = removeStars( header_text )
    
    # Define what blocks of text are, for parsing later
    words = ZeroOrMore(Word(alphanums + """#!?\/_-()^[]<>@,.=$&+*":;\n'"""))
    textFormat = Combine(words, joinString=" ", adjacent=False)
    
    # Parse component name
    componentFormat = "Component:" + Word(alphanums + "_").setResultsName("componentname") \
                    + Optional("." + textFormat)
    try:
        component_name = componentFormat.searchString(starlesstext)[0].componentname
    except:
        component_name = None
    
    # Parse copyright and simple description into a string containing both, temporarily 
    copyrightDescriptionFormat = "%I" + SkipTo(lineEnd) \
                               + textFormat.setResultsName("copyrightDescription")
    copyrightDescriptionList = copyrightDescriptionFormat.searchString(starlesstext)
    copyrightAndDescription = copyrightDescriptionList[0].copyrightDescription
        
    # Define way to split strings that are separated by extra newlines: 
    def mustBeNonBlank(s,l,t):
        if not t[0]:
            raise ParseException(s,l,"empty line body")
    lineBody = SkipTo(lineEnd).setParseAction(mustBeNonBlank)
    textLine = lineBody + Suppress(lineEnd).setParseAction(replaceWith("\n"))
    para = OneOrMore(textLine) + Suppress(lineEnd)

    # Split copyright and simple description, if applicable
    split = para.searchString(copyrightAndDescription)
    copyright = "".join(split[0])
    try:
        part2 = split[1]
    except IndexError:
        msg = "mal-formed 'simple description' for component %r" % \
            component_name
        msg += 'removing trailing white spaces may solve the problem'
        raise RuntimeError, msg
    simple_description = part2[0]

    # Parse full description
    descriptionInfo = "%D" + SkipTo(lineEnd) + textFormat.setResultsName("full_description")
    tmp_tokens = descriptionInfo.searchString(starlesstext)
    try:
        tmp_token0 = tmp_tokens[0]
    except IndexError:
        raise RuntimeError, ' '.join([
                'failed to find description. please add \%D section ',
                'to header of component %s' % component_name
                ])
    full_description = tmp_token0.full_description

    # Parse parameters
    parameterInfo = "%P" + SkipTo(lineEnd) + textFormat.setResultsName("parameters")  
    parsedParams = parameterInfo.searchString(starlesstext)[0]
    parameterBlock = parsedParams.parameters 
    
    # Define way to split parameters into input and output
    # parameterBlock is of the form:
    # INPUT PARAMETERS:
    # input parameters here
    # OPTIONAL PARAMETERS: <-- optional
    # optional parameters here <-- optional
    # OUTPUT PARAMETERS:
    # output parameters here
    input_ident = CaselessLiteral("INPUT PARAMETERS") + Optional(":")
    output_ident = CaselessLiteral("OUTPUT PARAMETERS") + Optional(":")
    optional_ident = CaselessLiteral("OPTIONAL PARAMETERS") + Optional(":")
    end_input = output_ident ^ optional_ident 
    paramWord = Word(alphanums + """ #?!*\/_-()[]^<>@+,.=$&":'""")
    param_block = Group(ZeroOrMore(~output_ident + paramWord))
    total_params = Optional(input_ident) + Group(ZeroOrMore(~end_input \
                  + paramWord)).setResultsName("input_parameters") + \
                  Optional(optional_ident + param_block) + Optional(output_ident \
                  + textFormat.setResultsName("output_parameters")) + StringEnd()
    splitParams = total_params.searchString(parameterBlock)[0]  

    # Put the input and output parameters into dictionaries.  
    # parameters passed to makeDictionary() are strings of the form:
    # name : value \n
    input_parameters = makeDictionary("\n".join(splitParams.input_parameters))
    if splitParams.output_parameters != None:
        output_parameters = makeDictionary(splitParams.output_parameters)   
    else:
        output_parameters = None

    # Finally, return a Header object containing the parsed information
    return Header(component_name, copyright, simple_description,  \
                 full_description, input_parameters, output_parameters)

def makeDictionary(text):
    """ Takes a string of parameters of the form 'name: value\n'
    and returns a dictionary where the names are keys to the values."""
    d = {}
    valueFormat = Word(alphanums + """!?\/_-()[]<>+@,.=:$&'"^ """) # colons ok
    nameFormat = Word(alphanums + """!?\/_-()[]<>+@,.=$&'"^ """) # no colons
    valueBlock = Combine(ZeroOrMore(valueFormat))
    nameBlock = Combine(ZeroOrMore(nameFormat))
    aline = nameBlock.setResultsName("name") + Literal(":") \
            + valueBlock.setResultsName("value") + LineEnd()
    # The value may go on to the next line(s)
    aUnit = aline + Optional(Combine(Combine(ZeroOrMore(nameFormat), joinString=" ", \
            adjacent=False).setResultsName("valueContinued") + LineEnd()))
    parsedParams = aUnit.searchString(text)
    for unit in parsedParams:
        d[unit.name] = unit.value + " " + unit.valueContinued
    return d

# Sample header to test the parse function
testtext = """
/*******************************************************************************
*
*
*
* McStas, neutron ray-tracing package
*         Copyright 1997-2002, All rights reserved
*         Risoe National Laboratory, Roskilde, Denmark
*         Institut Laue Langevin, Grenoble, France
*
* Component: E_monitor
* 
* %I
* Written by: Kristian Nielsen and Kim Lefmann
* Date: April 20, 1998
* Version: $Revision: 438 $
* Origin: Risoe
* Release: McStas 1.6
*
* Energy-sensitive monitor.
*
* %D
* A square single monitor that measures the energy of the incoming neutrons.
*
* Example: E_monitor(xmin=-0.1, xmax=0.1, ymin=-0.1, ymax=0.1, 
*                 Emin=1, Emax=50, nchan=20, filename="Output.nrj")
*
* %P
* INPUT PARAMETERS:
*
* xmin:     Lower x bound of detector opening (m)
* xmax:     Upper x bound of detector opening (m)
* ymin:     Lower y bound of detector opening (m)
* ymax:     Upper y bound of detector opening (m)
* Emin:     Minimum energy to detect (meV)
* Emax:     Maximum energy to detect (meV)
* nchan:    Number of energy channels (1)
* filename: Name of file in which to store the detector image (text)
*
* OUTPUT PARAMETERS:
*
* E_N:      Array of neutron counts
* E_p:      Array of neutron weight counts
* E_p2:     Array of second moments
*
* %E
*******************************************************************************/
"""

# Test the parsing
if __name__ == "__main__":
    results = parse(testtext)
    print "component:", results.componentname
    print "copyright:", results.copyright
    print "simple description:", results.simple_description
    print "full description:", results.full_description
    print "input parameters:", results.input_parameters
    print "output parameters:", results.output_parameters
    
