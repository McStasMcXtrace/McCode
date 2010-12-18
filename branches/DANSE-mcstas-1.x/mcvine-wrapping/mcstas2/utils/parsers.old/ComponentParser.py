__doc__="""
Parses original McStas instrument definition files. 
"""
__author__="Jiao Lin"

from pyparsing.pyparsing import *

# numbers: 1, 30.0, 1e-5, -99, for parsing parameter values
number = Combine( Optional('-') + ( '0' | Word('123456789',nums) ) + \
                  Optional( '.' + Word(nums) ) + \
                  Optional( Word('eE',exact=1) + Word(nums+'+-',nums) ) )

def convertNumbers(s,l,toks):
    n = toks[0]
    try: return int(n)
    except ValueError, ve: return float(n)
    raise

number.setParseAction( convertNumbers )

# Define strings for parsing parameter values.
# Evaluate the parsed parameter value into a string, and if the parameter value
# is a quoted string, remove the quotes. For parsing parameters like:
# string name = value, as well as string name="value"
string = quotedString ^ Word(alphanums + "_-.")

def convertString(s,l,toks):
    n = toks[0]
    try:
        toreturn = eval(n)
    except:
        toreturn = str(n)
    return toreturn

string.setParseAction( convertString )

# a, b, chop_phase.  instrument parameter of declared variable
identifier = Word( alphas, alphanums + "_" )   
global_var_name = identifier.setResultsName( "global_var_name")

comment = Suppress(ZeroOrMore( cppStyleComment ))
c_comment = Suppress(ZeroOrMore( cStyleComment ))

def header():
    return Optional(cStyleComment.setResultsName("header"))


def define():
    comp_name = identifier
    d = Suppress("DEFINE") + Suppress("COMPONENT") + comp_name.setResultsName( \
        "name") + Optional(comment)
    return d


# Format for parameters is:
#     PARAMETERS ( [type] [name] = [value], [type] [name] = [value], etc. )
# where [type] and = [value] are optional.
# [type] is either "double", "int", "char *", or "string"
# [name] is any combination of alphabetic letters and _'s
# value is either a number or a string (with or without quotes)
def def_parms():
    parameter_type = Optional(oneOf( ["double", "int", "char *", "string"] ))
    parameter_type = parameter_type.setResultsName( "type" )  # double, int
    parameter_name = identifier.setResultsName( "name" )
    parameter_value = (number|string).setResultsName( "value" ) 
    parameter_declr = Group( parameter_type + parameter_name + Optional( Suppress('=') + parameter_value ) ).setResultsName( "parameter" ) 
    parameter_list = delimitedList( parameter_declr )
    d = Suppress("DEFINITION") + Suppress("PARAMETERS") \
        + Suppress("(") \
        + Optional(parameter_list.setResultsName("definition_parameters")) \
        + Suppress(")")  + Optional(comment)
    return d


def set_parms():
    parameter_type = Optional(oneOf( ["double", "string", "int", "char *"] )).setResultsName( \
                     "type" )  # double, int
    parameter_name = identifier.setResultsName( "name" )
    parameter_value = (number|string).setResultsName( "value" )
    parameter_declr = Group( parameter_type + parameter_name + Optional( Suppress('=') + parameter_value) ).setResultsName( "parameter" )
    parameter_list = delimitedList( parameter_declr ) 
    d = Suppress("SETTING") + Suppress("PARAMETERS") \
        + Suppress("(") + Optional( parameter_list.setResultsName("setting_parameters") )\
        + Suppress(")")  + Optional(comment)
    return d


def output_parms():
    parameter_name = identifier
    parameter = Group( parameter_name.setResultsName('name') ).setResultsName('parameter')
    parameter_list = delimitedList( parameter )
    d = Suppress("OUTPUT") + Suppress("PARAMETERS") \
        + Suppress("(") + Optional(parameter_list.setResultsName("output_parameters") ) \
        + Suppress(")")  + Optional(comment)
    return d


def state_parms():
    parameter_name = identifier
    parameter_list = delimitedList( parameter_name )
    d = Suppress("STATE") + Suppress("PARAMETERS") \
        + Suppress("(") \
        + Optional(parameter_list.setResultsName("state_parameters") ) \
        + Suppress(")")  + Optional(comment)
    return d

def polarisation_params():
    return CaselessLiteral("POLARISATION PARAMETERS") + "(" + \
           OneOrMore(Word(alphanums + "=, ")) + ")"
    
# Define that a block of text is anything between %{ and %}, and replace the
# %{ with { and the %} with }
block = Regex(r"%{[\s\S]*?%}")
def convertBlock(s,l,toks):
    b = toks[0]
    b = b[1:] # %{ --> {
    b = b[:-2]; b += "}"  # %} --> }
    return b
block.setParseAction( convertBlock )

# Define another way to parse a block of text, so comments are stripped. Replaces
# %{ with { and %} with }
# Used in the DECLARE block 
words = Word(alphanums + """%#/*{}\_-()^[]!?<>@,.=$&+":;'""")
noComments = Combine(ZeroOrMore(~Literal("%}") + Optional(cppStyleComment).suppress() \
            + words + Optional(cppStyleComment).suppress()), joinString=" ", adjacent=False)
startBlock = Literal("%{").setParseAction(replaceWith("{"))
endBlock = Literal("%}").setParseAction(replaceWith("}"))
textBlock = Combine(startBlock + noComments + endBlock, joinString=" ", adjacent=False)

def declare(): return Suppress("DECLARE") + textBlock.setResultsName("declare")


def initialize(): return Suppress("INITIALIZE") + block.setResultsName( "initialize" ) 


def trace(): return Suppress("TRACE") + block.setResultsName("trace") 


def save(): return Suppress("SAVE") + block.setResultsName("save") 


def finalize(): return Suppress("FINALLY") + block.setResultsName("finalize")


def share(): return Suppress('SHARE') + block.setResultsName('share')

def component():
    return header() \
           + Optional( c_comment ) \
           + define() \
           + def_parms() \
           + set_parms() \
           + Optional(output_parms() ) \
           + state_parms() \
           + Optional(polarisation_params()) \
           + Optional(share()) \
           + Optional(declare()) \
           + Optional(initialize()) \
           + trace() \
           + Optional(save()) \
           + Optional(finalize()) 

def test_define():
    print "test parsing define block...",
    d = define()
    parsed = d.parseString( "DEFINE COMPONENT E_monitor" )
    print parsed
    assert parsed[0] == "E_monitor" 
    return


def test_def_parms():
    print "test parsing definition parmeters block...",
    d = def_parms()
    s = """ DEFINITION PARAMETERS (int nchan=20, char *filename="e.dat", xmin=-0.2)"""
    parsed = d.parseString( s )
    print parsed
    expected = [ ["int", "nchan", 20],
                 ["char *", "filename", "e.dat"],
                 ["", "xmin", -0.2] ]
    
    for e, parm in zip(expected, parsed):
        assert e[0] == parm.type, "%s is not equal to %s" % (e[0], parm.type)
        assert e[1] == parm.name, "%s is not equal to %s" % (e[1], parm.name)
        assert e[2] == parm.value, "%s is not equal to %s" % (e[2], parm.value)
        continue

    s = """ DEFINITION PARAMETERS ()"""
    parsed = d.parseString( s )
    print parsed
    return
    

def test_set_parms():
    print "test parsing setting parmeters block...",
    d = set_parms()
    s = """ SETTING PARAMETERS (int nchan=20, char *filename="e.dat", xmin=-0.2)"""
    parsed = d.parseString( s )
    print parsed
    for parm in parsed: print "%s %s = %s" % (parm.type, parm.name, parm.value)
    return
    

def test_output_parms():
    print "test parsing output parmeters block...",
    d = output_parms()
    s = """ OUTPUT PARAMETERS (E_N, E_p, E_p2)"""
    parsed = d.parseString( s )
    print parsed
    expected = ["E_N", "E_p", "E_p2"]
    
    for e,parm in zip(expected, parsed):  assert e == parm

    #empty parameter list
    s = """OUTPUT PARAMETERS ()"""
    parsed = d.parseString( s )
    print parsed
    
    return
    

def test_state_parms():
    print "test parsing state parmeters block...",
    d = state_parms()
    s = """STATE PARAMETERS (x, y, z)"""
    parsed = d.parseString( s )
    print parsed
    expected = ["x", "y", "z"]
    
    for e,parm in zip(expected, parsed):  assert e == parm
    
    s = """STATE PARAMETERS ()"""
    parsed = d.parseString( s )
    print parsed
    return
    

def test_component():
    s = E_monitor
    print "test component ... "
    parsed = component().parseString(s)
    print parsed.header
    print parsed.name
    print 'definition parameters: '
    for param in parsed.definition_parameters:
        print '  name: %s, type: %s, value: %s' % (
            param.name, param.type, param.value )
    print 'setting parameters: '
    for param in parsed.setting_parameters:
        print '  name: %s, type: %s, value: %s' % (
            param.name, param.type, param.value )
    print 'output parameters: '
    for param in parsed.output_parameters:
        print '  name: %s, type: %s, value: %s' % (
            param.name, param.type, param.value )
    print 'state parameters:', parsed.state_parameters
    print 'declare: ', parsed.declare
    print 'initialize: ', parsed.initialize
    print 'trace: ', parsed.trace
    print 'save: ', parsed.save
    print 'finalize: ', parsed.finalize
    return


E_monitor = """
/*******************************************************************************
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

DEFINE COMPONENT E_monitor
DEFINITION PARAMETERS (nchan=20, string filename)
SETTING PARAMETERS (xmin=0, xmax=0, ymin=0, ymax=0, xwidth=0, yheight=0, Emin, Emax)
OUTPUT PARAMETERS (E_N, E_p, E_p2, S_p, S_pE, S_pE2)
STATE PARAMETERS (x,y,z,vx,vy,vz,t,s1,s2,p)
DECLARE
  %{
    double *E_N, *E_p, *E_p2;
  %}
INITIALIZE
  %{
    int i;
    E_N = malloc(nchan*sizeof(double));
    E_p = malloc(nchan*sizeof(double));
    E_p2 = malloc(nchan*sizeof(double));

    for (i=0; i<nchan; i++)
    {
      E_N[i] = 0;
      E_p[i] = 0;
      E_p2[i] = 0;
    }
  %}
TRACE
  %{
    int i;
    double E;

    PROP_Z0;
    if (x>xmin && x<xmax && y>ymin && y<ymax)
    {
      E = VS2E*(vx*vx + vy*vy + vz*vz);

      i = floor((E-Emin)*nchan/(Emax-Emin));
      if(i >= 0 && i < nchan)
      {
        E_N[i]++;
        E_p[i] += p;
        E_p2[i] += p*p;
        SCATTER;
      }
    }
  %}
SAVE
  %{
    DETECTOR_OUT_1D(
        "Energy monitor",
        "Energy [meV]",
        "Intensity",
        "E", Emin, Emax, nchan,
        &E_N[0],&E_p[0],&E_p2[0],
        filename);
  %}

FINALLY
%{
#ifdef DEBUG
printf("free: E_N = %p, E_p = %p, E_p2 = %p\n", E_N, E_p, E_p2);
#endif
free(E_N); free(E_p); free(E_p2);
%}

MCDISPLAY
%{
  magnify("xy");
  multiline(5, (double)xmin, (double)ymin, 0.0,
               (double)xmax, (double)ymin, 0.0,
               (double)xmax, (double)ymax, 0.0,
               (double)xmin, (double)ymax, 0.0,
               (double)xmin, (double)ymin, 0.0);
%}

END
"""


def test():
    #test_define()
    #test_def_parms()
    #test_set_parms()
    #test_output_parms()
    #test_state_parms()
    test_component()
    return



if __name__ == "__main__": test()
