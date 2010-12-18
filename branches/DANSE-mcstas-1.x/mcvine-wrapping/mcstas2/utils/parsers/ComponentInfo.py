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


import journal
warning = journal.warning( 'mcstas2.parsers.ComponentInfo' )


formatstr = '''
Component %(name)r: %(simple_description)s

%(full_description)s

%(parameters)s
'''

class ComponentInfo:

    def __init__(self, name = 'name', copyright = '', simple_description ='',
                 full_description = '', 
                 input_parameters = [],
                 output_parameters = [],
                 state_parameters = [],
                 declare = '',
                 initialize='', trace='', save='', finalize='',
                 share = ('', ''),
                 ):
        self.name = name
        self.copyright = copyright
        self.simple_description =simple_description
        self.full_description = full_description
        self.input_parameters = input_parameters
        self.output_parameters = output_parameters
        self.state_parameters = state_parameters
        self.declare = declare
        self.initialize = initialize
        self.trace = trace
        self.save = save
        self.finalize = finalize
        self.share = share
        return

    def __str__(self):
        d = self.__dict__.copy()
        parameters = self.input_parameters
        parameters_str = 'Parameters: \n'
        parameters_str += '\n'.join( ['  - %s' % param for param in parameters ] )
        d['parameters'] = parameters_str
        return formatstr % d

    pass # end of ComponentInfo



def _str( s ):
    # 0 is NULL pointer. it is difficult to have a NULL pointer in python.
    # so we just use an empty string.
    if s == '0': return ''
    return str(s)
value_converters = {
    'float': float,
    'double': float,
    'int': int,
    'char *': _str,
    }




class Parameter:

    def __init__(self, name='name', type='', default='', description = '' ):
        self.name = '%s' % name
        type = '%s' % type
        
        #string --> char *
        if type == 'string': type = 'char *'
        #default type is double
        if type == '': type = 'double'
        
        self.type = type

        value_converter = value_converters[type]
        try:
            default = value_converter( default )
        except ValueError:
            # float(''), XXX: General case?
            # Case when type is float and default is a string
            default = 0     
        except Exception, err:
            warning.log( 'parameter %s: %s: %s' % (
                name, err.__class__.__name__, err) )
            default = value_converter( )
        self.default = default
        
        self.description = '%s' % description
        return

    def __str__(self):
        return '%s(%s, default = %s): %s' % (
            self.name, self.type, self.default, self.description)


    __repr__ = __str__
    
    pass # end of Parameter

# version
__id__ = "$Id$"

# End of file 
