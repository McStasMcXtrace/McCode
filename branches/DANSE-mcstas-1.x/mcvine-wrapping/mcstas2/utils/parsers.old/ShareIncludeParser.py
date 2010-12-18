#!/usr/bin/env python
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#                                   Jiao Lin
#                      California Institute of Technology
#                        (C) 2008 All Rights Reserved  
#
# {LicenseText}
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#


from pyparsing.pyparsing import *

def include():
    return Suppress( '%include') + quotedString.setResultsName( 'header' )


def test():
    text = '''
%include "read_table-lib"
'''
    print '%r' % include().parseString( text ).header
    return


if __name__ == "__main__": test()


# version
__id__ = "$Id$"

# End of file 
