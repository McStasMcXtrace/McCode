# -*- Python -*-
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#                               Alex Dementsov
#                      California Institute of Technology
#                        (C) 2010  All Rights Reserved
#
# {LicenseText}
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#

# Unit tests for the new parser of McStas components

import unittest

class mcstas_parser_TestCase(unittest.TestCase):
    pass





def main():
    suite   = unittest.makeSuite(mcstas_parser_TestCase)
    tests   = unittest.TestSuite( (suite,) )
    unittest.TextTestRunner(verbosity=2).run(tests)


if __name__ == "__main__":
    main()
    
__date__ = "$Sep 15, 2010 3:10:07 PM$"


