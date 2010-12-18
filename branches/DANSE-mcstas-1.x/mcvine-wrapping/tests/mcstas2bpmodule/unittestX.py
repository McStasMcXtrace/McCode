#!/usr/bin/env python
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#                                   Jiao Lin
#                      California Institute of Technology
#              (C) 2005 All Rights Reserved  All Rights Reserved
#
# {LicenseText}
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#


from unittest import *
from unittest import TestCase as TCBase

class TestCase(TCBase):

    def assertVectorEqual( self, v1, v2):
        self.assertEqual( len(v1), len(v2) )
        for x1, x2 in zip(v1, v2): self.assertEqual( x1, x2 )
        return

            
    def assertVectorAlmostEqual( self, v1, v2):
        self.assertEqual( len(v1), len(v2) )
        for x1, x2 in zip(v1, v2): self.assertAlmostEqual( x1, x2 )
        return
            
    def assertMatrixEqual( self, m1, m2):
        self.assertEqual( len(m1), len(m2) )
        for x1, x2 in zip(m1, m2): self.assertVectorEqual( x1, x2 )
        return
            
    def assertMatrixAlmostEqual( self, m1, m2):
        self.assertEqual( len(m1), len(m2) )
        for x1, x2 in zip(m1, m2): self.assertVectorAlmostEqual( x1, x2 )
        return

    pass # end of TestCaae

# version
__id__ = "$Id$"

# End of file 
