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


def test():
    from Class import example
    from CCMill import CCMill
    
    klass = example()

    from installationInfo import tmp

    from factory import createHHandCC

    hhfn, ccfn = createHHandCC( klass, tmp )

    import os
    os.system( "cd %s && g++ -c %s" % (tmp, ccfn) )
    
    return


if __name__ == "__main__": test()

# version
__id__ = "$Id$"

# End of file 
