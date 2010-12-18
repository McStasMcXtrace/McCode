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


from HHMill import HHMill
from CCMill import CCMill

def createContentsOfHHandCC( klass ):
    "create .h and .cc files for a c++ class"
    
    from pyre.applications.Script import Script
    
    class App(Script):

        def main(self):
            weaver = self.weaver
            weaver.renderer = HHMill()
            hh = weaver.render( klass )
            
            weaver.renderer = CCMill()
            cc = weaver.render( klass )

            self.results = hh, cc
            return

        pass # end of App

    import sys
    save = sys.argv
    sys.argv = ['']
    app = App("tmp_app")
    app.run()
    sys.argv = save
    hh, cc = app.results
    return hh,cc


def createHHandCC( klass, pathToSave ):
    from os.path import join
    from os import makedirs
    try: makedirs( pathToSave )
    except OSError, msg:
        if "File exists" in msg: pass
        else: raise
        pass

    hh, cc = createContentsOfHHandCC( klass )
    
    hhfn = join(pathToSave, "%s.h"%klass.name ) 
    hhfile = open( hhfn, 'w' )
    hhfile.write( '\n'.join(hh) )
    
    ccfn = join(pathToSave, "%s.cc"%klass.name ) 
    ccfile = open( ccfn, 'w' )
    ccfile.write( '\n'.join(cc) )
    
    return hhfn, ccfn


# version
__id__ = "$Id$"

# End of file 
