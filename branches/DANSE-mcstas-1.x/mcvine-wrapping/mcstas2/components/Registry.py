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

class NotRegisteredError(Exception): pass

class Registry:


    def __init__(self):
        self.factories = {} # (category, type): factory
        self.infos = {} # (category, type): info
        self.types = {} # dictionary of category: type list 
        return


    def registered(self, category, type):
        key = category, type
        return key in self.factories


    def register(self, category, type, module):
        self._addType( category, type )
        key = category, type
        self.factories[ key ] = module.factory
        self.infos[ key ] = module.info
        return


    def getFactory(self, category, type ):
        factories = self.factories
        key = category, type
        if not factories.has_key( key ):
            return self._getStaticComponent( key ).factory
        return factories[ key ]


    def getInfo(self, category, type):
        infos = self.infos
        key = category, type
        if not infos.has_key( key ):
            return self._getStaticComponent( key ).info
        return infos[ key ]

    
    def _getStaticComponent( self, key ):
        from repositories import all as repos
        repos = list(repos)
        repos.reverse()
        
        category, type = key

        module = None
        for repo in repos:
            modulename = '%s.%s.%s' % (
                repo, category, type )
            try:
                module = __import__( modulename, {}, {}, [''] )
            except:
                continue
            break

        if module:
            self.register( category, type, module )
        else:
            raise NotRegisteredError, "component %r of category %r " % (type, category)
        
        return module


    def _addType( self, category, type ):
        assert isinstance( category, str )
        assert isinstance( type, str )
        types = self.types
        if category in types:
            if type in types[category]: return
            types[category].append( type )
            return
        else:
            types[category] = [type]
            return
        raise "Should not reach here. category=%s, type=%s" % (
            category, type)
    
    pass # end of Registry



# version
__id__ = "$Id$"

# End of file 
