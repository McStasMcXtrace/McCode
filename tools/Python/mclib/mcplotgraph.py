import re
'''
Generic Node and data types for assembling a graph that can be plotted 
using various mcplot frontend implementations.
'''
class Data1D(object):
    def __init__(self):
        self.component = ''
        self.filename = ''
        self.title = ''
        self.xlabel = ''
        self.ylabel = ''
        
        self.xvar = ''
        self.xlimits = () # pair
        
        self.variables = []
        
        self.yvar = () # pair
        self.values = () # triplet
        self.statistics = ''
        
        # data references
        self.xvals = []
        self.yvals = []
        self.y_err_vals = []
        self.Nvals = []
    
    def clone(self):
        data = Data1D()
        
        data.component = self.component
        data.filename = self.filename
        data.title = self.title
        data.xlabel = self.xlabel
        data.ylabel = self.ylabel
        
        data.xvar = self.xvar
        data.xlimits = self.xlimits
        
        data.variables = self.variables
        
        data.yvar = self.yvar
        data.values = self.values
        data.statistics = self.statistics
        
        # data references
        data.xvals = self.xvals
        data.yvals = self.yvals
        data.y_err_vals = self.y_err_vals
        data.Nvals = self.Nvals
        
        return data
        
    def get_stats_title(self):
        '''I=.... Err=... N=...; X0=...; dX=...;'''
        stitle = '%s=%e Err=%e N=%d; %s' % (self.yvar[0], self.values[0], self.values[1], self.values[2], self.statistics)
        return stitle

class Data2D(object):
    ''' not implemented '''
    def __init__(self):
        self.component = ''
        self.filename = ''
        self.title = ''
        
        self.xlabel = ''
        self.ylabel = ''
        
        self.xvar = ''
        self.yvar = ''
        self.zvar = ''
        self.xylimits = () # quadruple
        
        self.values = () # triplet
        self.statistics = '' # quadruple
        self.signal = ''
        
        # data references
        self.zvals = []
        self.counts = []
    
    def get_stats_title(self):
        '''I=.... Err=... N=...; X0=...; dX=...;'''
        stitle = '%s=%e Err=%e N=%d' % (self.zvar, self.values[0], self.values[1], self.values[2])
        return stitle

class DataMultiHeader(object):
    ''' not implemented '''
    def __init__(self):
        self.title = ''

'''
Plot graph node types have parent, primaries and secondaries, corresponding to whether 
"back", "click" or "ctr-click" is used to navigate.
Descendents also have a data pointer, which is an instance or a list.
'''
class PlotNode(object):
    ''' 
    Base class for plot graph nodes. 
    Parent is set implicitly on "primary" and "secondary" child node lists.
    '''
    def __init__(self):
        self.parent = None
    
    def set_primaries(self, node_lst):
        self.primaries = node_lst
        for node in node_lst:
            node.parent = self
    def get_primaries(self):
        return self.primaries
    
    def set_secondaries(self, node_lst):
        self.secondaries = node_lst
        for node in node_lst: 
            node.parent = self
    def get_secondaries(self):
        return self.secondaries
    
    def get_parent(self):
        return self.parent

class PNMultiple(PlotNode):
    def __init__(self, header, data_lst):
        self.header = header
        self.data_lst = data_lst

class PNSingle(PlotNode):
    def __init__(self, data):
        self.data = data

