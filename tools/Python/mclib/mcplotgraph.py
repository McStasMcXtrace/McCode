'''
Plot-graph nodes and data types.

NOTE: Data loaders (specific and thus external) and 
graph assembly procedures are written elsewhere.
'''
import re

'''
mccode output data in pythonified form.
'''
class DataMcCode(object):
    ''' base type holding only the data object's title '''
    def __init__(self, *args, **kwargs):
        self.title = ''

    def __str__(self, *args, **kwargs):
        return self.title

class Data1D(DataMcCode):
    ''' 1d plots use this data type '''
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
        if len(self.values) >= 3:
            stitle = '%s=%e Err=%e N=%d; %s' % (self.yvar[0], self.values[0], self.values[1], self.values[2], self.statistics)
        else:
            stitle = '%s of %s' % (self.yvar[0], self.xvar)
        return stitle
    
    def __str__(self):
        return 'Data1D, ' + self.get_stats_title()

class Data2D(DataMcCode):
    ''' PSD data type '''
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
    
    def __str__(self):
        return 'Data2D, ' + self.get_stats_title()

class DataMultiHeader(DataMcCode):
    ''' "header" place holder type used in the plot graph '''
    def __str__(self):
        return 'DataMultiHeader'

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
        self.data = []
    
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
        self.data = data_lst
    
    def __str__(self):
        return 'PNMultiple'

class PNSingle(PlotNode):
    def __init__(self, data_obj):
        self.data = [data_obj]
    
    def __str__(self):
        return 'PNSingle'

class PlotGraphPrint(object):
    ''' NOTE: iteration logics not yet implemented '''
    def __init__(self, rootnode, indent_str):
        if indent_str == '' or type(indent_str) != str:
            raise Exception('PlotGraphPrint: indent_str must be a non-empty string.')
        self.indent_str = indent_str
        self.node = rootnode
        self.printed_ids = []

    def printnode(self, node, level=0):
        ''' prints the node id, its children id's and data reference, respecting indent '''        
        
        # only print nodes once
        if id(node) in self.printed_ids:
            return None
        indent = self.indent_str
        
        # print the node
        print
        #print indent*(level+0) + str(type(node)) + str(id(node)) + ':'
        print indent*(level+0) + 'node %s (%d):' % (node, id(node))
        print indent*(level+1) + 'data objects:'
        for d in node.data:
            print indent*(level+2) + '%s (%d)' % (d, id(d))
        
        print indent*(level+1) + 'primary children:'
        for p in node.primaries:
            #print indent*(level+2) + str(type(p)) + str(id(p))
            print indent*(level+2) + '%s (%d)' % (p, id(p))
        
        print indent*(level+1) + 'secondary children:'
        for s in node.secondaries:
            #print indent*(level+2) + str(type(s)) + str(id(s))
            print indent*(level+2) + '%s (%d)' % (s, id(s))
        
        self.printed_ids.append(id(node))













