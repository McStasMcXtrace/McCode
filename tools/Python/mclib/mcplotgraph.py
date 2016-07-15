'''
Generic Node and data types for assembling a graph that can be plotted 
using various mcplot frontend implementations.
'''
class Data1D(object):
    '''  '''
    def __init__(self):
        self.title = ''

class Data2D(object):
    '''  '''
    def __init__(self):
        self.title = ''

class DataMultiHeader(object):
    '''  '''
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

