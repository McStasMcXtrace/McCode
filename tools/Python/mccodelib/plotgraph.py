'''
Plot graph node types.

All plot graph node types have a parent, primaries and secondaries, corresponding to 
"click", "rclick" or "ctr-click" in a user interface.

The graph is directed from parents to (multiple) children.

Descendents also have a data handle instance or list. This facilitates deferred loading 
by means of the getdata() data handle method.
'''


class PlotNode(object):
    ''' 
    Base class for plot graph nodes. 
    '''
    def __init__(self):
        self.parent = None
        self.primaries = []
        self.secondaries = []
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
    def getdata_idx(self, idx):
        ''' must return a valid data handle index '''
    def getdata_lst(self):
        ''' must return a list '''
    def getnumdata(self):
        ''' must return the number of held data handles '''


class PNMultiple(PlotNode):
    '''
    A node containing multiple plot instances to be plotted simultaneously.
    '''
    def __init__(self, data_handle_lst):
        self._data_handle_lst = data_handle_lst
        super().__init__()
    def getdata_idx(self, idx):
        return self._data_handle_lst[idx].getdata()
    def getdata_lst(self):
        return [d.getdata() for d in self._data_handle_lst]
    def getnumdata(self):
        return len(self._data_handle_lst)
    def __str__(self):
        return 'PNMultiple'


class PNSingle(PlotNode):
    '''
    A node containing a data handle instance to be plotted singularly.
    '''
    def __init__(self, data_handle):
        self._data_handle = data_handle
        super().__init__()
    def getdata_idx(self, idx):
        if idx != 0: raise Exception('PNSingle: getdata_idx: idx must be zero.')
        return self._data_handle.getdata()
    def getdata_lst(self):
        return [self._data_handle.getdata()]
    def getnumdata(self):
        return 1
    def __str__(self):
        return 'PNSingle'


class DataHandle(object):
    ''' 
    Proxy object enabling deferred loading through the use of "getdata".
    
    Do not set the data property explicitly in the constructor unless you have a good reason.
    '''
    def __init__(self, load_fct, data=None):
        self._load_fct = load_fct
        self._data = data
    def getdata(self):
        if not self._data:
            self._data = self._load_fct()
        return self._data


class PlotGraphPrint(object):
    '''
    Simple plot graph visualization tool for debugging graph integrity.
    
    NOTE: Iteration logics has not been implemented.
    '''
    def __init__(self, rootnode, indent_str='    '):
        if indent_str == '' or type(indent_str) != str:
            raise Exception('PlotGraphPrint: indent_str must be a non-empty string.')
        self.indent_str = indent_str
        self.root = rootnode
        self.printed_ids = []
        # execute
        self.print_recurse(self.root, level=0)
        
    def print_recurse(self, node, level):
        ''' node print recursion '''
        self.printnode(node, level)
        children = node.primaries + node.secondaries
        for c in children:
            self.print_recurse( c, level+1)                

    def printnode(self, node, level=0):
        ''' 
        Prints the node id, its children id's and data reference, respecting indent and
        using self.indent_str.
        '''
        # only print nodes once
        if id(node) in self.printed_ids:
            return
        
        indent = self.indent_str
        
        # print the node
        print()
        print(indent*(level+0) + '%s (%d):' % (node, id(node)))
        
        if node.parent:
            print(indent*(level+1) + 'parent:')
            print(indent*(level+2) + '%s (%d)' % (node.parent, id(node.parent)))
        
        print(indent*(level+1) + 'data objects:')
        for d in node.getdata_lst():
            print(indent*(level+2) + '%s (%d)' % (d, id(d)))
        
        if not len(node.primaries) == 0:
            print(indent*(level+1) + 'primary children:')
            for p in node.primaries:
                print(indent*(level+2) + '%s (%d)' % (p, id(p)))
        
        if not len(node.secondaries) == 0:
            print(indent*(level+1) + 'secondary children:')
            for s in node.secondaries:
                print(indent*(level+2) + '%s (%d)' % (s, id(s)))
        
        self.printed_ids.append(id(node))

