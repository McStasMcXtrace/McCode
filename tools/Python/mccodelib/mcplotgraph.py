'''
Plot-graph node types.
'''
import re

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

class PNMultiple(PlotNode):
    def __init__(self, header, data_lst):
        super(PNMultiple, self).__init__()
        self.header = header
        self.data = data_lst
    
    def __str__(self):
        return 'PNMultiple'

class PNSingle(PlotNode):
    def __init__(self, data_obj):
        super(PNSingle, self).__init__()
        self.data = [data_obj]
    
    def __str__(self):
        return 'PNSingle'

class PlotGraphPrint(object):
    ''' NOTE: iteration logics not yet implemented '''
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
        for d in node.data:
            print(indent*(level+2) + '%s (%d)' % (d, id(d)))
        
        if type(node) is PNMultiple:
            print(indent*(level+1) + 'header:')
            print(indent*(level+2) + '%s (%d)' % (node.header, id(node.header)))
        
        if not len(node.primaries) == 0:
            print(indent*(level+1) + 'primary children:')
            for p in node.primaries:
                print(indent*(level+2) + '%s (%d)' % (p, id(p)))
        
        if not len(node.secondaries) == 0:
            print(indent*(level+1) + 'secondary children:')
            for s in node.secondaries:
                print(indent*(level+2) + '%s (%d)' % (s, id(s)))
        
        self.printed_ids.append(id(node))

