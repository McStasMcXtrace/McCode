'''
Implementation of classes involved in Python object construction from parsed mcdisplay --trace output.
'''
class Node(object):
    '''
    Node objects are used to construct the trace parse tree.
    '''
    def __init__(self, type, children=None, leaf=None):
        self.type = type
        if children:
            self.children = children
        else:
            self.children = []
        self.leaf = leaf
    def __str__(self):
        return 'type: %s, leaf: %s, numchildren: %s' % (self.type, str(self.leaf), str(len(self.children)))

class NodeTreePrint(object):
    '''
    Functional-ish programming implementation of node tree operation: iteratively print nodes 
    in human readable form.
    
    Node tree assumptions: children is a list of Node's, leaf is for data
    Does not print any rays by default, enable by using printrays=True during construction.
    '''
    rootnode = None
    def __init__(self, rootnode):
        self.rootnode = rootnode
        self.print_tree()
    
    def print_tree(self):
        self.level = 0
        self.recurse(self.rootnode, self.printfunc, self.inclevel, self.declevel, self.getlevel)
    
    def getlevel(self):
        return self.level
    
    def inclevel(self):
        self.level += 1
    
    def declevel(self):
        self.level -= 1
    
    @staticmethod
    def printfunc(node, level):
        fs = '{:>' + str(level*4) + '}' # e.g. '{:<16}'
        type = node.type
        leaf = ''
        if node.leaf:
            leaf = node.leaf
        print(fs.format('') + '%s: %s' % (type, leaf))
    
    @staticmethod
    def recurse(node, printfunc, inclevel, declevel, getlevel):
        for c in node.children:
            level = getlevel()
            printfunc(c, level)
            if len(c.children) > 0:
                inclevel()
                NodeTreePrint.recurse(c, printfunc, inclevel, declevel, getlevel)
        declevel()
