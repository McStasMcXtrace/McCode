'''
Implementation of classes involved in Python object construction from parsed mcdisplay --trace output.
'''
import re

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
        print fs.format('') + '%s: %s' % (type, leaf)
    
    @staticmethod
    def recurse(node, printfunc, inclevel, declevel, getlevel):
        for c in node.children:
            level = getlevel()
            printfunc(c, level)
            if len(c.children) > 0:
                inclevel()
                NodeTreePrint.recurse(c, printfunc, inclevel, declevel, getlevel)
        declevel()

def cleanTrace(data):
    ''' 
    splits data into three sections: 
    
    instrument definition
    component draw calls 
    neutron rays
    
    and captures all other text in a tertiary string
    '''
    pos_instr = data.find('INSTRUMENT:\n')
    pos_mcdisplay = data.find('MCDISPLAY: start')
    pos_neutrons = data.find('ENTER:\n')
    
    # get instrument definition
    lines = data[pos_instr:pos_mcdisplay].splitlines()
    cont = True
    lidx = 2
    while cont:
        testline = lines[lidx]
        if re.match('COMPONENT:', testline):
            lidx += 3
        else:
            cont = False
    instrdeftext = ''
    for i in range(lidx):
        instrdeftext = instrdeftext + lines[i] + '\n'
    remainder = ''
    for line in lines[lidx:]:
        remainder = remainder + line + '\n'
    
    # get mcdisplay draw calls
    lines = data[pos_mcdisplay:pos_neutrons].splitlines()
    cont = True
    lidx = 0
    while cont:
        if re.match('MCDISPLAY:', lines[lidx]):
            lidx += 1
        else:
            cont = False
    mcdisplaytext = ''
    for i in range(lidx):
        mcdisplaytext = mcdisplaytext + lines[i] + '\n'
    for line in lines[lidx+1:]: # NOTE: the +1 is because of the line "INSTRUMENT END:"
        remainder = remainder + line + '\n'
    
    # get neutron ray section (with trailing comment lines)
    lines = data[pos_neutrons:].splitlines()
    cont = True
    lidx = 0
    while cont:
        mat = re.match('(\w+):', lines[lidx])
        if mat:
            if mat.group(1) in ['ENTER', 'COMP', 'STATE', 'SCATTER', 'ABSORB', 'LEAVE']:
                lidx += 1
                continue
        cont = False
    raystext = ''
    for i in range(lidx):
        raystext = raystext + lines[i] + '\n'
    for line in lines[lidx:]:
        remainder = remainder + line + '\n'
    
    return instrdeftext, mcdisplaytext, raystext, remainder

