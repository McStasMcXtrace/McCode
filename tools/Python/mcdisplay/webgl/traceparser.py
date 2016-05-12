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
    
    
    try:
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
        
        # filter datalines
        raylines = []
        raycomments = []
        problem_idxs = []
        
        for i in range(len(lines)):
            mat = re.match('(\w+):', lines[i])
            if mat and mat.group(1) in ['ENTER', 'COMP', 'STATE', 'SCATTER', 'ABSORB', 'LEAVE']:
                
                # theck for corrupted lines 
                seapos = lines[i].find('Warning: ')
                if seapos > 0:
                    # in this case, the whole neutron ray should be removed. Save the comment and the index.
                    raycomments.append(lines[i][seapos:])
                    problem_idxs.append(i)
                    continue
                
                raylines.append(lines[i])
            else:
                raycomments.append(lines[i])
        
        # Handle the problems identified above; raylines interrupted by "Warning: " somewhere in the middle. 
        # We must exterminate those rays from the data.
        lineslen = len(raylines)
        for idx in problem_idxs:
            # search backwards for "ENTER":
            i = -1
            idx_enter = -1
            idx_leave = -1
            while True:
                i += 1
                
                # find last ENTER:
                if idx_enter == -1 and re.match('ENTER:', raylines[idx-i]):
                    idx_enter = idx-i
                
                if idx_leave == -1 and idx+i == lineslen:
                    idx_leave = lineslen-1
                elif idx_leave == -1 and re.match('LEAVE:', raylines[idx+i]):
                    idx_leave = idx+i
                
                if idx_enter >= 0 and idx_leave >= 0:
                    for j in range(idx_enter, idx_leave+1): # NOTE: there should always be a STATE after LEAVE
                        del raylines[idx_enter] # NOTE: raylines will disappear underway
                    break
        
        # make sure the sequence ends with a LEAVE then a STATE
        while True:
            lineslen = len(raylines)
            last = raylines[lineslen-1]
            nextlast = raylines[lineslen-2]
            if (not re.match('LEAVE:', nextlast)) or (not re.match('STATE:', last)):
                del raylines[lineslen-1]
            else:
                break
        
        # reconstruct strings from lists
        raystext = ''
        for line in raylines:
            raystext = raystext + line + '\n'
        for line in raycomments:
            remainder = remainder + line + '\n'
        
        return instrdeftext, mcdisplaytext, raystext, remainder
    
    except Exception as e:
        print e.message
