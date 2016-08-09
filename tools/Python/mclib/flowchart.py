'''
A simple flowchart with boolean decision nodes, enter and exit terminals.
'''
class FlowChartNode(object):
    ''' flow chart abstract node '''
    def process(self, args):
        pass

class FCNDecisionBool(FlowChartNode):
    ''' a boolean decision node '''
    def __init__(self, fct, node_T=None, node_F=None):
        ''' fct(args) -> bool '''
        self.fct = fct
        self.node_T = node_T
        self.node_F = node_F
    
    def set_nodes(self, node_T, node_F):
        self.node_T = node_T
        self.node_F = node_F
    
    def process(self, args):
        if self.fct(args):
            return self.node_T
        else:
            return self.node_F

class FCNDecisionMulti(FlowChartNode):
    ''' a decision node with multiple (boolean) exits '''
    def __init__(self, fct, node_lst=None):
        ''' fct(args) -> node index, as organized in nodes_lst '''
        self.fct = fct
        self.nodes = node_lst
    
    def set_node_lst(self, node_lst):
        self.nodes = node_lst
    
    def process(self, args):
        return self.nodes[self.fct(args)]

class FCNProcess(FlowChartNode):
    ''' action node with a single exit '''
    def __init__(self, fct, node_next=None):
        ''' fct(args) -> anything '''
        self.fct = fct
        self.node_next = node_next
    
    def set_nodenext(self, node_next):
        self.node_next = node_next
    
    def process(self, args):
        self.fct(args)
        return self.node_next

class FCNTerminal(FlowChartNode):
    ''' a terminal node '''
    def __init__(self, fct=None, node_next=None, key=''):
        ''' fct(args) -> anything '''
        self.fct = fct
        self.node_next = node_next
        self.key = key
    
    def set_nodenext(self, node_next):
        self.node_next = node_next
    
    def process(self, args):
        if self.fct:
            self.result = self.fct(args)
        
        return self.node_next

'''
Control class for traversing the chart, executing functions and so on
'''
class FlowChartControl(object):
    ''' implements the flowchart graph traversal control '''
    def __init__(self, terminal_enter):
        ''' '''
        self.terminal_enter = terminal_enter
    
    def process(self, args):
        ''' traverses the flowchart and returns a node that terminates on process(args) '''
        node = self.terminal_enter
        next = node
        
        while next:
            next = node.process(args)
            if next: 
                node = next
        
        # node must be the exit terminal
        return node
