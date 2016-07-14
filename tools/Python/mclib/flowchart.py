'''
A simple flowchart with boolean decision nodes, enter and exit terminals.
'''

class FlowChartNode(object):
    ''' flow chart abstract node '''
    def process(self, args):
        pass

class FCNDecisionBool(FlowChartNode):
    ''' a boolean decision node '''
    def __init__(self, eval_fct, node_T, node_F):
        self.eval_fct = eval_fct
        self.node_T = node_T
        self.node_F = node_F#
    
    def process(self, args):
        if self.eval_fct(args):
            return self.node_T
        else:
            return self.node_F

class FCNTerminal(FlowChartNode):
    ''' a terminal node '''
    def __init__(self, eval_fct=None, node_next=None, key=''):
        self.eval_fct = eval_fct
        self.node_next = node_next
        self.key = key
    
    def process(self, args):
        if self.eval_fct:
            self.eval_fct(args)
        
        return self.node_next
    
class FlowChartControl(object):
    ''' implements the flowchart traversal control '''
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
