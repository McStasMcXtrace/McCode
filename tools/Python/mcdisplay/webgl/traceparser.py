#!/usr/bin/env python
# -*- coding: utf-8 -*-
'''
Test script for PLY-based translation of mcdisplay --trace output mini language

Read the PLY documentation here: http://www.dabeaz.com/ply/ply.html#ply_nn23
'''
import logging
import argparse
from ply import lex, yacc

class Node:
    ''' Node objects are used to construct the AST-ish structure (abstract syntax tree) '''
    def __init__(self, type, children=None, leaf=None):
        self.type = type
        if children:
            self.children = children
        else:
            self.children = [ ]
        self.leaf = leaf
    def __str__(self):
        return 'type: %s, leaf: %s, numchildren: %s' % (self.type, str(self.leaf), str(len(self.children)))

class NodeTreePrint:
    ''' Node tree assumptions: children is a list of Node's, leaf is for data'''
    def __init__(self, rootnode, printrays=True):
        self.level = 0
        self.recurse(rootnode, printrays, self.printfunc, self.inclevel, self.declevel, self.getlevel)
    
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
    def recurse(node, printrays, printfunc, inclevel, declevel, getlevel):
        for c in node.children:
            # exit if printrays is not set
            if c.type=='rays' and not printrays:
                return
            
            level = getlevel()
            printfunc(c, level)
            if len(c.children) > 0:
                inclevel()
                NodeTreePrint.recurse(c, printrays, printfunc, inclevel, declevel, getlevel)
        declevel()

class TraceParser:
    '''
    Parser for --trace output enabling mcdisplay instrument drawing minilanguage
    
    In addition to the default INITIAL state in the lexer, the 'initialize', 'save' and 'finally' states are
    intended to parse the corresponding stdout as pure lines of comments.
    This way we can avoid defining a "catchall" token in INITIAL.
    '''
    
    # these tokens match ID, but are of these types (handled in t_ID)
    reserved = {
        'INSTRUMENT'  : 'INSTRUMENT',
        'Instrument'  : 'INSTRKW',
        'COMPONENT'   : 'COMPONENT',
        'Component'   : 'COMPKW',
        'POS'         : 'POS',
        'AT'          : 'AT',
        'MCDISPLAY'   : 'MCDISPLAY',
        'start'       : 'STARTKWLC',
        'end'         : 'ENDKWLC',
        'component'   : 'COMPKWLC',
        'END'         : 'END',
        
        'ENTER'       : 'ENTER',
        'COMP'        : 'COMP',
        'STATE'       : 'STATE',
        'SCATTER'     : 'SCATTER',
        'ABSORB'      : 'ABSORB',
        'LEAVE'       : 'LEAVE',
        
        'magnify'     : 'DRAWCALL',
        'line'        : 'DRAWCALL',
        'dashed_line' : 'DRAWCALL',
        'multiline'   : 'DRAWCALL',
        'rectangle'   : 'DRAWCALL',
        'box'         : 'DRAWCALL',
        'circle'      : 'DRAWCALL',
    }
    
    # tokens 
    tokens = [
              'LB',
              'RB',
              'COLON',
              'QUOTE',
              'SQUOTE',
              'COMMA',
              'NL',
              
              'ABSPATH',
              'DEC',
              'ID',
              
              # tokens only used in states "save" and "finally" (comment blocks)
              'COMMENT'
              ] + list(set(reserved.values()))

    ###############################
    # special states 
    ##############################
    
    # NOTE: these states must be implemented before 'ID' token, which would catch the states' 
    # "start condition" as an ID.
    
    states = (
        ('initialize', 'exclusive'),
        ('save', 'exclusive'),
        ('finally','exclusive'),
    )
    
    # initialize state
    def t_begin_initialize(self, t):
        r'\[\w+\]\s+Initialize'
        self.lexer.begin('initialize')
        t.type = 'COMMENT'
        return t
    def t_initialize_end(self, t):
        r'MCDISPLAY'
        self.lexer.begin('INITIAL')
        t.type = 'MCDISPLAY'
        return t
    t_initialize_COMMENT = r'.+'
    
    # save state
    def t_begin_save(self, t):
        r'Save.*'
        self.lexer.begin('save')
        t.type = 'COMMENT'
        return t
    def t_save_finallybegin(self, t):
        r'Finally.*'
        self.lexer.begin('finally')
        t.type = 'COMMENT'
        return t
    t_save_COMMENT = r'.+'
    
    # finally state (NOTE: there is no begin token, because it is initiated during save state)
    t_finally_COMMENT = r'.+'

    ##################################
    # INITIAL state
    ##################################
    
    t_LB = r'\('
    t_RB = r'\)'
    t_COLON = r':'
    t_QUOTE = r'"'
    t_SQUOTE = r'\''
    t_COMMA = r','
    
    def t_ANY_NL(self, t):
        r'\n'
        self.lexer.lineno += 1
        return t

    def t_ABSPATH(self, t):
        r'/[/\w\.]+'
        return t
    
    def t_DEC(self, t):
        r'-?[\d.]+(e[+-][0-9]+)?'
        return t
    
    def t_ID(self, t):
        r'[a-zA-Z_]\w*'
        t.type = self.reserved.get(t.value, 'ID')
        return t
    
    # ignore whitespaces and tabs means that we do not tokenize them, but they are still applied in the regex checks defined above for our tokens
    t_ignore = r' \t'
    
    def t_error(self, t):
        print('error: %s' % t.value)
    
    ##################################
    # parsing rules and action code
    ##################################
    
    def p_document(self, p):
        'document : instr_open comp_defs comments draw_lines instr_close comments ray_statements comments'
        print 'mcdisplay document parsed'
        self.parsetree = Node(type='document', children=[self.instr, self.comps, self.rays, Node(type='comments', leaf=self.comments)])
    
    instr = None
    def p_instr_open(self, p):
        'instr_open : INSTRUMENT COLON NL INSTRKW SQUOTE instr_name SQUOTE LB ABSPATH RB NL'
        self.instr = Node(type='instrument', children=[p[6], Node(type='abspath', leaf=p[9])])
        
    def p_instr_name(self, p):
        'instr_name : ID'
        p[0] = Node(type='instr_name', leaf=p[1])
    
    comps = Node(type='comps')
    def p_comp_defs(self, p):
        '''comp_defs : comp_def comp_defs
                     | comp_def '''
        self.comps.children.append(p[1])
        p[0] = self.comps

    def p_comp_def(self, p):
        'comp_def : COMPONENT COLON QUOTE comp_name QUOTE NL POS COLON 12dec NL COMPKW comp_name AT LB 3dec RB NL'
        p[0] = Node(type='comp', children=[p[4], p[9], p[15]])
    
    def p_comp_name(self, p):
        'comp_name : ID'
        p[0] = Node(type="comp_name", leaf=p[1])

    def p_12dec(self, p):
        '12dec : DEC COMMA DEC COMMA DEC COMMA DEC COMMA DEC COMMA DEC COMMA DEC COMMA DEC COMMA DEC COMMA DEC COMMA DEC COMMA DEC'
        p[0] = Node(type='12dec', leaf=[p[1], p[3], p[5], p[7], p[9], p[11], p[13], p[15], p[17], p[19], p[21], p[21]])
        
    def p_3dec(self, p):
        '3dec :  DEC COMMA DEC COMMA DEC'
        p[0] = Node(type='3dec', leaf=[p[1], p[3], p[5]])
    
    def p_comments(self, p):
        '''comments : comment comments
                    | empty '''
    
    comments = []
    def p_comment(self, p):
        '''comment : COMMENT NL
                   | NL'''
        self.comments.append(p[1]) 
    
    def p_draw_lines(self, p):
        '''draw_lines : draw_line draw_lines
                      | draw_line '''
    
    def p_draw_line(self, p):
        '''draw_line : draw_header
                     | draw_command
                     | draw_open
                     | draw_close'''
    
    def p_draw_header(self, p):
        '''draw_header : MCDISPLAY COLON COMPKWLC comp_name NL'''
        self.commands = Node(type='draw_commands')
        for comp in self.comps.children:
            for c in comp.children:
                if c.leaf==p[4].leaf:
                    comp.children.append(self.commands)
                    break
    
    commands = None
    def p_draw_command(self, p):
        '''draw_command : MCDISPLAY COLON DRAWCALL LB args RB NL
                        | MCDISPLAY COLON DRAWCALL LB SQUOTE arg SQUOTE RB NL
                        | MCDISPLAY COLON DRAWCALL LB SQUOTE arg SQUOTE COMMA args RB NL
                        | MCDISPLAY COLON DRAWCALL LB SQUOTE SQUOTE RB NL
                        | MCDISPLAY COLON DRAWCALL LB RB NL'''
        self.commands.children.append(Node(type='draw', children=[self.args], leaf=p[3]))
        # reset args after having parsed them all, which is now
        self.args = Node(type='args', leaf=[])
    
    args = Node(type='args', leaf=[])
    def p_args(self, p):
        '''args : arg COMMA args
                | arg'''
    
    def p_arg(self, p):
        '''arg : DEC
               | ID'''
        self.args.leaf.append(p[1])
    
    def p_empty(self, p):
        'empty :'
    
    def p_draw_open(self, p):
        '''draw_open : MCDISPLAY COLON STARTKWLC NL'''
    
    def p_draw_close(self, p):
        'draw_close : MCDISPLAY COLON ENDKWLC NL'
    
    def p_instr_close(self, p):
        'instr_close : INSTRUMENT END COLON NL'
    
    rays = Node(type='rays')
    def p_ray_statements(self, p):
        '''ray_statements : ray_statement ray_statements
                          | ray_statement '''
        # this function only receives a value in p[1] - the p[0] og p_ray_statement - when a ray is "finished" at that lower level
        if p[1] != None:
            self.rays.children.append(p[1])
        p[0] = self.rays
    
    ray = None
    def p_ray_statement(self, p):
        '''ray_statement : ray_enterstate
                         | ray_compstate
                         | ray_compstatestate
                         | ray_scatterstate
                         | ray_scatter
                         | ray_absorb
                         | ray_leavestate'''
        if p[1].type=='ENTER' and self.ray==None:
            self.ray = Node(type='ray', children=[p[1]])
        elif p[1].type=='LEAVE':
            self.ray.children.append(p[1])
            p[0] = self.ray
            self.ray = None
        elif self.ray==None:
            raise Exception('p_ray_statement: ray must begin start with a ray_enterstate')
        else:
            self.ray.children.append(p[1])
        
    def p_ray_compstate(self, p):
        'ray_compstate : COMP COLON QUOTE comp_name QUOTE NL STATE COLON 11dec NL'
        p[0] = Node(type='COMP', children=[p[4]], leaf=p[9].leaf)
    
    def p_ray_compstatestate(self, p):
        'ray_compstatestate : COMP COLON QUOTE comp_name QUOTE NL STATE COLON 11dec NL STATE COLON 11dec NL'
        p[0] = Node(type='COMP', children=[p[4]], leaf=p[9].leaf)
    
    def p_ray_scatterstate(self, p):
        'ray_scatterstate : SCATTER COLON 11dec NL STATE COLON 11dec NL'
        p[0] = Node(type='SCATTER', leaf=p[3].leaf)
    
    def p_ray_scatter(self, p):
        'ray_scatter : SCATTER COLON 11dec NL'
        p[0] = Node(type='SCATTER', leaf=p[3].leaf)
    
    def p_ray_absorb(self, p):
        'ray_absorb : ABSORB COLON NL'
        p[0] = Node(type='ABSORB')
    
    def p_ray_enterstate(self, p):
        'ray_enterstate : ENTER COLON NL STATE COLON 11dec NL'
        p[0] = Node(type='ENTER', leaf=p[6].leaf)
    
    def p_ray_leavestate(self, p):
        'ray_leavestate : LEAVE COLON NL STATE COLON 11dec NL'
        p[0] = Node(type='LEAVE', leaf=p[6].leaf)
    
    def p_11dec(self, p):
        '11dec : DEC COMMA DEC COMMA DEC COMMA DEC COMMA DEC COMMA DEC COMMA DEC COMMA DEC COMMA DEC COMMA DEC COMMA DEC'
        p[0] = Node(type='11dec', leaf=[p[1], p[3] ,p[5] ,p[7] ,p[9] ,p[11] ,p[13] ,p[15] ,p[17] ,p[19] ,p[21]])
    
    # error rule for syntax errors
    def p_error(self, p):
        print("Syntax error in input!")
        print(p)

    ##################################
    # build and test 
    ##################################
    
    def build_lexer(self, **kwargs):
        ''' builds the lexer '''
        self.lexer = lex.lex(module=self, **kwargs)

    def test_lexer(self, data):
        ''' test built lexer on data '''
        self.lexer.input(data)
        for token in self.lexer:
            print(token)
    
    def build_parser(self, **kwargs):
        ''' builds the lexer '''
        self.parser = yacc.yacc(module=self, debug=True)

    def parse(self, data):
        ''' attempts to parse data '''
        self.parser.parse(data, lexer=self.lexer)

def main(args):
    logging.basicConfig(level=logging.INFO)
    
    data = open(args.data, 'r').read()
    
    parser = TraceParser()
    parser.build_lexer()
    #parser.test_lexer(data)

    parser.build_parser()
    parser.parse(data)
    
    NodeTreePrint(parser.parsetree, printrays=False)
    

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('data', help='--trace data file.')

    args = parser.parse_args()
    main(args)
