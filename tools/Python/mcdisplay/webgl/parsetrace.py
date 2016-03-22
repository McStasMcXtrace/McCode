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
    # special states must be implemented before 'ID' below, which would catch the state's "start condition" as an ID
    ##############################
    
    # 'exclusive' means that no token definitions from INITIAL are carried over
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
    # back to INITIAL state impl
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
    # parsing rules
    ##################################
    
    def p_document(self, p):
        'document : instr_open comp_blocks comments draw_lines instr_close comments ray_statements comments'
        print 'parsed a document'
        print p[7]
    
    def p_instr_open(self, p):
        'instr_open : INSTRUMENT COLON NL INSTRKW SQUOTE instr_name SQUOTE LB ABSPATH RB NL'
        print 'parsed a instr_open'
        
    def p_instr_name(self, p):
        'instr_name : ID'
        print 'parsed a instr_name'
    
    def p_comp_blocks(self, p):
        '''comp_blocks : comp_block comp_blocks
                       | comp_block '''
        print 'parsed a comp_blocks'

    def p_comp_block(self, p):
        'comp_block : COMPONENT COLON QUOTE comp_name QUOTE NL POS COLON 12dec NL COMPKW comp_name AT LB 3dec RB NL'
        print 'parsed a comp_block'
    
    def p_comp_name(self, p):
        'comp_name : ID'
        print p[1]
        p[0] = Node(type="comp_name", leaf=p[1])

    def p_12dec(self, p):
        '12dec : DEC COMMA DEC COMMA DEC COMMA DEC COMMA DEC COMMA DEC COMMA DEC COMMA DEC COMMA DEC COMMA DEC COMMA DEC COMMA DEC'
        print 'parsed a 12dec'
        
    def p_3dec(self, p):
        '3dec :  DEC COMMA DEC COMMA DEC'
        print 'parsed a 3dec'
    
    def p_comments(self, p):
        '''comments : comment comments
                    | empty '''
        print 'parsed a comments'
    
    def p_comment(self, p):
        '''comment : COMMENT NL
                   | NL'''
        print 'parsed a comment'
    
    def p_draw_lines(self, p):
        '''draw_lines : draw_line draw_lines
                      | draw_line '''
        print 'parsed draw_lines'
    
    def p_draw_line(self, p):
        '''draw_line : draw_header
                     | draw_command
                     | draw_open
                     | draw_close'''
        print 'parsed a draw_line'
    
    def p_draw_header(self, p):
        '''draw_header : MCDISPLAY COLON COMPKWLC comp_name NL'''
        print 'parsed a draw_header'
    
    def p_draw_command(self, p):
        '''draw_command : MCDISPLAY COLON DRAWCALL LB args RB NL
                        | MCDISPLAY COLON DRAWCALL LB SQUOTE SQUOTE RB NL
                        | MCDISPLAY COLON DRAWCALL LB SQUOTE arg SQUOTE RB NL
                        | MCDISPLAY COLON DRAWCALL LB SQUOTE arg SQUOTE COMMA args RB NL
                        | MCDISPLAY COLON DRAWCALL LB RB NL'''
        print 'parsed a draw_command', p[3]
    
    def p_args(self, p):
        '''args : arg COMMA args
                | arg'''
        print 'parsed an args'
    
    def p_arg(self, p):
        '''arg : DEC
               | ID'''
        print 'parsed an arg', p[1]
    
    def p_empty(self, p):
        'empty :'
        print 'parsed a empty'
    
    def p_draw_open(self, p):
        '''draw_open : MCDISPLAY COLON STARTKWLC NL'''
        print 'parsed a draw_open'
    
    def p_draw_close(self, p):
        'draw_close : MCDISPLAY COLON ENDKWLC NL'
        print 'parsed a draw_close'
    
    def p_instr_close(self, p):
        'instr_close : INSTRUMENT END COLON NL'
        print 'parsed a instr_close'
    
    rays = Node(type='rays')
    def p_ray_statements(self, p):
        '''ray_statements : ray_statement ray_statements
                          | ray_statement '''
        if p[1] != None:
            self.rays.children.append(p[1])
            print 'appended ray to rays'
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
            print 'ENTER ray'
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
        p[0] = Node(type='COMP', children=[p[4], p[9]])
    
    def p_ray_compstatestate(self, p):
        'ray_compstatestate : COMP COLON QUOTE comp_name QUOTE NL STATE COLON 11dec NL STATE COLON 11dec NL'
        p[0] = Node(type='COMP', children=[p[4], p[9]])
    
    def p_ray_scatterstate(self, p):
        'ray_scatterstate : SCATTER COLON 11dec NL STATE COLON 11dec NL'
        p[0] = Node(type='SCATTER', children=[p[3]])
    
    def p_ray_scatter(self, p):
        'ray_scatter : SCATTER COLON 11dec NL'
        p[0] = Node(type='SCATTER', leaf=p[3])
    
    def p_ray_absorb(self, p):
        'ray_absorb : ABSORB COLON NL'
        p[0] = Node(type='ABSORB')
    
    def p_ray_enterstate(self, p):
        'ray_enterstate : ENTER COLON NL STATE COLON 11dec NL'
        p[0] = Node(type='ENTER', leaf=p[6])
    
    def p_ray_leavestate(self, p):
        'ray_leavestate : LEAVE COLON NL STATE COLON 11dec NL'
        p[0] = Node(type='LEAVE', leaf=p[6])
    
    def p_11dec(self, p):
        '11dec : DEC COMMA DEC COMMA DEC COMMA DEC COMMA DEC COMMA DEC COMMA DEC COMMA DEC COMMA DEC COMMA DEC COMMA DEC'
        p[0] = Node(type='11dec', leaf=[p[1],p[3],p[5],p[7],p[9],p[11],p[13],p[15],p[17],p[19],p[21]])
    
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

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('data', help='--trace data file.')

    args = parser.parse_args()
    main(args)
