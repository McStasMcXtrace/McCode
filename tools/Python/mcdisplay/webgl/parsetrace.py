#!/usr/bin/env python
# -*- coding: utf-8 -*-
'''
Test script for PLY-based translation of mcdisplay --trace output mini language
'''
import logging
import argparse
from ply import lex

class TraceLexer():
    ''' 
    Lexer for --trace output enabling mcdisplay instrument drawing minilanguage
    
    In addition to the default INITIAL state in the lexer, the 'initialize', 'save' and 'finally' states are
    intended to parse the corresponding stdout as pure lines of comments. 
    This way we can avoid defining a "catchall" token in INITIAL.
     
    NOTEWARNING: the order of the token implementations matters
    '''
    
    # all tokens 
    tokens = (
              'INSTRUMENT',
              'INSTRKW',
              'COMPONENT',
              'COMPKW',
              'POS',
              'AT',
              'MCDISPLAY',
              'ENDKWLC',
              'COMPKWLC',
              
              'LB',
              'RB',
              'LSB',
              'RSB',
              'COLON',
              'QUOTE',
              'SQUOTE',
              'COMMA',
              'NL',
              
              'ENTER',
              'COMP',
              'STATE',
              'SCATTER',
              'ABSORB',
              'LEAVE',
              
              'ABSPATH',
              'DEC',
              'ID',
              
              # tokens only used in states "save" and "finally" (comment blocks)
              'COMMENT'
              )

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
    
    t_INSTRUMENT = r'INSTRUMENT'
    t_INSTRKW = r'Instrument'
    t_COMPONENT = r'COMPONENT'
    t_COMPKW = r'Component'
    t_POS = r'POS'
    t_AT = r'AT'
    t_MCDISPLAY = r'MCDISPLAY'
    t_ENDKWLC = r'end'
    t_COMPKWLC = r'component'
    
    t_LB = r'\('
    t_RB = r'\)'
    t_LSB = r'\['
    t_RSB = r'\]'
    t_COLON = r':'
    t_QUOTE = r'"'
    t_SQUOTE = r'\''
    t_COMMA = r','
    
    def t_ANY_NL(self, t):
        r'\n'
        self.lexer.lineno += 1
        return t
    
    t_ENTER = r'ENTER'
    t_COMP = r'COMP'
    t_STATE = r'STATE'
    t_SCATTER = r'SCATTER'
    t_ABSORB = r'ABSORB'
    t_LEAVE = r'LEAVE'

    def t_ABSPATH(self, t):
        r'/[\w/\.]+'
    
    def t_DEC(self, t):
        r'-?[\d.]+(e[+-][0-9]+)?'
        return t
    
    def t_ID(self, t):
        r'[a-zA-Z_]\w*'
        return t
    
    # ignore whitespaces and tabs means that we do not tokenize them, but they are still applied in the regex checks defined above for our tokens
    t_ignore = r' \t'
    
    def t_error(self, t):
        print('error: %s' % t.value)
    
    ##########################
    # build and test 
    ##########################
    def build(self, **kwargs):
        ''' builds the lexer '''
        self.lexer = lex.lex(module=self, **kwargs)

    def test(self, data):
        ''' test built lexer on data '''
        self.lexer.input(data)
        for token in self.lexer:
            print(token)

def main(args):
    logging.basicConfig(level=logging.INFO)
    
    data = open(args.data, 'r').read()
    lexer = TraceLexer()
    lexer.build()
    lexer.test(data)

    
if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('data', help='--trace data file.')

    args = parser.parse_args()
    main(args)
