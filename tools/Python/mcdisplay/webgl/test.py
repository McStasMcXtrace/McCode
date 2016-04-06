#!/usr/bin/env python
# -*- coding: utf-8 -*-
'''
Test script for PLY-based translation of mcdisplay "--trace output" mini language
'''
import logging
import argparse
from traceparser import TraceParser, NodeTreePrint, InstrProduction

def main(args):
    logging.basicConfig(level=logging.INFO)
    
    data = open(args.data, 'r').read()
    
    # build trace output parser
    parser = TraceParser(data)
    NodeTreePrint(parser.parsetree) 
    
    # build instrument data object from parsetree
    instrbuilder = InstrProduction(parser.parsetree)
    instrbuilder.build()
    
    # build html template
    
    
    # step-wise test
    if False: 
        parser = TraceParser()
        parser.build_lexer()
        #parser.test_lexer(data)
    
        parser.build_parser()
        parser.parse(data)
        
        NodeTreePrint(parser.parsetree, printrays=True)

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('data', help='--trace data file.')

    args = parser.parse_args()
    main(args)
