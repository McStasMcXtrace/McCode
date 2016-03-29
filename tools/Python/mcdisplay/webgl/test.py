#!/usr/bin/env python
# -*- coding: utf-8 -*-
'''
Test script for PLY-based translation of mcdisplay "--trace output" mini language
'''
import logging
import argparse
from traceparser import TraceParser, NodeTreePrint

def main(args):
    logging.basicConfig(level=logging.INFO)
    
    data = open(args.data, 'r').read()
    
    parser = TraceParser(data)
    NodeTreePrint(parser.parsetree) 
    
    # step-wise test
    if False: 
        parser = TraceParser()
        parser.build_lexer()
        parser.test_lexer(data)
    
        parser.build_parser()
        parser.parse(data)
        
        NodeTreePrint(parser.parsetree, printrays=True)

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('data', help='--trace data file.')

    args = parser.parse_args()
    main(args)
