#!/usr/bin/env python
# -*- coding: utf-8 -*-
'''
Test script for PLY-based translation of mcdisplay "--trace output" mini language
'''
import logging
import argparse
from traceparser import TraceParser, NodeTreePrint, InstrProduction
from drawcalls import TemplateWebGLWrite

def main(args):
    logging.basicConfig(level=logging.INFO)
    
    # build trace parser and parse data
    data = open(args.data, 'r').read()
    parser = TraceParser(data)
    
    # print the parse tree as a test
    treeprint = NodeTreePrint(parser.parsetree) 
    treeprint.print_tree()
    
    # build instrument data object
    instrbuilder = InstrProduction(parser.parsetree)
    instrbuilder.build()
    
    # build html
    writer = TemplateWebGLWrite(instrbuilder.instrument_tree)
    writer.build()
    #print writer.text
    #writer.save('mymultilines.html')
    
    # step-wise trace parser test
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
