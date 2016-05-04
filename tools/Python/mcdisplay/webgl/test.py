#!/usr/bin/env python
# -*- coding: utf-8 -*-
'''
Test script for PLY-based translation of mcdisplay "--trace output" mini language
'''
import logging
import argparse
from traceinstrparser import TraceInstrParser, InstrObjectConstructor
from traceparser import NodeTreePrint, cleanTrace
from drawcalls import TemplateWebGLWrite
from traceneutronrayparser import NeutronRayConstructor, TraceNeutronRayParser

def main(args):
    logging.basicConfig(level=logging.INFO)
    
    # build trace parser and parse data
    instr, display, rays, comments = cleanTrace(open(args.data, 'r').read())
    
    instrparser = TraceInstrParser(instr + display)
    rayparser = TraceNeutronRayParser(rays)
    
    # print the parse tree as a test
    #treeprint = NodeTreePrint(instrparser.parsetree)
    #treeprint = NodeTreePrint(rayparser.parsetree)
    
    # build the instrument object
    instrbuilder = InstrObjectConstructor(instrparser.parsetree)
    instrument = instrbuilder.build_instr()
    
    # build the neutron ray tree
    raybuilder = NeutronRayConstructor(rayparser.parsetree)
    rays = raybuilder.build_rays()
    
    instrument.rays = rays
    
    # build html
    writer = TemplateWebGLWrite(instrument, 'template.html')
    writer.build()
    writer.save('mymultilines.html')
    
    # step-wise trace parser test
    if False: 
        parser = TraceInstrParser()
        parser.build_lexer()
        #parser.test_lexer(data)
        
        parser.build_parser()
        parser.parse(instr + display)
        
        NodeTreePrint(parser.parsetree, printrays=True)

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('data', help='--trace data file.')

    args = parser.parse_args()
    main(args)
