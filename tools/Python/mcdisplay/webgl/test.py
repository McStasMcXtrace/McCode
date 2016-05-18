#!/usr/bin/env python
# -*- coding: utf-8 -*-
'''
Test script for PLY-based translation of mcdisplay "--trace output" mini language
'''
import logging
import argparse
from traceinstrparser import TraceInstrParser, InstrObjectConstructor
from nodetree import NodeTreePrint
from pipetools import cleanTrace
from drawcalls import TemplateWebGLWrite
from traceneutronrayparser import NeutronRayConstructor, TraceNeutronRayParser

def main(args):
    logging.basicConfig(level=logging.INFO)
    
    # build trace parser and parse data
    instr_txt, display_txt, rays_txt, comments_txt = cleanTrace(open(args.data, 'r').read())
    
    instrparser = TraceInstrParser(instr_txt + display_txt)
    rayparser = TraceNeutronRayParser(rays_txt)
    
    # print the parse tree as a test
    #treeprint = NodeTreePrint(instrparser.parsetree)
    #treeprint = NodeTreePrint(rayparser.parsetree)
    
    # build the instrument and neutron ray objects
    instrbuilder = InstrObjectConstructor(instrparser.parsetree)
    raybuilder = NeutronRayConstructor(rayparser.parsetree)
    
    instrument = instrbuilder.build_instr()
    instrument.rays = raybuilder.build_rays()
    
    # build html
    writer = TemplateWebGLWrite(instrument, 'template.html')
    writer.build()
    writer.save('mymultilines.html')


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('data', help='--trace data file.')

    args = parser.parse_args()
    main(args)
