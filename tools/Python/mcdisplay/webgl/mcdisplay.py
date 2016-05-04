#!/usr/bin/env python
# -*- coding: utf-8 -*-
'''
mcdisplay webgl script.
'''
import sys
import os
import webbrowser
import logging
import argparse
from traceinstrparser import TraceInstrParser, InstrObjectConstructor
from traceparser import cleanTrace
from drawcalls import TemplateWebGLWrite
from traceneutronrayparser import NeutronRayConstructor, TraceNeutronRayParser

sys.path.append(os.path.join(os.path.dirname(__file__), '..', '..'))

def main(args):
    logging.basicConfig(level=logging.INFO)
    
    # read data from stdin
    print "reading data..."
    instr, display, rays, comments = cleanTrace(read())
    
    # parse 
    print "parsing data..."
    instrparser = TraceInstrParser(instr + display)
    rayparser = TraceNeutronRayParser(rays)

        # build the instrument object
    instrbuilder = InstrObjectConstructor(instrparser.parsetree)
    instrument = instrbuilder.build_instr()
    
    # build the neutron ray tree
    raybuilder = NeutronRayConstructor(rayparser.parsetree)
    rays = raybuilder.build_rays()
    
    instrument.rays = rays
    
    # build html
    outfile = 'mcdisplay.html'
    templatefile = os.path.join(os.path.dirname(__file__), "template.html")
    
    writer = TemplateWebGLWrite(instrument, templatefile)
    writer.build()
    writer.save(outfile)
    webbrowser.open_new_tab(outfile)

def read():
    data = ''
    for line in sys.stdin:
        data += line.strip() + '\n'
    return data

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    #parser.add_argument('data', help='--trace data file.')

    args = parser.parse_args()
    main(args)
