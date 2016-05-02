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
from traceparser import TraceParser, InstrProduction
from drawcalls import TemplateWebGLWrite

sys.path.append(os.path.join(os.path.dirname(__file__), '..', '..'))


def main(args):
    logging.basicConfig(level=logging.INFO)
    
    # read data from stdin
    print "reading data..."
    data = read()
    
    # build trace parser and parse data
    print "parsing data..."
    parser = TraceParser(data)
    
    # build instrument data object
    instrbuilder = InstrProduction(parser.parsetree)
    instrbuilder.build()
    
    # build html
    templatefile = os.path.join(os.path.dirname(__file__), "template.html")
    outfile = 'mcdisplay.html'
    writer = TemplateWebGLWrite(instrbuilder.instrument_tree, templatefile)
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
