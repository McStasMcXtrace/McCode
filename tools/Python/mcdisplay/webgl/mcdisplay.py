#!/usr/bin/env python
# -*- coding: utf-8 -*-
'''
mcdisplay webgl script.
'''
#import sys
import os
import webbrowser
import logging
import argparse

from pipetools import McrunPipeMan
from traceinstrparser import TraceInstrParser, InstrObjectConstructor
from traceparser import cleanTrace
from drawcalls import TemplateWebGLWrite
from traceneutronrayparser import NeutronRayConstructor, TraceNeutronRayParser

#sys.path.append(os.path.join(os.path.dirname(__file__), '..', '..'))
#from mclib import mccode_config

def parse_rays(rays_txt):
    '''  '''
    rayparser = TraceNeutronRayParser(rays_txt)
    
    # build the neutron ray tree
    raybuilder = NeutronRayConstructor(rayparser.parsetree)
    rays = raybuilder.build_rays()
    return rays

def parse_instrdef(instr_txt, display_txt):
    '''  '''
    print "parsing data..."
    instrparser = TraceInstrParser(instr_txt + display_txt)

    # build the instrument object
    instrbuilder = InstrObjectConstructor(instrparser.parsetree)
    instrument = instrbuilder.build_instr()
    return instrument

def write_oldhtml(instrument):
    '''  '''
    outfile = 'mcdisplay.html'
    templatefile = os.path.join(os.path.dirname(__file__), "template.html")
    
    writer = TemplateWebGLWrite(instrument, templatefile)
    writer.build()
    writer.save(outfile)
    webbrowser.open_new_tab(outfile)

def read(cmd):
    ''' reads trace output given by command 'cmd' '''
    pipeman = McrunPipeMan(cmd)
    pipeman.start_pipe()
    pipeman.join()
    return pipeman.readall()

def main(args):
    logging.basicConfig(level=logging.INFO)
    
    # test strings:
    #data = read('mcrun ESS_Brilliance_2013.instr --trace -n1000')
    #data = read('mcrun PSI_DMC.instr --trace -n10')
    
    # assemble cmd
    data = read('mcrun ' + args.instr + ' --trace -n100')
    
    instr, display, rays, comments = cleanTrace(data)
    print comments
    
    instrument = parse_instrdef(instr, display)
    instrument.rays = parse_rays(rays)
    #write_oldhtml(instrument)

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('instr', help='display this instrument file (.instr or .out)')
    parser.add_argument('--inspect', help='display only neutrons reaching this component passed to mcrun')
    parser.add_argument('--first', help='zoom range first component')
    parser.add_argument('--last', help='zoom range last component')
    parser.add_argument('instr_options', nargs='*', help='simulation options and instrument params')
    
    args, unknown = parser.parse_known_args()
    # if --inspect --first or --last are given after instr, the remaining args become "unknown",
    # but we assume that they are instr_options
    if len(unknown)>0:
        args.instr_options = unknown
    
    main(args)

