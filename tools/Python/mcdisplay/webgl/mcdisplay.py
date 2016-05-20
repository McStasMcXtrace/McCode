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

from pipetools import McrunPipeMan, cleanTrace
from traceinstrparser import TraceInstrParser, InstrObjectConstructor
from drawcalls import TemplateWebGLWrite, calcLargestBoundingVolumeWT
from traceneutronrayparser import NeutronRayConstructor, TraceNeutronRayParser
from instrrep import Vector3d, Transform

#sys.path.append(os.path.join(os.path.dirname(__file__), '..', '..'))
#from mclib import mccode_config

def write_oldhtml(instrument):
    ''' writes instrument definition to html/js '''
    # calculate campost by means of the component bounding boxes (mediated by drawcalls)
    drawcalls = []
    for comp in instrument.components:
        transform = Transform(comp.rot, comp.pos)
        for drawcall in comp.drawcommands:
            drawcalls.append((drawcall, transform))
    box = calcLargestBoundingVolumeWT(drawcalls)
    x = -(box.z2 - box.z1)/2
    y = 0
    z = box.z1 + (box.z2 - box.z1)/2
    
    outfile = 'mcdisplay.html'
    templatefile = os.path.join(os.path.dirname(__file__), "template.html")
    
    writer = TemplateWebGLWrite(instrument, templatefile, campos=Vector3d(x, y, z))
    writer.build()
    writer.save(outfile)
    webbrowser.open_new_tab(outfile)

def write_instrument():
    ''' writes instrument definitions to html/ js '''
    # TODO: implement
    return

def write_neutrons():
    ''' writes neutron ray definitions to html/ js '''
    # TODO: implement
    return

class McMicsplayReader(object):
    ''' 
    High-level trace output reader
    '''
    pipeman = None
    cmd = ''
    debug = None
    def __init__(self, args, n=None, debug=False):
        ''' supported args: instr, inspect, default, instr_options '''
        if not os.path.exists(args.instr) or not os.path.splitext(args.instr)[1] not in ['instr', 'out']:
            print "Please supply a valid .instr or .out file."
            exit()
        
        cmd = 'mcrun ' + args.instr + ' --trace'
        if n: 
            cmd = cmd + ' -n' + str(n)
        if args.instr_options: 
            cmd = cmd + ' ' + args.instr_options
        
        self.debug = debug
        
        self.cmd = cmd
        self.pipeman = McrunPipeMan(cmd, inspect=args.inspect, send_enter=args.default)
    
    def read_instrument(self):
        ''' starts a pipe to mcrun given cmd, waits for instdef and reads, returning the parsed instrument '''
        self.pipeman.start_pipe()
        self.pipeman.join_instrdef()

        instrdef = self.pipeman.read_instrdef()
        instr, display, rays, comments = cleanTrace(instrdef)
        print comments
        
        instrparser = TraceInstrParser(instr + display)
        instrbuilder = InstrObjectConstructor(instrparser.parsetree)
        instrument = instrbuilder.build_instr()
        
        if self.debug:
            debug_save(instrdef, 'instrdata')
            debug_save('\n\nINSTR:\n\n' + instr + '\n\nDISPLAY:\n\n' + display + '\n\nCOMMENTS:\n\n' + comments, 'instrdata_cleaned')
        
        return instrument
    
    def read_neutrons(self):
        ''' waits for pipeman object to finish, then read and parse neutron data '''
        print "reading neutron data..."
        self.pipeman.join()
        neutrons = self.pipeman.read_neutrons()
        instr, display, rays_str, comments = cleanTrace(neutrons)
        print comments
        
        if self.debug:
            debug_save(neutrons, 'neutrondata')
            debug_save('\n\NEUTRONS:\n\n' + rays_str + '\n\nCOMMENTS:\n\n' + comments, 'neutrondata_cleaned')
        
        rayparser = TraceNeutronRayParser(rays_str)
        raybuilder = NeutronRayConstructor(rayparser.parsetree)
        rays = raybuilder.build_rays()
        
        return rays

def debug_save(data, filename):
    ''' saves data for debug purposes '''
    f = open(filename, 'w')
    f.write(data)
    f.close()

def main(args):
    logging.basicConfig(level=logging.INFO)
    
    # TODO: implement --default (also disables instr_options)
    # TODO: implement first, last
    
    reader = McMicsplayReader(args, n=300)
    instrument = reader.read_instrument()
    #write_instrument(instrument)
    instrument.rays = reader.read_neutrons()
    #write_neutrons(rays)
    
    write_oldhtml(instrument)
    

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('instr', help='display this instrument file (.instr or .out)')
    parser.add_argument('--default', '-d', action='store_true', help='use instrument defaults (fast)')
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

