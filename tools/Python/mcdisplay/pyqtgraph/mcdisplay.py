#!/usr/bin/env python3
# -*- coding: utf-8 -*-
'''
mcdisplay webgl script.
'''
import sys
import os
import logging
import argparse
import json
import subprocess
from datetime import datetime

sys.path.append(os.path.join(os.path.dirname(__file__), '..', '..'))

from mccodelib import mccode_config
from mccodelib.mcdisplayutils import McDisplayReader
from mccodelib.instrgeom import Vector3d
   
def get_datadirname(instrname):
    ''' returns an mcrun-like name-date-time string '''
    return "%s_%s" % (instrname, datetime.strftime(datetime.now(), "%Y%m%d_%H%M%S"))

def file_save(data, filename):
    ''' saves data for debug purposes '''
    f = open(filename, 'w')
    f.write(data)
    f.close()

def main(args):
    logging.basicConfig(level=logging.INFO)
    debug = False
    
    # output directory
    dirname = get_datadirname(os.path.splitext(os.path.basename(args.instr))[0])
    if args.dirname:
        dirname = args.dirname
    
    # set up a pipe, read and parse the particle trace
    reader = McDisplayReader(args, n=100, dir=dirname, debug=debug)
    instrument = reader.read_instrument()
    instrument.setCmd(reader.cmd)
    raybundle = reader.read_particles()

    # write output
    
    if debug:
        # this should enable template.html to load directly
        jsonized = json.dumps(instrument.jsonize(), indent=0)
        file_save(jsonized, 'jsonized.json')

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('instr', help='display this instrument file (.instr or .out)')
    parser.add_argument('--default', action='store_true', help='automatically use instrument defaults for simulation run')
    parser.add_argument('--nobrowse', action='store_true', help='do not open a webbrowser viewer')
    parser.add_argument('--dirname', help='name of the output directory requested to mcrun')
    parser.add_argument('--inspect', help='display only particle rays reaching this component passed to mcrun')
    parser.add_argument('--first', help='zoom range first component')
    parser.add_argument('--last', help='zoom range last component')
    parser.add_argument('instr_options', nargs='*', help='simulation options and instrument params')
    
    args, unknown = parser.parse_known_args()
    # if --inspect --first or --last are given after instr, the remaining args become "unknown",
    # but we assume that they are instr_options
    if len(unknown)>0:
        args.instr_options = unknown
    
    try:
        main(args)
    except KeyboardInterrupt:
        print('')
    except Exception as e:
        print(e)
        raise e

