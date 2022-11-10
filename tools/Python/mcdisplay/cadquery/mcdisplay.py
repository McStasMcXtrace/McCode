#!/usr/bin/env python3
# -*- coding: utf-8 -*-
'''
mcdisplay pyqtgraph script.
'''
import sys
import logging
import json
import subprocess
from pathlib import Path

sys.path.append(str(Path(__file__).resolve().parent.parent.parent))

from mccodelib import utils
from mccodelib.mcdisplayutils import McDisplayReader
from mccodelib.instrgeom import Vector3d, DrawLine, DrawMultiline, DrawCircle
from mccodelib.instrparser import InstrTraceParser, InstrObjectConstructor
from mccodelib.fcparticleparser import FlowChartParticleTraceParser

# colour stuff
colour_idx = 0
def get_next_colour(colour_idx):
    colour = colours[colour_idx % len(colours)]
    return colour

colours = [(248, 0, 0), (0, 248, 0), (0, 0, 248), (0, 248, 248), (248, 0, 248), (0, 248, 128), (248, 248, 0), (248, 128, 0), (128, 248, 0), (0, 128, 248), (128, 0, 248), (248, 0, 128), (168, 168, 168)]

def debug_load_instr(filename):
    instrdef = open(filename).read()
    
    instrparser = InstrTraceParser(instrdef)
    instrbuilder = InstrObjectConstructor(instrparser.parsetree)
    return instrbuilder.build_instr()

def debug_load_rays(filename):
    particles = open(filename).read()
    
    parser = FlowChartParticleTraceParser()
    rays = parser.execute(particles)
    return rays

def get_datadirname(instrname):
    ''' returns an mcrun-like name-date-time string '''
    return "%s_%s" % (instrname, datetime.strftime(datetime.now(), "%Y%m%d_%H%M%S"))

def debug_file_save(data, filename):
    ''' saves data for debug purposes '''
    with open(filename, 'w') as f:
        f.write(data)


def bigprint(info):
    print("-"*80)
    print(info)
    print("-"*80)

def main(instr=None, dirname=None, **kwds):
    ''' script execution '''
    logging.basicConfig(level=logging.INFO)

    # output directory
    if dirname is None:
        from datetime import datetime as dt
        p = Path(instr).resolve()
        dirname = str(p.parent.joinpath(f"{p.stem}_{dt.strftime(dt.now(), '%Y%m%d_%H%M%S')}"))
    
    bigprint("Starting reader")
    reader = McDisplayReader(dir=dirname, instr=instr, **kwds)
    bigprint("Starting instrument reader")
    instrument = reader.read_instrument()
    bigprint("Starting raybundle reader")
    raybundle = reader.read_particles()

    print("everything is A. OK.")



if __name__ == '__main__':
    from mccodelib.mcdisplayutils import make_common_parser
    # Only pre-sets instr, --default, options
    parser, prefix = make_common_parser(__file__, __doc__)

    parser.add_argument('--dirname', help='output directory name override')
    parser.add_argument('-n', '--ncount', dest='n', type=int, default=0, help='Number of particles to simulate')

    args, unknown = parser.parse_known_args()
    # if --inspect --first or --last are given after instr, the remaining args become "unknown",
    # but we assume that they are instr_options
    args = {k: args.__getattribute__(k) for k in dir(args) if k[0] != '_'}
    if len(unknown):
        args['options'] = unknown

    # enable ^C termination
    import signal
    signal.signal(signal.SIGINT, signal.SIG_DFL)

    try:
        main(**args)
    except KeyboardInterrupt:
        print('')

