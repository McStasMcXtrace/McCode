'''
Intermediary layer for setting up a --trace pipe and interpreting the results.
'''
import os
import re
from pathlib import Path

from .pipetools import McrunPipeMan
from .instrparser import InstrTraceParser, InstrObjectConstructor
from .particleparser import ParticleBundleRayFactory, ParticleTraceParser
from .fcparticleparser import FlowChartParticleTraceParser
from . import mccode_config

class McDisplayReader(object):
    ''' High-level trace manager '''
    def __init__(self, instr=None, inspect=None, default=None, n=None, dir=None, debug=False, options=None, **kwds):
        ext = mccode_config.platform["EXESUFFIX"]

        if instr is None or ('.instr' not in instr and ext not in instr):
            print(f"A .instr or compiled instrument file ({ext}) is required")
            exit()
        
        # assemble command
        mcruncmd = str(Path(mccode_config.directories['bindir'],mccode_config.configuration['MCRUN']))
        
        cmd = f"{mcruncmd} {instr} --no-output-files --trace --ncount={300 if n is None else n}"

        if dir:
            cmd = cmd + ' --dir=' + dir
        if options is not None:
            for option in options:
                cmd = f"{cmd} {option}"
        
        self.debug = debug
        self.count = n
        self.cmd = cmd
        self.pipeman = McrunPipeMan(cmd, inspect=inspect, send_enter=False if default is None else default)
    
    def read_instrument(self):
        ''' starts a pipe to mcrun given cmd, waits for instdef and reads, returning the parsed instrument '''
        self.pipeman.start_pipe()
        self.pipeman.join()
        
        instrdef = self.pipeman.read_instrdef()
        
        if self.debug:
            file_save(instrdef, 'instrdata')
        
        instrparser = InstrTraceParser(instrdef)
        instrbuilder = InstrObjectConstructor(instrparser.parsetree)
        instrument = instrbuilder.build_instr()
        
        instrument.set_cmd(self.cmd)
        
        return instrument
    
    def read_particles(self):
        ''' waits for pipeman object to finish, then read and parse neutron data '''
        rays = None
        if self.count > 0:
            print("reading particle data...")
            particles = self.pipeman.read_particles()
            print(self.pipeman.read_comments())
            if self.debug:
                file_save(particles, 'particledata')
            parser = FlowChartParticleTraceParser()
            rays = parser.execute(particles)
        return rays

def file_save(data, filename):
    ''' saves data for debug purposes '''
    f = open(filename, 'w')
    f.write(data)
    f.close()

def make_common_parser(filename, documentation):
    """ Create an argparse.ArgumentParser with arguments (instr, --default, options)"""
    from pathlib import Path
    scriptname = Path(filename).stem
    #possibly replace the mc prefix in the description string
    from argparse import ArgumentParser

    ext = mccode_config.platform["EXESUFFIX"]
    prefix=scriptname[:2]

    parser = ArgumentParser(description=documentation.replace('mcdisplay',scriptname))
    parser.add_argument('instr', help=f'display this instrument file (.instr or .{ext})')
    parser.add_argument('--default', action='store_true', help='automatically use instrument defaults for simulation run')
    parser.add_argument('options', nargs='*', help='simulation options and instrument params')

    return parser, prefix
