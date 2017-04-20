'''
Intermediary layer for setting up a --trace pipe and interpreting the results.
'''
import os
import re

from .pipetools import McrunPipeMan
from .instrparser import InstrTraceParser, InstrObjectConstructor
from .particleparser import ParticleBundleRayFactory, ParticleTraceParser
from .fcparticleparser import FlowChartParticleTraceParser
from . import mccode_config

class McDisplayReader(object):
    ''' High-level trace manager '''
    def __init__(self, args, n=None, dir=None, debug=False):
        '''
        supported args: 
            instr 
            inspect
            default
            instr_options
            
        NOTE: n is only applied to the command string if neither
         '-n' nor '--ncount' strings are found in args.instr_options
        '''
        if not os.path.exists(args.instr) or not os.path.splitext(args.instr)[1] not in ['instr', 'out']:
            print("Please supply a valid .instr or .out file.")
            exit()
        
        # assemble command
        cmd = mccode_config.configuration["MCRUN"] + ' ' + args.instr + ' --no-output-files --trace'
        b1 = False
        b2 = False
        for o in args.instr_options:
            b1 = bool(re.search('--ncount', o)) or b1
            b2 = bool(re.search('-n', o)) or b2
        if not b1 and not b2:
            if not n:
                cmd = cmd + ' --ncount=' + str(100)
            else:
                cmd = cmd + ' --ncount=' + str(n)
        else:
            n = None
        
        if dir:
            cmd = cmd + ' --dir=' + dir
        if args.instr_options:
            for o in args.instr_options:
                cmd = cmd + ' ' + o
        
        self.args = args
        self.n = n
        self.dir = dir
        self.debug = debug
        self.cmd = cmd
        self.pipeman = McrunPipeMan(cmd, inspect=args.inspect, send_enter=args.default)
    
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
