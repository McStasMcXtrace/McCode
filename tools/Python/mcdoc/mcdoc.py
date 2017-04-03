#!/usr/bin/env python
# -*- coding: utf-8 -*-
import logging
import argparse
import sys
import os
import re

sys.path.append(os.path.join(os.path.dirname(__file__), '..'))
from mccodelib import utils, mccode_config

class InstrParInfo:
    ''' Component parameter info, used as McComponentParser.pars '''
    def __init__(self, info=None):
        if info:
            self.par_name = info.par_name
            self.type = info.type
            self.default_value = info.default_value
            self.doc_and_unit = info.doc_and_unit
        else:
            self.par_name = ''       # parameter par_name
            self.type = ''           # can be "string" or "int", but is mostly empty
            self.default_value = ''
            self.doc_and_unit = ''   # doc string and unit (no linebreaks)

class InstrParser:
    ''' parses an instr file, extracting all relevant information into python '''
    def __init__(self, instr_file):
        self.instr_file = instr_file
        
        self.author = None
        self.date = None
        self.origin = None
        self.release = None
        self.version = None
        
        self.site = None
        
        self.description = None
        self.example = None
        
        self.parameters = None
        self.instrname = None
    
    def parse(self):
        try:
            self._parse()
        except:
            self._parse_legacy()
        
    def _parse(self):
        raise Exception()
    
    def _parse_legacy(self):
        ''' parses the given instr file '''
        
        f = open(self.instr_file)
        logging.debug('parsing file "%s"' % self.instr_file)
        
        header = utils.read_header(f)
        info = utils.parse_instr_header(header)
        info.site = utils.get_instr_site_fromtxt(header)
        info.name, info.params = utils.parse_define_instr(utils.read_define_instr(f))
        
        print(info)

def test():
    text = open('/home/jaga/test/simulations/ISIS_SANS2d.instr').read()
    #print(text)
    
    #ip = InstrParser('/home/jaga/test/simulations/ILL_IN5.instr')
    #ip = InstrParser('/home/jaga/test/simulations/ILL_IN5_mod01.instr')
    ip = InstrParser('/usr/share/mcstas/2.3/examples/template_header_simple.instr')
    #ip = InstrParser('/usr/share/mcstas/2.3/examples/Test_Sample_nxs_diffraction.instr')
    
    ip.parse()
    
    quit()

def main(args):
    logging.basicConfig(level=logging.INFO)
    locdir = args.localdir or '.'
    print(locdir)
    
    # get lib dir
    lib_instr_files, lib_comp_files = utils.get_instr_comp_files(mccode_config.configuration["MCCODE_LIB_DIR"])
    local_instr_files, local_comp_files = utils.get_instr_comp_files(locdir)
    
    for f in lib_instr_files:
        try:
            print("parsing... %s" % f)
            print(InstrParser(f).parse())
        except:
            print("failed parsing instr file: %S" % f)
            quit()
    
    print("parsed instr files: %s" % str(len(lib_instr_files)))

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('localdir', nargs='?', help='local dir to parse (in addition to lib dir)')
    args = parser.parse_args()
    
    #test()
    main(args)

