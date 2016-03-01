#!/usr/bin/env python
# -*- coding: utf-8 -*-
import logging
import argparse
import sys
from os.path import join, dirname

# get access to mcgui folder
sys.path.append(join(dirname(__file__), '..'))
from mcgui import mcguiutils, mcfileutils

def parsecomps_test(comproot, mcdisplay):
    ''' parse components in comproot and (optinally) print mcdisplay section  '''
    instr, comp = mcguiutils.McGuiUtils.getInstrumentAndComponentFiles(comproot)
    # '/home/jaga/source/McCode/mcstas-comps'
    errors = []
    for c in comp: 
        parser = mcfileutils.McComponentParser(c)
        try:
            parser.parse()
            parser.parseDisplaySection()
        except Exception as e:
            errors.append('error (%s): %s' % (parser.file, e.__str__()))
        
        if mcdisplay:
            print('********************')
            print('* %s' % parser.file)
            print('********************')
            print('\n%s\n' % parser.mcdisplay)
        
    for e in errors:
        print e

def main(args):
    logging.basicConfig(level=logging.INFO)
    
    # TODO: log test sequence
    parsecomps_test(args.comproot[0], args.mcdisplay)
    

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--mcdisplay', action='store_true', help='print mcdisplay sections flag' )
    parser.add_argument('comproot', nargs=1, help='components root dir dir' )

    args = parser.parse_args()

    main(args)
