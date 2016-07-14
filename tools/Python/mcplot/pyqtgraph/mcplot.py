#!/usr/bin/env python
'''
pyqtgraph based mcplot frontend
'''
import argparse
import logging
import os
import sys

sys.path.append(os.path.join(os.path.dirname(__file__), '..', '..'))
from mclib import mccode_config
from mclib.mcplotloader import McPlotDataLoader

def main(args):
    logging.basicConfig(level=logging.INFO)
    
    if len(args.simulation) == 0:
        simfile = ''
    else:
        simfile = args.simulation[0]
    
    loader = McPlotDataLoader(simfile=simfile)
    loader.load()
    
    print "mcplot-pyqtgraph"

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('simulation', nargs='*', help='file to plot')
    #parser.add_argument('-s', '--save',  action='store_true', default=False, help='save to file, but don\'t plot (defaults to png)')
    args = parser.parse_args()

    main(args)
