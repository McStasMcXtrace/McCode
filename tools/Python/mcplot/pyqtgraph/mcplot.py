#!/usr/bin/env python
'''
pyqtgraph based mcplot frontend
'''
import argparse
import logging
import os
import sys

sys.path.append(os.path.join(os.path.dirname(__file__), '..', '..'))
#from mclib import mccode_config
from mclib.mcplotloader import McPlotDataLoader, test_decfuncs
from mclib.mcplotgraph import PlotGraphPrint

def main(args):
    logging.basicConfig(level=logging.INFO)
    
    try:
        
        if len(args.simulation) == 0:
            simfile = ''
        else:
            simfile = args.simulation[0]
    
        if args.test:
            test_decfuncs(simfile)
        
        loader = McPlotDataLoader(simfile=simfile)
        loader.load()
        graph = loader.plot_graph
    
        if args.test:
            printer = PlotGraphPrint(graph)
    
    except Exception as e:
        print 'mcplot error: %s' % e.message
    
    # TODO: plot the generated plot-graph
    
if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('simulation', nargs='*', help='file or directory to plot')
    parser.add_argument('-t', '--test',  action='store_true', default=False, help='mccode data loader test run')
    args = parser.parse_args()

    main(args)
