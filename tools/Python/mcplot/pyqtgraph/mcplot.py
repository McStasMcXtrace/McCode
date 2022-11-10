#!/usr/bin/env python3
'''
pyqtgraph mcplot frontend, which uses the mcplot graph-based data loader.
'''
import argparse
import logging
import os
import sys

import plotfuncs

sys.path.append(os.path.join(os.path.dirname(__file__), '..', '..'))

from mccodelib.mcplotloader import McCodeDataLoader, test_decfuncs
from mccodelib.plotgraph import PlotGraphPrint
from mccodelib import pqtgfrontend


def main(args):
    ''' load data from mcplot backend and send it to the pyqtgraph frontend above '''
    logging.basicConfig(level=logging.INFO)
    
    # ensure keyboardinterrupt ctr-c
    import signal
    signal.signal(signal.SIGINT, signal.SIG_DFL)
    try:
        if len(args.simulation) == 0:
            simfile = ''
        else:
            simfile = args.simulation[0]
        if args.test:
            test_decfuncs(simfile)
        
        # load data
        loader = McCodeDataLoader(simfile=simfile)
        try:
            loader.load()
        except Exception as e:
            # invallid input case:
            print('mcplot loader: ' + e.__str__())
            print(pqtgfrontend.get_help_string())
            quit()
        graph = loader.plot_graph
        if args.test:
            printer = PlotGraphPrint(graph)
        
        # run pqtg frontend
        plotter = pqtgfrontend.McPyqtgraphPlotter(
            graph, sourcedir=loader.directory,
            plot_func=plotfuncs.plot, invcanvas=args.invcanvas)
        print(pqtgfrontend.get_help_string())
        plotter.runplot()
        
    except KeyboardInterrupt:
        print('keyboard interrupt')
    except Exception as e:
        print('mcplot error: %s' % e.__str__())
        raise e


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('simulation', nargs='*', help='file or directory to plot')
    parser.add_argument('-t', '--test',  action='store_true', default=False, help='mccode data loader test run')
    parser.add_argument('--invcanvas', action='store_true', help='invert canvas background from black to white')
    args = parser.parse_args()

    main(args)

