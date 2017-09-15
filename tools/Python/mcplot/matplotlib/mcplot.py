#!/usr/bin/env python
'''
matplotlib mcplot impl
'''
import argparse
import logging
import os
import sys
import math
from matplotlib import pylab

import plotfuncs
from plotfuncs import plot_Data1D

sys.path.append(os.path.join(os.path.dirname(__file__), '..', '..'))

from mccodelib.mcplotloader import McCodeDataLoader, test_decfuncs
from mccodelib.plotgraph import PlotGraphPrint
from mccodelib import pqtgfrontend

from mccodelib.plotgraph import PNSingle
from mccodelib.mcplotloader import Data1D, Data2D

def plot_single_data(node, i, n, opts=None):
    ''' plot the data of node, at index i, to a subplot '''
    def calc_panel_size(nfigs):
        nx = int(math.sqrt(nfigs*1.61803398875)) # golden ratio
        ny = nfigs // nx + int(bool(nfigs % nx))
        return nx, ny
    
    if type(node) == PNSingle and i != 0:
        raise Exception("inconsistent plot request, idx=%s" % str(i))
    
    dims = calc_panel_size(n)
    subplt = pylab.subplot(dims[1],dims[0],i+1)
    
    data = node.getdata_idx(i)

    pylab.title('%d' % i)
    
    if type(data) is Data1D:
        pass
    elif type(data) is Data2D:
        pass
    else:
        raise Exception("unknown plot data type")
    
    return subplt

class McMatplotlibPlotter():
    ''' matplotlib plotting frontend '''
    def __init__(self, sourcedir, invcanvas):
        self.sourcedir = sourcedir
        self.event_dc_cid = None
    
    def _click_proxy(self, event):
        ''' state-updating proxy for click event handler '''
        dc_cb = lambda: pylab.disconnect(self.event_dc_cid)
        click(event, subplts=self.subplts, click_cbs=self.click_cbs, ctrl_cbs=self.ctrl_cbs, back_cb=self.back_cb, dc_cb=dc_cb)
    
    def plot_node(self, node):
        '''  '''
        if not node:
            return
        
        # plot data and keep subplots for the click area filter
        n = node.getnumdata()
        self.subplts = [plot_single_data(node, i, n) for i in range(n)]
        
        # click handlers
        self.click_cbs = [lambda: self.plot_node(node=node) for node in node.primaries]
        self.ctrl_cbs = [lambda: self.plot_node(node=node) for node in node.secondaries]
        self.back_cb = None
        if node.parent:
            self.back_cb = lambda: self.plot_node(node=node.parent)
        self.event_dc_cid = pylab.connect('button_press_event', self._click_proxy)
        
        # set keypress handlers 
        #replot_cb = lambda: plot_node(node)
        #back_cb = lambda: plot_node(node.parent)
        #set_keyhandler(layout.scene(), replot_cb, back_cb, 'l', get_modifiers("none"), viewmodel=viewmodel)
        
        pylab.show()

def click(event, subplts, click_cbs, ctrl_cbs, back_cb, dc_cb):
    ''' universal click handler '''
    subplt = event.inaxes
    if not subplt:
        return False
    else:
        lclick = event.button==1
        rclick = event.button==3
        ctrlmod = event.key == 'control'
        
        if lclick and len(click_cbs) > 0 and not ctrlmod:
                idx = subplts.index(subplt)
                dc_cb()
                click_cbs[idx]()
        elif rclick and back_cb: # right button
            dc_cb()
            back_cb()
        elif ctrlmod and len(ctrl_cbs) > 0 and ctrlmod:
            idx = subplts.index(subplt)
            dc_cb()
            ctrl_cbs[idx]()

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
            pqtgfrontend.print_help(nogui=True)
            quit()
        rootnode = loader.plot_graph
        if args.test:
            PlotGraphPrint(rootnode)
        
        # run pqtg frontend
        plotter = McMatplotlibPlotter(sourcedir=loader.directory, invcanvas=args.invcanvas)
        pqtgfrontend.print_help(nogui=True)
        plotter.plot_node(rootnode)
        
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

