#!/usr/bin/env python3
'''
matplotlib mcplot implementation
'''

import argparse
import logging
import os
import sys
import math
import numpy as np
import matplotlib 

sys.path.append(os.path.join(os.path.dirname(__file__), '..', '..'))

from mccodelib.mcplotloader import McCodeDataLoader, test_decfuncs
from mccodelib.plotgraph import PlotGraphPrint

from mccodelib.plotgraph import PNSingle
from mccodelib.mcplotloader import Data1D, Data2D
from mccodelib.mccode_config import get_mccode_prefix
filenamebase=get_mccode_prefix() + 'plot'


FONTSIZE = 10

# Global placeholder for later import of pylab inside main()
# - necessary due to use of matplotlib.use() which has to happen
# before import of pylab
pylab=0


def plot_single_data(node, i, n, log):
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

    verbose = n == 1

    if type(data) is Data1D:
        ''' plot 1D data '''
        xmin = data.xlimits[0]
        xmax = data.xlimits[1]
        pylab.xlim(xmin,xmax)

        x = np.array(data.xvals).astype(float)
        y = np.array(data.yvals).astype(float)
        yerr = np.array(data.y_err_vals).astype(float)

        ylabel = data.ylabel
        if log == True:
            # handle Log intensity
            invalid = np.where(y <= 0)
            valid = np.where(y > 0)
            min_valid = np.min(y[valid])
            y[invalid] = min_valid/10
            yerr=yerr / y
            y=np.log(y)
            ylabel ="log(" + data.ylabel +")"

        pylab.errorbar(x, y, yerr)

        pylab.xlabel(data.xlabel, fontsize=FONTSIZE, fontweight='bold')
        pylab.ylabel(ylabel, fontsize=FONTSIZE, fontweight='bold')
        try:
            title = '%s [%s]\n%s\nI = %s Err = %s N = %s; %s' % (data.component, data.filename, data.title, data.values[0], data.values[1], data.values[2], data.statistics)
        except:
            title = '%s\n[%s]' % (data.component, data.filename)
        pylab.title(title, fontsize=FONTSIZE, fontweight='bold')

    elif type(data) is Data2D:
        ''' plot 2D data '''
        zvals = np.array(data.zvals)

        xmin = data.xlimits[0]
        xmax = data.xlimits[1]
        ymin = data.xlimits[2]
        ymax = data.xlimits[3]

        mysize=zvals.shape
        x = pylab.linspace(xmin, xmax, mysize[1])
        y = pylab.linspace(ymin, ymax, mysize[0])
        pylab.xlim(xmin, xmax)
        pylab.ylim(ymin, ymax)

        if log == True:
            invalid = np.where(zvals <= 0)
            valid = np.where(zvals > 0)
            min_valid = np.min(zvals[valid])
            zvals[invalid] = min_valid/10
            zvals = np.log(zvals)

        ''' out-commented code: alternative plot types '''
        #from mpl_toolkits.mplot3d import Axes3D
        #ax = Axes3D(pylab.gcf())
        #pylab.contour(x,y,zvals)
        #ax.plot_surface(x,y,zvals)
        pylab.pcolor(x,y,zvals)
        pylab.colorbar()

        pylab.xlabel(data.xlabel, fontsize=FONTSIZE, fontweight='bold')
        pylab.ylabel(data.ylabel, fontsize=FONTSIZE, fontweight='bold')

        try:
            title = '%s\nI = %s' % (data.component, data.values[0])
            if verbose:
                title = '%s [%s]\n%s\nI = %s Err = %s N = %s; %s' % (data.component, data.filename, data.title, data.values[0], data.values[1], data.values[2], data.statistics)
        except:
            title = '%s\n[%s]' % (data.component, data.filename)
        pylab.title(title, fontsize=FONTSIZE, fontweight='bold')

    else:
        print("Unsuported plot data type %s" % type(data))

    return subplt


class McMatplotlibPlotter():
    ''' matplotlib plotting frontend '''
    def __init__(self, sourcedir, log):
        self.sourcedir = sourcedir
        self.event_dc_cid = None
        self.log = log


    def _flip_log(self):
        self.log = not self.log


    def _click_proxy(self, event):
        ''' state-updating proxy for click handler '''
        dc_cb = lambda: pylab.disconnect(self.event_dc_cid)
        click(event, subplts=self.subplts, click_cbs=self.click_cbs,
              ctrl_cbs=self.ctrl_cbs, back_cb=self.back_cb, dc_cb=dc_cb)


    def _keypress_proxy(self, event):
        ''' state-updating proxy for keypress handler '''
        keypress(event, back_cb=self.back_cb, replot_cb=self.replot_cb,
                 togglelog_cb=self._flip_log)


    def plot_node(self, node):
        ''' plot recursion '''
        # safety
        if not node:
            return

        # clear
        pylab.close()

        # plot data and keep subplots for the click area filter
        n = node.getnumdata()
        self.subplts = [plot_single_data(node, i, n, self.log) for i in range(n)]

        # create callbacks
        self.click_cbs = [lambda nde=n: self.plot_node(nde) for n in node.primaries]
        self.ctrl_cbs = [lambda nde=n: self.plot_node(nde) for n in node.secondaries]
        # we need root node back click to prompt a replot, rather than to be ignored,
        # because disconnect is always called by the click handler
        bck_node = node.parent if node.parent else node 
        self.back_cb = lambda n=bck_node: self.plot_node(n)
        self.replot_cb = lambda n=node: self.plot_node(n) if node else None

        # regiseter click events
        self.event_dc_cid = pylab.connect('button_press_event', self._click_proxy)

        # register keypress events
        pylab.connect('key_press_event', self._keypress_proxy)

        # show the plot
        pylab.show()


    def html_node(self, node, fileobj):
        '''  plots node and saves to html using mpld3 '''
        import mpld3
        
        n = node.getnumdata()
        self.subplts = [plot_single_data(node, i, n, self.log) for i in range(n)]
        
        mpld3.save_html(pylab.gcf(), fileobj)


def keypress(event, back_cb, replot_cb, togglelog_cb):
    key = event.key.lower()

    if key == 'q':
        quit()
    elif key == 'l':
        togglelog_cb()
        replot_cb()
    elif event.key == 'p':
        dumpfile('ps')
    elif event.key == 'd':
        dumpfile('pdf')
    elif event.key == 'n':
        dumpfile('png')
    elif event.key == 'j':
        dumpfile('jpg')
    elif key == 'b':
        back_cb()
    elif key == 'f5':
        replot_cb()
    elif key == 'f1' or key == 'h':
        print_help()
    elif key == 'b':
        back_cb()


def print_help(nogui=False):
    helplines = []
    helplines.append('')
    helplines.append('q              - quit')
    helplines.append('F1/h           - help')
    helplines.append('l              - toggle log')
    helplines.append('p              - save ps')
    helplines.append('d              - save pdf')
    helplines.append('n              - save png')
    helplines.append('j              - save jpg')
    helplines.append('s              - native save dialog')
    helplines.append('F5             - replot')
    helplines.append('click          - display subplot')
    helplines.append('right-click/b  - back')
    helplines.append('ctrl + click   - sweep monitors')
    print('\n'.join(helplines))


def dumpfile(frmat, filename = None):
    """ save current fig to softcopy """
    from pylab import savefig

    global filenamebase
    
    if filename is None:
        filename = filenamebase
        
    # get directory, basename and extension
    if isinstance(filename, list):
        filename = filename[0]
    basename  = os.path.splitext(filename)[0] # contains full path and base filename
    if frmat is None:
        ext       = os.path.splitext(filename)[1] # extension with the dot
    else:
        ext = frmat
    if ext[0] != '.':
        ext = '.'+ext
    # assemble target name
    saveto = basename + ext
    
    # Check for existance of earlier exports
    if os.path.isfile(saveto):
        index=1
        saveto = f"{basename}_{index}{frmat}"
        while os.path.isfile(saveto):
            index += 1
            saveto = f"{basename}_{index}{frmat}"

    savefig(saveto)
    print("Saved " + saveto)


def click(event, subplts, click_cbs, ctrl_cbs, back_cb, dc_cb):
    subplt = event.inaxes
    if not subplt:
        return False
    else:
        lclick = event.button==1
        rclick = event.button==3
        ctrlmod = (event.key == 'control' or event.key == 'cmd' or event.key == 'x')

        if lclick and len(click_cbs) > 0 and not ctrlmod:
            # left button
            idx = subplts.index(subplt)
            dc_cb()
            click_cbs[idx]()
        elif rclick and back_cb and not ctrlmod:
            # right button
            dc_cb()
            back_cb()
        elif ctrlmod and len(ctrl_cbs) > 0:
            # control-click
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

        if args.format or args.output:
            matplotlib.use('template')
                       
        if args.backend:
            try:
                matplotlib.use(args.backend)
            except Exception as e:
                print('backend use error: ' + e.__str__())
                return

        global pylab
        from matplotlib import pylab

        # load data
        loader = McCodeDataLoader(simfile=simfile)
        try:
            loader.load()
        except Exception as e:
            # invallid input case:
            print('mcplot loader: ' + e.__str__())
            print_help(nogui=True)
            quit()
        rootnode = loader.plot_graph
        if args.test:
            PlotGraphPrint(rootnode)

        # start the plotter
        plotter = McMatplotlibPlotter(sourcedir=loader.directory,log=args.log)

        if (sys.platform == "linux" or sys.platform == "linux2") & args.html:
            # save to html and exit
            plotter.html_node(rootnode, open('%s.html' % os.path.splitext(simfile)[0], 'w'))
        else:
            # display gui / prepare graphics dump
            print_help(nogui=True)
            plotter.plot_node(rootnode)
        
        if args.output or args.format:
            try:
                dumpfile(args.format, args.output)
            except Exception as e:
                print('dumpfile issue: ' + e.__str__())

    except KeyboardInterrupt:
        print('keyboard interrupt')
    except Exception as e:
        print('mcplot error: %s' % e.__str__())
        raise e


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('simulation', nargs='*', 
        help='file or directory to plot')
    parser.add_argument('-t', '--test',  action='store_true', default=False, 
        help='mccode data loader test run')
    parser.add_argument('--html', action='store_true', 
        help='save plot to html using mpld3 (linux only)')
    parser.add_argument('--format', dest='format', 
        help='save plot to pdf/png/eps/svg... without bringing up window')
    parser.add_argument('--output',nargs=1, dest='output', default=None,
        help='save plot to given file without bringing up window. Extension (e.g. pdf/png/eps/svg) can be specified in the file name or --format')
    parser.add_argument('--log', action='store_true', 
        help='initiate plot(s) with log of signal')
    parser.add_argument('--backend', dest='backend', 
        help='use non-default backend for matplotlib plot')

    args = parser.parse_args()

    main(args)
