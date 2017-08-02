#!/usr/bin/env python
'''
mcplot script for testing the ifit interface.
'''
import argparse
import logging
import os
import sys

import matlab.engine # official mathworks impl

import numpy as np
import pyqtgraph as pg

sys.path.append(os.path.join(os.path.dirname(__file__), '..', '..'))
from mccodelib import pqtgfrontend
from mccodelib import plotgraph

#import matlab_ef # Emmanuel Farhi's matlab interface


'''
Local plotting functions
'''
def plot_iData_1D(data, plt, log=False, legend=True, icolormap=0, verbose=True, legend_fontsize=10):
    ''' create a plotItem and populate it with data, Data1D '''
    # data
    x = np.array(data.axes_vals).astype(np.float)
    y = np.array(data.signal).astype(np.float)
    e = np.array(data.error).astype(np.float)

    if log:
        nonzeros=[]
        zeros=[]
        for i in range(len(y)):
            if y[i]>0:
                nonzeros.append(i)
            else:
                zeros.append(i)
        if len(nonzeros)>0:
            y[zeros] = np.min(y[nonzeros])/10
            plt.setLogMode(y=True)
        else:
            plt.setLogMode(y=False)
    else:
        plt.setLogMode(y=False)

    plt.setXRange(np.min(x), np.max(x), padding=0)

    # labels
    #plt.setLabels(title=" ",bottom=data.xlabel, left=data.ylabel)

    # how to add labels with html styling:
    #plt.titleLabel.item.setHtml('<span style="font-size:5pt; text-align:center;color:#FFFFFF">data.component <br>hest</span>')
    #axis_style = {'color': '#FFF', 'font-size': '5pt'}
    #plt.setLabel(axis='left', text=data.ylabel, **axis_style)
    #plt.setLabel(axis='bottom', text=data.xlabel, **axis_style)

    # error bars
    beam = 0
    if len(x) > 1:
        beam = (x[1]-x[0])*0.5

    # TODO: Find solution for adding errorbars in the log case
    if not log:
        err = pg.ErrorBarItem(x=x, y=y, height=e, beam=beam)
        plt.addItem(err)

    # plots data
    plt.plot(x, y)

    plt.setMenuEnabled(False)
    vb = plt.getViewBox()

    return vb

def plot(node, i, plt, opts):
    '''
    node : plot node containing data
    i    : index of said data in node
    opts : dict containing options such as --> log, legend, icolormap, verbose, legend_fontsize
    '''
    if type(node) == plotgraph.PNSingle and i != 0:
        raise Exception("inconsistent plot request, idx=%s" % str(i))

    data = node.getdata_idx(i)

    if type(data) is IDataShadow:
        view_box = plot_iData_1D(data, plt, log=opts['log'], legend=opts['legend'], icolormap=opts['icolormap'], verbose=opts['verbose'], legend_fontsize=opts['legend_fontsize'])
        return view_box, plt
    #elif type(data) is Data2D:
    #    view_box, lyt = plot_Data2D(data, plt, log=opts['log'], legend=opts['legend'], icolormap=opts['icolormap'], verbose=opts['verbose'], legend_fontsize=opts['legend_fontsize'])
    #    return view_box, lyt
    else:
        # have a look at 1D and 2D data types
        raise Exception("unknown plot data type")


'''
iFit interface classes
'''
class IDataShadow:
    def __init__(self, signal, error, monitor, axes_vals):
        self.signal = signal
        self.error = error
        self.monitor = monitor
        self.axes_vals = axes_vals

class IFuncShadow:
    def __init__(self, definition):
        self.definition = definition

class IFitInterface:
    '''
    The lowest level above matlab, with special functions for getting
    data out of iFit objects of type iData and iFunc.
    '''
    def __init__(self):
        self.eng = matlab.engine.start_matlab('-nodesktop', async=False)

    def get_idata(self, varname):
        signal = np.array(self.eng.eval('%s.Signal' % varname, nargout=1))
        signal = np.reshape(signal, (1, len(signal)))[0]

        error = np.array(self.eng.eval('%s.Error' % varname, nargout=1))
        error = np.reshape(error, (1, len(error)))[0]

        #monitor = np.array(self.eng.eval('%s.Monitor' % varname, nargout=1))
        #monitor = np.reshape(monitor, (1, len(monitor)))[0]
        monitor = None

        axes_names = self.eng.eval('%s.Axes' % varname, nargout=1)
        firstaxes_vals = np.array(self.eng.eval('a.%s' % axes_names[0]))
        firstaxes_vals = np.reshape(firstaxes_vals, (1, len(error)))[0]

        return IDataShadow(signal, error, monitor, firstaxes_vals)

    def get_ifunc(self, varname):
        definition = self.eng.eval('%s.Signal' % varname, nargout=1)
        return IFuncShadow(definition)

    def get(self, varname):
        something = self.eng.eval('%s' % varname, nargout=1)
        return something

    def assign(self, varname, expression):
        self.eng.eval("%s = %s" % (varname, expression), nargout=0)

    def eval(self, expression):
        self.eng.eval("" % expression, nargout=1)


'''
Local script classes
'''
class IFitLoaderSimple:
    '''
    loads 1D data from ifit and transforms this into a plotgraph.
    '''
    def __init__(self, datafile):
        '''  '''
        self.datafile = datafile[0]
        self.plot_graph = None
        self.interface = IFitInterface()
        if not os.path.exists(self.datafile):
            raise Exception("requested file to load does not exist")

    def load(self):

        i = self.interface

        i.assign('a', 'load(iData, \'%s\')' % self.datafile)

        #i.assign('a', 'load(iData, [ ifitpath \'Data/sv1850.scn\' ])')
        #i.assign('a', 'load(iData, \'\/home\/jaga\/source\/McCode\/tools\/Python\/mcplot\/fitlab\/100706.dat\')')

        #ap = eng.eval('a.Signal', nargout=1)
        iData = i.get_idata('a')

        graph = plotgraph.PNSingle(plotgraph.DataHandle(load_fct=None, data=iData))
        return graph


def main(args):
    ''' load data from iFit interface and send it to the pyqtgraph frontend '''
    logging.basicConfig(level=logging.INFO)

    # ensure keyboardinterrupt ctr-c
    import signal
    signal.signal(signal.SIGINT, signal.SIG_DFL)

    try:
        # load data
        loader = IFitLoaderSimple(datafile=args.datafile)
        graph = loader.load()

        # run pqtg frontend
        plotter = pqtgfrontend.McPyqtgraphPlotter(graph, sourcedir='/nosourcedir/', plot_func=plot, invcanvas=args.invcanvas)
        plotter.runplot()

    except KeyboardInterrupt:
        print('keyboard interrupt')
    except Exception as e:
        print('mcplot error: %s' % e.__str__())
        raise e


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('datafile', nargs='*', help='file to plot')
    parser.add_argument('--invcanvas', action='store_true', help='invert canvas background from black to white')
    args = parser.parse_args()

    main(args)
