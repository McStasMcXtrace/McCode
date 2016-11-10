#!/usr/bin/env python
'''
pyqtgraph mcplot frontend, which uses the mcplot graph-based data loader.
'''
import argparse
import logging
import os
import sys
import math

import PyQt4
import pyqtgraph as pg
from pyqtgraph.Qt import QtGui, QtCore

from plotfuncs import plot_Data1D, plot_Data2D

sys.path.append(os.path.join(os.path.dirname(__file__), '..', '..'))

from mccodelib.mcplotloader import McPlotDataLoader, test_decfuncs
from mccodelib.mcplotgraph import PlotGraphPrint, PNMultiple, Data1D, Data2D

class McPyqtgraphPlotter():
    '''
    PyQtGraph-based plot wrapper class.
    '''
    def __init__(self, plotgraph):
        self.graph = plotgraph
        self.win = None
        
    def runplot(self):
        node = self.graph
        
        # set up the qt app
        app = QtGui.QApplication([])
        window = pg.GraphicsWindow()
        window.resize(1000,600)
        
        # initiate event driven plot recursion
        plot_node(node, window)
        
        # start
        QtGui.QApplication.instance().exec_()

def plot_node(node, window):
    '''
    Event driven recursive plot function. Click events are registered with each recursion.
    '''
    # init
    clear_window_and_handlers(window)
    
    # get references from node
    data_lst = node.data
    parent = node.parent
    prim_lst = node.primaries
    sec_lst = node.secondaries
    
    # add plot(s) NOTE: these four lines might be simplified to one by use of an iterator in place of the fct. call
    n = len(data_lst)
    plt_lst = []
    for i in range(n):
        plt_lst.append(add_plot(window, data_lst[i], i, n))
    # get viewbox's
    viewbox_lst = [get_viewbox(plt) for plt in plt_lst]
    
    # set up viewbox - node correspondences for each action (click, right-click, ctrl-click, ...)
    vn_dict_click = {}
    vn_dict_rclick = {}
    vn_dict_ctrlclick = {}
    # for each primary node, a click is registered to it
    for i in range(len(prim_lst)):
        vn_dict_click[viewbox_lst[i]] = prim_lst[i]
    # if parent exists, all right-clicks are registered to it
    if parent:
        for i in range(len(data_lst)):
            vn_dict_rclick[viewbox_lst[i]] = parent
    # for each secondary node, a ctrl-click is registered to it
    for i in range(len(sec_lst)):
        vn_dict_ctrlclick[viewbox_lst[i]] = sec_lst[i]
    
    # set handlers on the window
    plot_node_cb = lambda node: plot_node(node, window=window)
    set_handler(window.scene(), vn_dict_click, plot_node_cb, "click", get_modifiers("none"))
    set_handler(window.scene(), vn_dict_rclick, plot_node_cb, "rclick", get_modifiers("none"))
    set_handler(window.scene(), vn_dict_ctrlclick, plot_node_cb, "click", get_modifiers("ctrl"))

def get_modifiers(modname):
    ''' get int codes for keyboardmodifiers '''
    if modname == "none":
        return 0
    if modname == "ctrl":
        return 67108864
    if modname == "shft":
        return 33554432
    if modname == "ctrl-shft":
        return 100663296

def clear_window_and_handlers(window):
    ''' clears all click handlers '''
    window.clear()
    try:
        window.scene.sigMouseClicked.disconnect()
    except:
        # TODO: log.DEBUG("no events to disconnect")
        pass

def set_handler(scene, vn_dict, node_cb, click, modifier):
    ''' sets a clickhadler according to input '''
    if len(list(vn_dict)) == 0:
        return
    scene.sigMouseClicked.connect(lambda event: click_handler(event, vn_dict=vn_dict, node_cb=node_cb, click=click, mod=modifier))

def click_handler(event, vn_dict, node_cb, click, mod):
    ''' generic conditional-branch-tree, catch-all, mouse click event handler  '''
    # prevent action for modifiers mismatch
    if int(event.modifiers()) != mod:
        return
    # prevent action for mouse button mismatch
    if click == "rclick" and event.button() != 2:
        return
    if click == "click" and event.button() != 1:
        return
    
    node = vn_dict.get(event.currentItem, None)
    if node:
        node_cb(node)

def get_viewbox(plt):
    ''' returns the viewbox of a plot object '''
    return plt.getViewBox()

def get_golden_rowlen(n):
    ''' find rowlength by golden ratio '''
    return int(math.sqrt(n*1.61803398875))

def add_plot(window, data, i, n):
    ''' constructs a plot from data and adds this to window '''
    plt = window.addPlot()
    plt.setMenuEnabled(False)
    if type(data) is Data1D:
        plot_Data1D(data, plt)
    else:
        plot_Data2D(data, plt)
    
    if (i+1) % get_golden_rowlen(n) == 0:
        window.nextRow()
    
    return plt

def main(args):
    ''' load data from mcplot backend and send it to the pyqtgraph frontend above '''
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
        
        plotter = McPyqtgraphPlotter(graph)
        plotter.runplot()
        
    except Exception as e:
        print('mcplot error: %s' % e.__str__())
        raise e


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('simulation', nargs='*', help='file or directory to plot')
    parser.add_argument('-t', '--test',  action='store_true', default=False, help='mccode data loader test run')
    args = parser.parse_args()

    main(args)
