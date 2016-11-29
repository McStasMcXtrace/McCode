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

from mccodelib.mcplotloader import McPlotDataLoader, test_decfuncs, Data1D, Data2D
from mccodelib.mcplotgraph import PlotGraphPrint

class McPyqtgraphPlotter():
    '''
    PyQtGraph-based plotter class.
    '''
    def __init__(self, plotgraph, simfile):
        self.graph = plotgraph
        self.simfile = simfile
        
    def runplot(self):
        node = self.graph
        
        plt_layout = create_plotwindow(title=self.simfile)
        
        # create the logflipper
        flipper = LogFlipper()
        
        # initiate event driven plot recursion
        plot_node(node, plt_layout, flipper)

def create_plotwindow(title):
    ''' set up and return a plotlayout "window" '''
    window = pg.GraphicsWindow()
    window.resize(1000,600)
    window.setWindowTitle(title)
    
    layout = pg.GraphicsLayout()
    window.setCentralItem(layout)
    layout.window = window # keep window to avoid garbage collection
    layout.setContentsMargins(2, 2, 2, 2) # outermost margin
    
    return layout

class LogFlipper():
    ''' boolean log state keeper object '''
    def __init__(self, log=False):
        self.log = log
    def flip(self):
        self.log = not self.log
        return self.log
    def state(self):
        return self.log

def plot_node(node, layout, logflipper):
    '''
    Event driven recursive plot function. Click events are registered with each recursion.
    '''
    # init
    clear_window_and_handlers(layout)
    
    # get references from node
    data_lst = node.data
    parent = node.parent
    prim_lst = node.primaries
    sec_lst = node.secondaries
    
    # add plot instances and record viewbox results
    n = len(data_lst)
    viewbox_lst = []
    for i in range(n):
        viewbox_lst.append(add_plot(layout, data_lst[i], i, n, logflipper.state()))
    
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
    
    # set mouse click handlers on the window
    plot_node_cb = lambda node: plot_node(node, layout=layout, logflipper=logflipper)
    set_handler(layout.scene(), vn_dict_click, plot_node_cb, "click", get_modifiers("none"))
    set_handler(layout.scene(), vn_dict_rclick, plot_node_cb, "rclick", get_modifiers("none"))
    set_handler(layout.scene(), vn_dict_ctrlclick, plot_node_cb, "click", get_modifiers("ctrl"))
    
    # set keypress handlers 
    replot_cb = lambda log: plot_node(node, layout, logflipper=logflipper)
    set_keyhandler(layout.scene(), replot_cb, 'l', get_modifiers("none"), flip_log=logflipper.flip)

def set_keyhandler(scene, replot_cb, key, modifier, flip_log):
    ''' sets a clickhadler according to input '''
    
    def key_handler(ev, cb, savefile_cb, flip_log, debug=False):
        ''' global keypress handler, cb is a function of log '''
        if ev.key() == 81: # q
            quit()
        elif ev.key() == 76: # l
            log = flip_log()
            cb(log)
        elif ev.key() == 80: # p
            savefile_cb(format='png')
            print("png file saved")
        elif ev.key() == 60:
            savefile_cb(format='svg')
            print("svg file saved")
        elif ev.key() == 68:
            savefile_cb(format='pdf')
            print("svg and pdf files saved")
        elif ev.key() == 16777268: # F5
            cb(log=False)
        elif ev.key() == 16777264 or ev.key() == 72: # F1 or h
            if sys.platform == 'darwin':
                modifier = 'Meta'
            else:
                modifier = 'ctrl'
            print("q            - quit\np            - save png\ns            - save svg\nd            - save pdf\nl            - log\nF1/h         - help\nF5           - replot\nclick        - display subplot\nrclick       - back\n" + modifier + " + click - sweep monitors")
        # print debug info
        if debug:
            print("key code: %s" % str(ev.key()))
    
    savefile_cb = lambda format: dumpfile(scene=scene, format=format)
    
    scene.keyPressEvent = lambda ev: key_handler(ev=ev, cb=replot_cb, savefile_cb=savefile_cb, flip_log=flip_log)

def dumpfile(scene, filenamebase='mcplot', format='png'):
    ''' save as png file. Pdf is not supported, althouhg svg kind-of is '''
    import pyqtgraph.exporters
    if format=='png':
        exporter = pg.exporters.ImageExporter(scene)
        exporter.export('%s.png' % filenamebase)
    elif format=='svg':
        exporter = pg.exporters.SVGExporter(scene)
        exporter.export('%s.svg' % filenamebase)
    elif format=='pdf':
        exporter = pg.exporters.SVGExporter(scene)
        exporter.export('%s.svg' % filenamebase)
        import subprocess
        # TODO: error handling
        process = subprocess.Popen('svg2pdf %s.svg %s.pdf' % (filenamebase, filenamebase), 
                                   stdout=subprocess.PIPE, 
                                   stderr=subprocess.PIPE,
                                   shell=True,
                                   universal_newlines=True)
    else:
        raise Exception('png, svg and pdf (via svg2pdf) are the only supported file formats (format=%s)' % format)

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
    
    def click_handler(event, vn_dict, node_cb, click, mod, debug=False):
        ''' generic conditional-branch-tree, catch-all, mouse click event handler  '''
        # print debug info
        if debug:
            print("click modifier: %s" % str(int(event.modifiers())))
            
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
    
    if len(list(vn_dict)) == 0:
        return
    scene.sigMouseClicked.connect(lambda event: click_handler(event, vn_dict=vn_dict, node_cb=node_cb, click=click, mod=modifier))

def get_viewbox(plt):
    ''' returns the viewbox of a plot object '''
    return plt.getViewBox()

def get_golden_rowlen(n):
    ''' find rowlength by golden ratio '''
    return int(math.sqrt(n*1.61803398875))

def add_plot(layout, data, i, n, log=False):
    ''' constructs a plot from data and adds this to layout '''
    rowlen = get_golden_rowlen(n)
    
    if type(data) is Data1D:
        item, view_box = plot_Data1D(data, log=log)
    else:
        item, view_box = plot_Data2D(data, log=log)

    layout.addItem(item, i / rowlen, i % rowlen)
    
    return view_box

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
        
        loader = McPlotDataLoader(simfile=simfile)
        loader.load()
        graph = loader.plot_graph
        
        if args.test:
            printer = PlotGraphPrint(graph)
        
        # Qt app
        app = QtGui.QApplication(sys.argv)
        
        # set up
        plotter = McPyqtgraphPlotter(graph, loader.simfile)
        plotter.runplot()
        
        # start
        sys.exit(app.exec_())
    
    except KeyboardInterrupt:
        print('keyboard interrupt')
    except Exception as e:
        print('mcplot error: %s' % e.__str__())
        raise e


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('simulation', nargs='*', help='file or directory to plot')
    parser.add_argument('-t', '--test',  action='store_true', default=False, help='mccode data loader test run')
    args = parser.parse_args()

    main(args)
