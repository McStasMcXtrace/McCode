#!/usr/bin/env python
'''
pyqtgraph mcplot frontend, which uses the mcplot graph-based data loader.
'''
import argparse
import logging
import os
import sys
import math
import subprocess

import PyQt4
import pyqtgraph as pg
from pyqtgraph.Qt import QtGui, QtCore

from plotfuncs import plot_Data1D, plot_Data2D

sys.path.append(os.path.join(os.path.dirname(__file__), '..', '..'))

from mccodelib import utils
from mccodelib import mccode_config
from mccodelib.mcplotloader import McPlotDataLoader, test_decfuncs, Data1D, Data2D, PlotGraphPrint

class McPyqtgraphPlotter():
    '''
    PyQtGraph-based plotter class.
    '''
    def __init__(self, plotgraph, sourcedir):
        self.graph = plotgraph
        self.sourcedir = sourcedir
    
    def runplot(self):
        node = self.graph
        
        plt_layout = create_plotwindow(title=self.sourcedir)
        
        # create the logflipper
        flipper = LogFlipper(sourcedir=self.sourcedir)
        
        # initiate event driven plot recursion
        plot_node(node, plt_layout, flipper)
    
def create_plotwindow(title):
    ''' set up and return a plotlayout "window" '''
    
    window = pg.GraphicsWindow()
    
    mw = QtGui.QMainWindow()
    window.setParent(mw)
    mw.setCentralWidget(window)
    mw.setWindowTitle(title)
    
    # window size
    rect = QtGui.QApplication.desktop().screenGeometry()
    w = 0.7 * rect.width()
    h = 0.7 * rect.height()
    mw.resize(w, h)
    
    global g_window
    g_window = mw
    
    layout = pg.GraphicsLayout(border=True)
    window.setCentralItem(layout)
    layout.window = window # keep window to avoid garbage collection
    layout.setContentsMargins(2, 2, 2, 2) # outermost margin
    layout.mw = mw
    
    mw.show()
    mw.raise_()
    
    return layout

class LogFlipper():
    ''' 
    It is a kind of viewmodel, originally a log logstate housekeeping object, 
    extended by various other logstate variables as well.
    '''
    def __init__(self, log=False, legend=True, sourcedir=None):
        self.log = log
        self.icolormap = 0
        self.legend = legend
        self.sourcedir = sourcedir
    def flip_log(self):
        self.log = not self.log
        return self.log
    def inc_colormap(self):
        self.icolormap += 1
    def flip_legend(self):
        self.legend = not self.legend
        return self.legend
    def logstate(self):
        return self.log
    def legendstate(self):
        return self.legend
    def cmapindex(self):
        return self.icolormap
    def get_sourcedir(self):
        return self.sourcedir

def plot_node(node, layout, viewmodel):
    '''
    Event driven recursive plot function. Click events are registered with each recursion.
    '''
    # First check if we have been called with a meaningful node
    if node:
        # init
        clear_window_and_handlers(layout)
    
        # get references from node
        data_lst = node.getdata_lst()
        parent = node.parent
        prim_lst = node.primaries
        sec_lst = node.secondaries
    
        # add plot instances and record viewbox results
        n = len(data_lst)
        viewbox_lst = []
        for i in range(n):
            viewbox_lst.append(add_plot(layout, data_lst[i], i, n, viewmodel.logstate(), viewmodel.legendstate(), viewmodel.cmapindex()))
    
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
        plot_node_cb = lambda node: plot_node(node, layout=layout, viewmodel=viewmodel)
        set_handler(layout.scene(), vn_dict_click, plot_node_cb, "click", get_modifiers("none"))
        set_handler(layout.scene(), vn_dict_rclick, plot_node_cb, "rclick", get_modifiers("none"))
        set_handler(layout.scene(), vn_dict_ctrlclick, plot_node_cb, "click", get_modifiers("ctrl"))
    
        # set keypress handlers 
        replot_cb = lambda: plot_node(node, layout, viewmodel=viewmodel)
        back_cb = lambda: plot_node(node.parent, layout, viewmodel=viewmodel)
        set_keyhandler(layout.scene(), replot_cb, back_cb, 'l', get_modifiers("none"), viewmodel=viewmodel)

def print_help(nogui=False):
    if sys.platform == 'darwin':
        modifier = 'Meta'
    else:
        modifier = 'ctrl'
    
    helplines = []
    helplines.append('')
    helplines.append('q              - quit')
    helplines.append('p              - save png')
    if not os.name == 'nt':
        helplines.append('s              - save svg')
    helplines.append('l              - log toggle')
    helplines.append('t              - textinfo toggle')
    helplines.append('c              - cycle colormap')
    helplines.append('F1/h           - help')
    helplines.append('F5             - replot')
    helplines.append('click          - display subplot')
    helplines.append('right-click/b  - back')
    helplines.append('%s + click   - sweep monitors' % modifier)
    helplines.append('x              - expand subplots')
    print('\n'.join(helplines))
    
    if not nogui:
        if mccode_config.configuration["MCCODE"] == "mcstas":
            prefix = "mc"
        else:
            prefix = "mx"
        QtGui.QMessageBox.about(g_window, prefix+'plot-pyqtgraph', '\n'.join(helplines))

def set_keyhandler(scene, replot_cb, back_cb, key, modifier, viewmodel):
    ''' sets a clickhadler according to input '''
    
    def key_handler(ev, replot_cb, back_cb, savefile_cb, flip_log, flip_legend, inc_cmap, expand_sp, debug=False):
        ''' global keypress handler, replot_cb is a function of log '''
        if ev.key() == 81:                              # q
            QtGui.QApplication.quit()
        elif ev.key() == 76:                            # l
            flip_log()
            replot_cb()
        elif ev.key() == 80:                            # p
            savefile_cb(format='png')
        elif ev.key() == 83:                            # s
            if not os.name == 'nt':
                savefile_cb(format='svg')
        elif ev.key() == 84:                            # t
            print("Toggle legend visibility")
            flip_legend()
            replot_cb()
        elif ev.key() == 67:                            # c
            inc_cmap()
            replot_cb()
        elif ev.key() == 16777268:                      # F5
            replot_cb()
        elif ev.key() == 88:                            # x
            expand_sp()
        elif ev.key() == 16777264 or ev.key() == 72:    # F1 or h
            print_help()
        elif ev.key() == 66:                            # b
            back_cb()
        # print debug info
        if debug:
            print("key code: %s" % str(ev.key()))
    
    savefile_cb = lambda format: utils.dumpfile_pqtg(scene=scene, format=format)
    expand_sp = lambda : expand_subplots(sourcedir=viewmodel.get_sourcedir())
    
    scene.keyPressEvent = lambda ev: key_handler(ev=ev,
                                                 replot_cb=replot_cb,
                                                 savefile_cb=savefile_cb,
                                                 back_cb=back_cb,
                                                 flip_log=viewmodel.flip_log,
                                                 flip_legend=viewmodel.flip_legend,
                                                 inc_cmap=viewmodel.inc_colormap,
                                                 expand_sp=expand_sp)

def expand_subplots(sourcedir):
    ''' opens a new process of mcplot-pyqtgraph on each subdir '''
    # stolen from stack overflow:
    def get_immediate_subdirectories(a_dir):
        return [name for name in os.listdir(a_dir)
                if os.path.isdir(os.path.join(a_dir, name))]
    def sortalpha(data):
        return sorted(data, key=lambda item: (
                                       int(item.partition(' ')[0])
                                       if item[0].isdigit() else float('inf'), item)
               )
    
    subdirs = sortalpha(get_immediate_subdirectories(sourcedir))
    
    if len(subdirs) == 0:
        print("no subdirs to plot")
        return
        
    for s in subdirs:
        subprocess.Popen('mcplot-pyqtgraph %s' % os.path.join(sourcedir, s), shell=True, cwd=os.getcwd())

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

def clear_window_and_handlers(rootui):
    ''' clears all click handlers on "rootui" '''
    rootui.clear()
    try:
        rootui.scene.sigMouseClicked.disconnect()
    except:
        # TODO: log.DEBUG("no events to disconnect")
        pass

def set_handler(scene, vn_dict, node_cb, click, modifier):
    ''' sets a clickhandler according to input '''
    
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

def add_plot(layout, data, i, n, log=False, legend=True, icolormap=0):
    ''' constructs a plot from data and adds this to layout '''
    view_box = None
    plt = pg.PlotItem()
    rowlen = get_golden_rowlen(n)
    
    verbose = n<=4
    legend_fontsize = (8, 14)[n<=2]
    
    if type(data) is Data1D:
        view_box = plot_Data1D(data, plt, log=log, legend=legend, icolormap=icolormap, verbose=verbose, legend_fontsize=legend_fontsize)
        layout.addItem(plt, i / rowlen, i % rowlen)
    elif type(data) is Data2D:
        view_box, lyt = plot_Data2D(data, plt, log=log, legend=legend, icolormap=icolormap, verbose=verbose, legend_fontsize=legend_fontsize)
        layout.addItem(lyt, i / rowlen, i % rowlen)
    else:
        raise Exception("unknown plot data type")
    
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
        try:
            loader.load()
        except Exception as e:
            # invallid input case:
            print('mcplot loader: ' + e.__str__())
            print_help(nogui=True)
            quit()
        
        graph = loader.plot_graph
        
        if args.test:
            printer = PlotGraphPrint(graph)
        
        # Qt app
        app = QtGui.QApplication(sys.argv)
        
        # set up
        plotter = McPyqtgraphPlotter(graph, sourcedir=loader.directory)
        plotter.runplot()
        print_help(nogui=True)
        
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

