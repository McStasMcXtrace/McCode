'''
Plugable pyqtgraph mcplot frontend.
'''
import os
import sys
import math
import subprocess
import numpy as np

import PyQt5
import pyqtgraph as pg
from pyqtgraph.Qt import QtGui, QtCore

from . import utils
from . import mccode_config
from .mcplotloader import McCodeDataLoader, test_decfuncs, PlotGraphPrint
from .plotgraph import PNMultiple
from .mcplotloader import Data2D


class McPyqtgraphPlotter():
    '''
    PyQtGraph-based plotter class.
    '''
    def __init__(self, plotgraph, sourcedir, plot_func, invcanvas):
        '''
        plotgraph: plotgraph root node
        sourcedir: data files source directory
        plot_funct: the user-specified matching user data.
            Signature:
                node     - plotgraph node
                i        - int
                plt      - pqtg plot object
                opts     - (dict, see code)))
                
            returns: 
                view_box - the click-able view box of the constructed plot object
                plt      - the plot object to be added to the pqtg window 
        inv_canvas: inverts background from black to white
        '''
        self.graph = plotgraph
        self.sourcedir = sourcedir
        self.plot_func = plot_func
        
        # Qt app
        self.app = QtGui.QApplication(sys.argv)
        
        if invcanvas:
            # switch to using white background and black foreground
            pg.setConfigOption('background', 'w')
            pg.setConfigOption('foreground', 'k')
        
    def runplot(self):
        node = self.graph
        
        plt_layout = create_plotwindow(title=self.sourcedir+" - Press 'h' for app shortcuts")
        
        # create the logflipper
        viewmodel = ViewModel(sourcedir=self.sourcedir)
    
        # initiate event driven plot recursion
        plot_node(node, self.plot_func, plt_layout, viewmodel)
        
        # start
        sys.exit(self.app.exec_())


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
    
    layout = pg.GraphicsLayout(border=None)
    window.setCentralItem(layout)
    layout.window = window # keep window to avoid garbage collection
    layout.setContentsMargins(2, 2, 2, 2) # outermost margin
    layout.mw = mw
    
    mw.show()
    mw.raise_()
    
    return layout

class ViewModel():
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

def plot_node(node, plot_func, layout, viewmodel, sync_zoom_obj_ids=[]):
    '''
    Event driven recursive plot function. Click events are registered with each recursion.
    '''
    # First check if we have been called with a meaningful node
    if node:
        # init
        clear_window_and_handlers(layout)
    
        # get references from node
        parent = node.parent
        prim_lst = node.primaries
        sec_lst = node.secondaries
    
        # add plot instances and record viewbox results
        #n = len(data_lst)
        n = node.getnumdata()
        viewbox_lst = []
        for i in range(n):
            viewbox_lst.append(add_plot(layout, node, plot_func, i, n, viewmodel))
        if id(node) in sync_zoom_obj_ids:
            sync_views_zooming(viewbox_lst)
    
        # set up viewbox - node correspondences for each action (click, right-click, ctrl-click, ...)
        vn_dict_click = {}
        vn_dict_rclick = {}
        vn_dict_ctrlclick = {}
        # tag sync-zoom node id's (secondaries)
        sync_zoom_obj_ids = sync_zoom_obj_ids + [id(n) for n in sec_lst]
        # for each primary node, a click is registered to it
        for i in range(len(prim_lst)):
            vn_dict_click[viewbox_lst[i]] = prim_lst[i]
        # if parent exists, all right-clicks are registered to it
        if parent:
            for i in range(n):
                vn_dict_rclick[viewbox_lst[i]] = parent
        # for each secondary node, a ctrl-click is registered to it
        for i in range(len(prim_lst)):
            vn_dict_ctrlclick[viewbox_lst[i]] = sec_lst[i]
        
        # set mouse click handlers on the window
        plot_node_cb = lambda node: plot_node(node, plot_func, layout=layout, viewmodel=viewmodel, sync_zoom_obj_ids=sync_zoom_obj_ids)
        set_handler(layout.scene(), vn_dict_click, plot_node_cb, "click", get_modifiers("none"))
        set_handler(layout.scene(), vn_dict_rclick, plot_node_cb, "rclick", get_modifiers("none"))

        # set modifiers "alt" and "ctrl" for all platforms, since some may not work on win, darwin etc.
        set_handler(layout.scene(), vn_dict_ctrlclick, plot_node_cb, "click", get_modifiers("alt"))
        set_handler(layout.scene(), vn_dict_ctrlclick, plot_node_cb, "click", get_modifiers("ctrl"))        

        # set keypress handlers 
        replot_cb = lambda: plot_node(node, plot_func, layout, viewmodel=viewmodel)
        back_cb = lambda: plot_node(node.parent, plot_func, layout, viewmodel=viewmodel)
        set_keyhandler(layout.scene(), replot_cb, back_cb, 'l', get_modifiers("none"), viewmodel=viewmodel)

def get_modifiers(modname):
    ''' Get int codes for keyboardmodifiers. WARNING: String codes may be used directly in code. '''
    if modname == "none":
        return 0
    if modname == "ctrl":
        return 67108864
    if modname == "shft":
        return 33554432
    if modname == "ctrl-shft":
        return 100663296
    if modname == "alt":
        return 134217728

def print_help(nogui=False):
    if sys.platform == 'darwin':
        modifier = 'Meta'
    elif sys.platform == 'win32':
        modifier = 'alt'
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

def get_plot_func_opts(log, legend, icolormap, verbose, fontsize, cbmin=None, cbmax=None):
    ''' returns a dict for holding the plot options relevant for this plotting frontend '''
    d = {}
    d['log'] = log
    d['legend'] = legend 
    d['icolormap'] = icolormap 
    d['verbose'] = verbose
    d['fontsize'] = fontsize
    if cbmin != None and cbmax != None:
        d['cbmin'] = cbmin
        d['cbmax'] = cbmax
    return d

def get_sweep_multiplot_colorbar_limits(node):
    if type(node) == PNMultiple:
        cbmin = float("inf")
        cbmax = float("-inf")
        monname = None
        for data in node.getdata_lst():
            if type(data) != Data2D:
                continue

            # make sure all nodes in the multiplot have the same component name (indicating comparable (sweep) data)
            if monname == None:
                monname = data.component
            else:
                if data.component != monname:
                    return None, None

            # update min/max values
            if type(data) == Data2D: 
                localmin = np.min(np.array(data.zvals))
                localmax = np.max(np.array(data.zvals))
                cbmin = min(cbmin, localmin)
                cbmax = max(cbmax, localmax)
        return cbmin, cbmax
    return None, None

def add_plot(layout, node, plot_node_func, i, n, viewmodel):
    ''' constructs a plot from data and adds this to layout '''
    plt = pg.PlotItem()
    rowlen = get_golden_rowlen(n)
    
    verbose = n<=4
    fontsize = (4, 10, 14)[int(n<=2) + int(n<12)]
    
    cbmin, cbmax = get_sweep_multiplot_colorbar_limits(node)
    options = get_plot_func_opts(viewmodel.logstate(), viewmodel.legendstate(), viewmodel.cmapindex(), verbose, fontsize, cbmin, cbmax)
    view_box, plt_itm = plot_node_func(node, i, plt, options)
    if (view_box):
        layout.addItem(plt_itm, i / rowlen, i % rowlen)
    
    return view_box

def sync_views_zooming(vb_lst):
    ''' replace individual viewbox vheel events with a new, global wheel event which calls all of them '''
    org_wheel_events = [vb.wheelEvent for vb in vb_lst]
    def modded_wheel_event(ev):
        for vb in vb_lst:
            org_wheel_events[vb_lst.index(vb)](ev)
    for vb in vb_lst:
        vb.wheelEvent = modded_wheel_event

