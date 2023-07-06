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
from pyqtgraph.Qt import QtCore, QtGui, QtWidgets

from . import utils
from . import mccode_config
from .mcplotloader import McCodeDataLoader, test_decfuncs, PlotGraphPrint
from .plotgraph import PNSingle, PNMultiple
from .mcplotloader import Data2D


def get_help_string():
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
    return '\n'.join(helplines)


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
        self.app = QtWidgets.QApplication(sys.argv)

        if invcanvas:
            # switch to using white background and black foreground
            pg.setConfigOption('background', 'w')
            pg.setConfigOption('foreground', 'k')


    def runplot(self):
        node = self.graph

        self.create_plotwindow(title=self.sourcedir)

        # create the logflipper
        self.viewmodel = ViewModel(sourcedir=self.sourcedir)

        # initiate event driven plot recursion
        self.plot_node(node)
        self.update_statusbar(None, node, None)

        # start
        sys.exit(self.app.exec_())


    def create_plotwindow(self, title):
        ''' set up and return a plotlayout "window" '''

        helpmessage = QtWidgets.QLabel()
        helpmessage.setText("Press 'h' for app shortcuts.")

        self.statusmessage = QtWidgets.QLabel()

        statusbar = QtWidgets.QStatusBar()
        statusbar.addWidget(helpmessage)
        statusbar.addPermanentWidget(self.statusmessage)

        self.main_window = QtWidgets.QMainWindow()
        self.graphics_view = pg.GraphicsView(self.main_window)
        self.main_window.setCentralWidget(self.graphics_view)
        self.main_window.setStatusBar(statusbar)
        self.main_window.setWindowTitle(title)
        self.main_window.setMouseTracking(True)

        # window size
        rect = QtWidgets.QApplication.desktop().screenGeometry()
        w = int(0.7 * rect.width())
        h = int(0.7 * rect.height())
        self.main_window.resize(w, h)

        self.plot_layout = pg.GraphicsLayout(border=None)
        self.graphics_view.setCentralItem(self.plot_layout)
        self.plot_layout.setContentsMargins(2, 2, 2, 2) # outermost margin

        self.main_window.show()
        self.main_window.raise_()
        self.main_window.activateWindow()


    def plot_node(self, node, sync_zoom_obj_ids=[]):
        '''
        Event driven recursive plot function. Click events are registered with each recursion.
        '''
        # First check if we have been called with a meaningful node
        if node == None:
            return

        def get_modifiers(modname):
            ''' Get int codes for keyboardmodifiers. WARNING: String codes may be used directly in code. '''
            if modname == "none":
                return 0
            if modname == "ctrl":
                return QtCore.Qt.ControlModifier
            if modname == "shft":
                return QtCore.Qt.ShiftModifier
            if modname == "ctrl-shft":
                return QtCore.Qt.ControlModifier | QtCore.Qt.ShiftModifier
            if modname == "alt":
                return QtCore.Qt.AltModifier

        # init
        self.clear_window_and_handlers()

        # get references from node
        parent = node.parent
        prim_lst = node.primaries
        sec_lst = node.secondaries

        # add plot instances and record viewbox results
        n = node.getnumdata()
        self.viewbox_list = []
        self.plot_list = []
        for i in range(n):
            viewbox, plt = self.add_plot(node, i, n)
            self.viewbox_list.append(viewbox)
            self.plot_list.append(plt)
        if id(node) in sync_zoom_obj_ids:
            self.sync_views_zooming()

        # set up viewbox - node correspondences for each action (click, right-click, ctrl-click, ...)
        node_list_click = []
        node_list_rclick = []
        node_list_ctrlclick = []

        # tag sync-zoom node id's (secondaries)
        sync_zoom_obj_ids = sync_zoom_obj_ids + [id(n) for n in sec_lst]

        # for each primary node, a click is registered to it
        node_list_click = prim_lst

        # if parent exists, all right-clicks are registered to it
        if parent:
            for i in range(n):
                node_list_rclick.append(parent)

        # for each secondary node, a ctrl-click is registered to it
        node_list_ctrlclick = sec_lst

        # set mouse click handlers on the window
        plot_node_cb = lambda node: self.plot_node(
            node, sync_zoom_obj_ids=sync_zoom_obj_ids)
        self.set_handler(node_list_click, plot_node_cb, "click", get_modifiers("none"))
        self.set_handler(node_list_rclick, plot_node_cb, "rclick", get_modifiers("none"))

        # set modifiers "alt" and "ctrl" for all platforms, since some may not work on win, darwin etc.
        self.set_handler(node_list_ctrlclick, plot_node_cb, "click", get_modifiers("alt"))
        self.set_handler(node_list_ctrlclick, plot_node_cb, "click", get_modifiers("ctrl"))

        # set keypress handlers
        replot_cb = lambda: self.plot_node(node)
        back_cb = lambda: self.plot_node(parent)
        self.set_keyhandler(replot_cb, back_cb, 'l', get_modifiers("none"))


    def show_help(self):
        if mccode_config.configuration["MCCODE"] == "mcstas":
            prefix = "mc"
        else:
            prefix = "mx"
        QtWidgets.QMessageBox.about(self.main_window, prefix+'plot-pyqtgraph', get_help_string())


    def set_keyhandler(self, replot_cb, back_cb, key, modifier):
        ''' sets a clickhadler according to input '''

        def key_handler(ev, replot_cb, back_cb, savefile_cb, flip_log, flip_legend, inc_cmap, expand_sp, debug=False):
            ''' global keypress handler, replot_cb is a function of log '''
            if ev.key() == QtCore.Qt.Key_Q:                 # q
                QtWidgets.QApplication.quit()
            elif ev.key() == QtCore.Qt.Key_L:               # l
                flip_log()
                replot_cb()
            elif ev.key() == QtCore.Qt.Key_P:               # p
                savefile_cb(format='png')
            elif ev.key() == 83:                            # s
                if not os.name == 'nt':
                    savefile_cb(format='svg')
            elif ev.key() == QtCore.Qt.Key_T:               # t
                print("Toggle legend visibility")
                flip_legend()
                replot_cb()
            elif ev.key() == QtCore.Qt.Key_C:               # c
                inc_cmap()
                replot_cb()
            elif ev.key() == QtCore.Qt.Key_F5:              # F5
                replot_cb()
            elif ev.key() == QtCore.Qt.Key_X:               # x
                expand_sp()
            elif ev.key() == QtCore.Qt.Key_F1 or ev.key() == QtCore.Qt.Key_H:   # F1 or h
                self.show_help()
            elif ev.key() == QtCore.Qt.Key_B:               # b
                back_cb()
            # print debug info
            if debug:
                print("key code: %s" % str(ev.key()))

        savefile_cb = lambda format: utils.dumpfile_pqtg(scene=self.plot_layout.scene(), format=format)
        expand_sp = lambda : self.expand_subplots(sourcedir=self.viewmodel.get_sourcedir())

        self.plot_layout.scene().keyPressEvent = \
            lambda ev: key_handler(ev = ev,
                                   replot_cb = replot_cb,
                                   savefile_cb = savefile_cb,
                                   back_cb = back_cb,
                                   flip_log = self.viewmodel.flip_log,
                                   flip_legend = self.viewmodel.flip_legend,
                                   inc_cmap = self.viewmodel.inc_colormap,
                                   expand_sp = expand_sp)


    def expand_subplots(self, sourcedir):
        ''' opens a new process of mcplot-pyqtgraph on each subdir '''
        # stolen from stack overflow:
        def get_immediate_subdirectories(a_dir):
            return [name for name in os.listdir(a_dir)
                    if os.path.isdir(os.path.join(a_dir, name))]
        def sortalpha(data):
            return sorted(data, key =
                lambda item: (
                    int(item.partition(' ')[0])
                    if item[0].isdigit() else float('inf'), item))

        subdirs = sortalpha(get_immediate_subdirectories(sourcedir))

        if len(subdirs) == 0:
            print("no subdirs to plot")
            return

        for s in subdirs:
            if mccode_config.configuration["MCCODE"] == "mcstas":
                prefix = "mc"
            else:
                prefix = "mx"
            subprocess.Popen(prefix+'plot-pyqtgraph %s' %
                os.path.join(sourcedir, s), shell=True, cwd=os.getcwd())


    def clear_window_and_handlers(self):
        ''' clears all click handlers on "rootui" '''
        self.plot_layout.clear()
        try:
            self.plot_layout.scene().sigMouseClicked.disconnect()
            self.plot_layout.scene().sigMouseMoved.disconnect()
        except TypeError:
            pass


    def set_handler(self, node_list, node_cb, click, modifier):
        ''' sets a clickhandler according to input '''

        def click_handler(event, node_list, node_cb, click, mod, debug=False):
            ''' generic conditional-branch-tree, catch-all, mouse click event handler  '''
            posItem = None
            posScene = None
            plotIdx = -1
            try:
                posItem = event.pos()
                posScene = event.scenePos()
                plotIdx = self.get_plot_index(posScene)
            except AttributeError:
                pass

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

            if plotIdx >= 0 and plotIdx < len(node_list):
                node = node_list[plotIdx]

                # replot
                node_cb(node)

                # update status message
                self.update_statusbar(None, node, posItem)

        if len(list(node_list)) == 0:
            return

        self.plot_layout.scene().sigMouseClicked.connect(
            lambda event: click_handler(
                event, node_list=node_list, node_cb=node_cb, click=click, mod=modifier))


    def get_plot_func_opts(self, log, legend, icolormap, verbose, fontsize, cbmin=None, cbmax=None):
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


    def get_sweep_multiplot_colorbar_limits(self, node):
        if type(node) == PNMultiple:
            cbmin = float("inf")
            cbmax = float("-inf")
            monname = None
            for data in node.getdata_lst():
                if type(data) != Data2D:
                    continue

                # make sure all nodes in the multiplot have the same component name
                # (indicating comparable (sweep) data)
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


    def add_plot(self, node, i, n):
        ''' constructs a plot from data and adds this to layout '''

        def get_golden_rowlen(n):
            ''' find rowlength by golden ratio '''
            return int(math.sqrt(n*1.61803398875))

        plt = pg.PlotItem()
        rowlen = get_golden_rowlen(n)

        verbose = n<=4
        fontsize = (4, 10, 14)[int(n<=2) + int(n<12)]

        if self.viewmodel.logstate():
            cbmin = cbmax = None
        else:
            cbmin, cbmax = self.get_sweep_multiplot_colorbar_limits(node)

        options = self.get_plot_func_opts(
            self.viewmodel.logstate(), self.viewmodel.legendstate(), self.viewmodel.cmapindex(),
            verbose, fontsize, cbmin, cbmax)
        view_box, plt_itm = self.plot_func(node, i, plt, options)
        if view_box:
            self.plot_layout.addItem(plt_itm, i // rowlen, i % rowlen)

        # disconnect any old mouse handlers
        try:
            self.plot_layout.scene().sigMouseMoved.disconnect()
        except TypeError:
            pass

        mouse_func = lambda pos: self.update_statusbar(plt, node, pos)
        self.plot_layout.scene().sigMouseMoved.connect(mouse_func)

        return view_box, plt


    def update_statusbar(self, plot, node, pos):
        # if a single plot is visible, show the coordinates
        if isinstance(node, PNSingle):
            if plot == None and len(self.plot_list) == 1:
                plot = self.plot_list[0]
            self.single_plot_mouse_moved(plot, node, pos)
        # else show infos about the plots
        elif isinstance(node, PNMultiple):
            self.multi_plot_mouse_moved(node, pos)


    def single_plot_mouse_moved(self, plot, node, pos):
        ''' shows the mouse coordinates for single plots '''
        if plot!=None and pos!=None:
            pos = plot.getViewBox().mapSceneToView(pos)

        # get axis labels
        xlabel = ""
        ylabel = ""
        if node != None:
            xlabel = node.getdata_idx(0).xlabel
            ylabel = node.getdata_idx(0).ylabel
        if xlabel == "":
            xlabel = "x"
        if ylabel == "":
            ylabel = "y"

        if pos != None:
            x = pos.x()
            y = pos.y()
            logstr = ""
            if self.viewmodel.log:
                # TODO: do this for non-colourplots:
                #y = np.power(10., y)
                logstr = " (log)"
            posstr = "%.24s=%g, %.24s=%g%s." % (xlabel, x, ylabel, y, logstr)
        else:
            posstr = ""
        self.statusmessage.setText(posstr)


    def multi_plot_mouse_moved(self, node, pos):
        ''' shows information on multi-plots '''
        prim = node.primaries
        num_plots = node.getnumdata()
        statustext = ""

        # find the plot under the cursor
        idx = self.get_plot_index(pos)
        if idx >= 0:
            # plot found?
            statustext = "Plot %d/%d" % (idx+1, num_plots)
            if prim != None:
                title = prim[idx].getdata_idx(0).title
                # truncate the tile string if it's too long
                if len(title) > 64:
                    title = title[0:64] + "..."
                statustext += ": " + title
            statustext += "."
        else:
            if num_plots == 1:
                statustext = "Showing 1 plot."
            else:
                statustext = "Showing %d plots." % num_plots

        if self.viewmodel.log:
            statustext = "Mode: log. " + statustext
        else:
            statustext = "Mode: lin. " + statustext

        self.statusmessage.setText(statustext)


    def get_plot_index(self, pos):
        ''' get the plot index at (cursor) position pos '''
        if self.viewbox_list == None or pos == None:
            return -1

        for idx in range(len(self.viewbox_list)):
            viewbox = self.viewbox_list[idx]
            topRight = viewbox.mapViewToScene(viewbox.viewRect().topRight())
            bottomLeft = viewbox.mapViewToScene(viewbox.viewRect().bottomLeft())

            rect = QtCore.QRectF()
            rect.setTopRight(topRight)
            rect.setBottomLeft(bottomLeft)

            # plot found?
            if rect.contains(pos):
                return idx

        # plot not found
        return -1


    def sync_views_zooming(self):
        ''' replace individual viewbox wheel events with a new, global wheel event which calls all of them '''
        org_wheel_events = [vb.wheelEvent for vb in self.viewbox_list]
        def modded_wheel_event(ev, axis = None):
            for vb in self.viewbox_list:
                org_wheel_events[self.viewbox_list.index(vb)](ev, axis=axis)
        for vb in self.viewbox_list:
            vb.wheelEvent = modded_wheel_event
