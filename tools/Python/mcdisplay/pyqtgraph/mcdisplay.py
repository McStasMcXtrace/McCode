#!/usr/bin/env python3
# -*- coding: utf-8 -*-
'''
mcdisplay pyqtgraph script.
'''
import sys
import os
import logging
import argparse
from datetime import datetime
import numpy as np
from enum import Enum
import pathlib
import PyQt5
from pyqtgraph.Qt import QtGui, QtCore
import pyqtgraph as pg
from pyqtgraph.graphicsItems.LegendItem import LegendItem, ItemSample

sys.path.append(os.path.join(os.path.dirname(__file__), '..', '..'))

from mccodelib import utils
from mccodelib.mcdisplayutils import McDisplayReader
from mccodelib.instrgeom import Vector3d, DrawLine, DrawMultiline, DrawCircle
from mccodelib.instrparser import InstrTraceParser, InstrObjectConstructor
from mccodelib.fcparticleparser import FlowChartParticleTraceParser


def get_2d_ray(ray_story, instr, plane='zy'):
    ''' returns transformed projection into plane, instr is used to transform points '''
    coords = []
    
    (k1, k2) = (0,1)
    if plane == 'zy': (k1, k2) = (2,1)
    if plane == 'xy': (k1, k2) = (0,1)
    if plane == 'zx': (k1, k2) = (2,0)
    
    for group in ray_story.groups:
        try:
            transform = [c.transform for c in instr.components if c.name == group.compname][0]
        except:
            logging.debug('missing comp in ray gropus: %s' % group.compname)
            continue
        
        for e in group.events:
            p = transform.apply(Vector3d(e.args[0], e.args[1], e.args[2]))
            coords.append((p[k1], p[k2]))
    
    return coords

# colour stuff
colour_idx = 0
def get_next_colour(colour_idx):
    colour = colours[colour_idx % len(colours)]
    return colour

def plot_tof_instr(instr, t_min, t_max, plt):
    ''' creates a horizontal line for the lower- and upper bounds of each component '''
    global colour_idx
    for c in instr.components:
        bb = c.get_tranformed_bb()
        colour = get_next_colour(colour_idx)
        colour_idx += 1
        z1 = bb.z1
        z2 = bb.z2
        zpos = c.pos.z
        if not (abs(z1) == float("inf") or abs(z2) == float("inf")):
            if z1 != zpos:
                plt.plot(np.array([t_min, t_max]), np.array([z1, z1]), pen=pg.mkPen(color=colour, style=QtCore.Qt.DashLine))
            if z2 != zpos:
                plt.plot(np.array([t_min, t_max]), np.array([z2, z2]), pen=pg.mkPen(color=colour, style=QtCore.Qt.DashLine))
            plt.plot(np.array([t_min, t_max]), np.array([zpos, zpos]), pen=pg.mkPen(color=colour))
    
    plt.setLabels(left="z [m]",bottom="time [secs]")

def plot_1d_tof_rays(instr, rays, plt):
    for story in rays:
        t = []
        z = []
        for g in story.groups:
            if not g.transform:
                g.transform = [c.transform for c in instr.components if c.name == g.compname][0]
            pvt_lst = g.get_transformed_pos_vel_t_lst()
            t = t + [pvt[2] for pvt in pvt_lst]
            z = z + [pvt[0].z for pvt in pvt_lst]
        plt.plot(t, z, symbol='o', symbolSize=7, pen=pg.mkPen(pg.getConfigOption('foreground')))
    

def plot_2d_ray(coords, plt):
    ''' see get_2d_ray to understand the data structure '''
    x = np.array([p[0] for p in coords])
    y = np.array([p[1] for p in coords])
    return plt.plot(x, y, pen=pg.mkPen(pg.getConfigOption('foreground')))

def get_2d_instrument(instr, plane='zy'):
    ''' returns a list of (compname, coords-lst) tuples, coords-lst being a list of 2-tuple points in the plane '''
    (k1, k2) = (0,1)
    if plane == 'zy': (k1, k2) = (2,1)
    if plane == 'xy': (k1, k2) = (0,1)
    if plane == 'zx': (k1, k2) = (2,0)
    
    coords_sets = []
    
    for c in instr.components:
        comp_coord_sets = []
        for d in c.drawcalls:
            if type(d) in [DrawLine, DrawMultiline]:
                comp_coord_sets.append([(tp[k1], tp[k2]) for tp in [c.transform.apply(p) for p in d.points]])
            if type(d) in [DrawCircle]:
                set = [(tp[k1], tp[k2]) for tp in [c.transform.apply(p) for p in d.get_points_on_circle()]]
                comp_coord_sets.append(set)
        coords_sets.append((c.name, comp_coord_sets))
    
    return coords_sets

colours = [(248, 0, 0), (0, 248, 0), (0, 0, 248), (0, 248, 248), (248, 0, 248), (0, 248, 128), (248, 248, 0), (248, 128, 0), (128, 248, 0), (0, 128, 248), (128, 0, 248), (248, 0, 128), (168, 168, 168)]

def plot_2d_instr(coords_sets, plt, xlabel, ylabel):
    '''
    See get_2d_instrument impl to understand the data structure 
    
    Returns a list of compname, plotitem
    '''
    idx = 0
    def get_next_colour(idx):
        colour = colours[idx % len(colours)]
        return colour
    
    compnames_plts = []
    
    for i in range(len(coords_sets)):
        comp_coords_sets = coords_sets[i]
        
        colour = get_next_colour(idx)
        idx += 1
        comp = comp_coords_sets[0]
        comp_data = comp_coords_sets[1]
        
        x_comp = np.array([])
        y_comp = np.array([])
        connect_comp = np.array([])
        for coords in comp_data:
            x = np.array([p[0] for p in coords])
            y = np.array([p[1] for p in coords])
            # all true except for the last entry, which gives a hole towards the next set
            connect = np.array([True for x in x])
            connect[len(x)-1] = False
            
            x_comp = np.concatenate([x_comp, x])
            y_comp = np.concatenate([y_comp, y])
                        
            connect_comp = np.concatenate([connect_comp, connect])
        
        itm = plt.plot(x_comp, y_comp, connect=connect_comp, pen=pg.mkPen(color=colour))
        compnames_plts.append((comp, itm))

    # Fix the axes to have a common x and y zooom factor
    AxisBounds=plt.getViewBox().childrenBounds()
    x0=(AxisBounds[0][1]+AxisBounds[0][0])/2
    y0=(AxisBounds[1][1]+AxisBounds[1][0])/2
    dx=AxisBounds[0][1]-AxisBounds[0][0]
    dy=AxisBounds[1][1]-AxisBounds[1][0]
    if dy>=dx:
        dx=dy
    else:
        dy=dx
    plt.getViewBox().setRange(xRange=[x0-dx/2,x0+dx/2],yRange=[y0-dy/2,y0+dy/2])
    plt.setLabels(left=ylabel,bottom=xlabel)
    
    return compnames_plts

class ModLegend(pg.LegendItem):
    """
    Modified LegendItem to remove the ugly / in the label. Also reduces text size and padding.
    """
    def __init__(self, offset, text_size='9pt'):
        self.text_size = text_size
        LegendItem.__init__(self, None, offset)
    
    def addItem(self, item, name):
        label = pg.LabelItem(name, size=self.text_size)
        if isinstance(item, ItemSample):
            sample = item
        else:
            sample = ItemSample(item)
        self.layout.setContentsMargins(0, 0, 0, 0)
        row = self.layout.rowCount()
        self.items.append((sample, label))
        self.layout.addItem(label, row, 1)
        self.updateSize()
        
    def paint(self, p, *args):
        p.setPen(pg.functions.mkPen(255,255,255,100))
        p.setBrush(pg.functions.mkBrush(0,0,0,100))
        p.drawRect(self.boundingRect())

def get_help_lines():
    ''' print help lines to the console '''
    
    helplines = []
    helplines.append('q            - quit')
    helplines.append('p            - save png')
    if not os.name == 'nt':
        helplines.append('s            - save svg')
    helplines.append('space        - next ray')
    helplines.append('click        - enter subplot')
    helplines.append('right-click  - exit subplot')
    helplines.append('h/F1         - show component list')
    
    return helplines

def create_help_pltitm():
    
    plt = pg.PlotItem(enableMenu=False)
    plt.axes['left']['item'].hide()
    plt.axes['bottom']['item'].hide()
    
    plt.legend = ModLegend(offset=(-140, 60))
    plt.legend.setParentItem(plt.vb)

    for l in get_help_lines():
        plt.plot([0], [0], name=l)
    
    return plt

def create_infowindow(comp_colour_pairs):
    class InfoWindow(QtGui.QMainWindow):
        ''' infowindow that is designed to be static '''
        class Ui_InfoWindow(object):
            ''' info window widgets (auto-generated code) '''
            def setupUi(self, MainWindow):
                MainWindow.setObjectName("MainWindow")
                MainWindow.resize(259, 395)
                MainWindow.setStyleSheet("background-color: rgb(0, 0, 0);")
                self.centralwidget = QtGui.QWidget(MainWindow)
                self.centralwidget.setObjectName("centralwidget")
                self.verticalLayoutTechnicalReason = QtGui.QVBoxLayout(self.centralwidget)
                self.verticalLayoutTechnicalReason.setObjectName("verticalLayoutTechnicalReason")
                self.scrollArea = QtGui.QScrollArea(self.centralwidget)
                self.scrollArea.setWidgetResizable(True)
                self.scrollArea.setObjectName("scrollArea")
                self.scrollAreaWidgetContents = QtGui.QWidget()
                self.scrollAreaWidgetContents.setGeometry(QtCore.QRect(0, 0, 239, 375))
                self.scrollAreaWidgetContents.setObjectName("scrollAreaWidgetContents")
                self.vlayout = QtGui.QVBoxLayout(self.scrollAreaWidgetContents)
                self.vlayout.setObjectName("vlayout")
                MainWindow.setCentralWidget(self.centralwidget)
                
                self.scrollArea.setWidget(self.scrollAreaWidgetContents)
                self.verticalLayoutTechnicalReason.addWidget(self.scrollArea)
                
                self.labels = []
                self.spacerItem = QtGui.QSpacerItem(20, 448, QtGui.QSizePolicy.Minimum, QtGui.QSizePolicy.Expanding)
                
        def set_components(self, str_colour_pairs):
            ''' colours are tri-tupples of rgb '''
            for pair in str_colour_pairs:
                s = pair[0]
                c = pair[1]
                
                lbl = QtGui.QLabel(self.ui.scrollAreaWidgetContents)
                lbl.setText(s)
                lbl.setStyleSheet("color: rgb(%d, %d, %d);" % c)
                self.ui.labels.append(lbl)
                self.ui.vlayout.addWidget(lbl)
            
            self.ui.vlayout.addItem(self.ui.spacerItem)
        
        def __init__(self, parent=None):
            super(InfoWindow, self).__init__(parent)
            
            # create ui and set info
            self.ui = self.Ui_InfoWindow()
            self.ui.setupUi(self)
    
    iw = InfoWindow()
    iw.set_components(comp_colour_pairs)
    
    return iw

class McDisplay2DGui(object):
    ''' view, viewmodel and controller all in one object '''
    class ZoomState(Enum):
        ZOOM = 0
        UNZOOM = 1
    
    def __init__(self, title):
        #
        # app, scene, plots 
        #
        self.app = QtGui.QApplication(sys.argv)
        
        window = pg.GraphicsWindow()
        window.resize(1000, 600)
        
        mw = QtGui.QMainWindow()
        window.setParent(mw)
        mw.setCentralWidget(window)
        mw.setWindowTitle(title)
        
        mw.show()
        mw.raise_()
        mw.activateWindow()
        self.mw = mw
        
        layout = pg.GraphicsLayout()
        window.setCentralItem(layout)
        layout.window = window # keep window to avoid garbage collection
        layout.setContentsMargins(2, 2, 2, 2) # outermost margin
        self.layout = layout
        
        self.ray_idx = 0
        self.rayplots = []
        
        self.iw = None
        self.iw_visible = False
    
    def _init_2dmode(self):
        self.plt_zy = pg.PlotItem(enableMenu=False)
        self.plt_xy = pg.PlotItem(enableMenu=False)
        self.plt_zx = pg.PlotItem(enableMenu=False)
        self.plt_help = create_help_pltitm()
        
        self._unzoom()
        
        def unzoom_handler(event):
            if event.button() != 2:
                return
            if self.zoomstate == self.ZoomState.ZOOM:
                self._unzoom()
        
        def zoom_handler(event, item=None, idx=None):
            if event.button() != 1:
                return
            if self.zoomstate == self.ZoomState.UNZOOM and event.currentItem == item:
                self._zoom(idx)
        
        self.zoomstate = self.ZoomState.UNZOOM
        
        self.plt_zy.scene().sigMouseClicked.connect(lambda event: zoom_handler(event=event, item=self.plt_zy.getViewBox(), idx=0))
        self.plt_xy.scene().sigMouseClicked.connect(lambda event: zoom_handler(event=event, item=self.plt_xy.getViewBox(), idx=1))
        self.plt_zx.scene().sigMouseClicked.connect(lambda event: zoom_handler(event=event, item=self.plt_zx.getViewBox(), idx=2))
        
        self.layout.scene().sigMouseClicked.connect(unzoom_handler)
        
        self.layout.scene().keyPressEvent = self._key_handler
        
        print('')
        print('\n'.join(get_help_lines()))

    def _key_handler(self, event):
        ''' global keypress handler '''
        if False:
            print(event.key())
        
        if event.key() == 66:                   # b
            if self.zoomstate == self.ZoomState.ZOOM:
                self._unzoom()
        if event.key() == 81:                   # q
            QtGui.QApplication.quit()
        elif event.key() == 80:                 # p
            self._dumpfile(format='png')
        elif event.key() == 83:                 # s
            if not os.name == 'nt':
                self._dumpfile(format='svg')
        elif event.key() in [32, 16777268]:  # space, F5
            self._display_nextray()
        elif event.key() in [72, 16777264]:  # h, F1
            if not self.iw_visible:
                self.iw = create_infowindow(self._get_comp_color_pairs())
                self.iw.show()
                self.iw_visible = True
                self.mw.activateWindow()
            else:
                self.iw.hide()
                self.iw_visible = False
        
    def _dumpfile(self, format):
        utils.dumpfile_pqtg(scene=self.layout.scene(), filenamebase='mcdisplay', format=format)
    
    def _get_comp_color_pairs(self):
        ''' extracts component names and matches then with colours in the natural order '''
        lst = []
        numcolours = len(colours)
        for idx in range(len(self.instr.components)):
            tpl = (self.instr.components[idx].name, colours[idx%numcolours])
            lst.append(tpl)
        return lst
    
    def run_ui(self, instr, rays):
        '''  '''
        self._init_2dmode()
        self._set_and_plot_instr(instr)
        self._set_rays(rays)
        self._unzoom()
        self._display_nextray()
        return self.app.exec_()
    
    def run_ui_tof(self, instr, rays):
        '''  '''
        self.instr = instr
        
        # plot instrument
        plt = pg.PlotItem(enableMenu=False)
        # get max t_min from ray events 
        t_min = 0 
        t_max = 0
        for story in rays:
            for g in story.groups:
                for state in g.events:
                    t_min = min(t_min, state.get_time())
                    t_max = max(t_max, state.get_time())
        
        plot_tof_instr(instr, t_min, t_max, plt)
        self.layout.addItem(plt)
        
        # plot rays
        plot_1d_tof_rays(instr, rays, plt)
        
        self.layout.scene().keyPressEvent = self._key_handler
        
        return self.app.exec_()
    
    def _set_and_plot_instr(self, instr, enable_clickable=False):
        ''' set internal references to the full instrument and three 2d instrument set of coordinate pairs '''
        self.instr = instr
        
        # get instrument 2d projections
        self.instr_zy = get_2d_instrument(instr, 'zy')
        self.instr_xy = get_2d_instrument(instr, 'xy')
        self.instr_zx = get_2d_instrument(instr, 'zx')
        
        # plot instrument three times
        comp_plotdataitm_pairs_zy = plot_2d_instr(self.instr_zy, self.plt_zy, 'z/[m]', 'y/[m]')
        comp_plotdataitm_pairs_xy = plot_2d_instr(self.instr_xy, self.plt_xy, 'x/[m]', 'y/[m]')
        comp_plotdataitm_pairs_zx = plot_2d_instr(self.instr_zx, self.plt_zx, 'z/[m]', 'x/[m]')
        
        # set PlotDataItem click events
        if enable_clickable:
            for pairs in [comp_plotdataitm_pairs_zy, comp_plotdataitm_pairs_xy, comp_plotdataitm_pairs_zx]:
                for p in pairs:
                    comp = p[0]
                    itm = p[1]
                    itm.curve.setClickable(True)
                    itm.curve.mouseClickEvent = lambda event, comp=comp: self._handle_comp_clicked(event, comp)

    def _handle_comp_clicked(self, event, comp):
        ''' display clicked component info '''
        print(comp)
        # prevent event propagation (e.g. _zoom)
        event.accept()
    
    def _set_rays(self, rays):
        ''' set a reference to rays '''
        self.rays = rays
    
    def _display_nextray(self):
        ''' plots the next ray to the three plot windows '''
        ray = self.rays[self.ray_idx % len(self.rays)]
        
        self.ray_zy = get_2d_ray(ray, self.instr, 'zy')
        self.ray_xy = get_2d_ray(ray, self.instr, 'xy')
        self.ray_zx = get_2d_ray(ray, self.instr, 'zx')
        
        for plt in self.rayplots:
            plt.clear()
        
        plt0 = plot_2d_ray(self.ray_zy, self.plt_zy)
        plt1 = plot_2d_ray(self.ray_xy, self.plt_xy)
        plt2 = plot_2d_ray(self.ray_zx, self.plt_zx)
        
        self.rayplots = (plt0, plt1, plt2)
        
        self.ray_idx += 1
    
    def _clear(self):
        ''' prepare for a new _zoom state '''
        self.layout.clear()
    
    def _zoom(self, idx_subwin):
        ''' _zoom action, plot a single view full-window '''
        self._clear()
        
        if idx_subwin == 0: self.layout.addItem(self.plt_zy, 0, 0)
        if idx_subwin == 1: self.layout.addItem(self.plt_xy, 0, 0)
        if idx_subwin == 2: self.layout.addItem(self.plt_zx, 0, 0)
        
        self.zoomstate = self.ZoomState.ZOOM
    
    def _unzoom(self):
        ''' _unzoom action, plot the overview window with the three side views along each axis '''
        self._clear()
        
        self.layout.addItem(self.plt_zy, 0, 0)
        self.layout.addItem(self.plt_xy, 0, 1)
        self.layout.addItem(self.plt_zx, 1, 0)
        self.layout.addItem(self.plt_help, 1, 1)
        
        self.zoomstate = self.ZoomState.UNZOOM

def debug_load_instr(filename):
    instrdef = open(filename).read()
    
    instrparser = InstrTraceParser(instrdef)
    instrbuilder = InstrObjectConstructor(instrparser.parsetree)
    return instrbuilder.build_instr()

def debug_load_rays(filename):
    particles = open(filename).read()
    
    parser = FlowChartParticleTraceParser()
    rays = parser.execute(particles)
    return rays

def get_datadirname(instrname):
    ''' returns an mcrun-like name-date-time string '''
    return "%s_%s" % (instrname, datetime.strftime(datetime.now(), "%Y%m%d_%H%M%S"))

def debug_file_save(data, filename):
    ''' saves data for debug purposes '''
    f = open(filename, 'w')
    f.write(data)
    f.close()

def main(args):
    ''' script execution '''
    logging.basicConfig(level=logging.INFO)

    # output directory
    dirname = get_datadirname(os.path.splitext(os.path.basename(args.instr))[0])
    if args.dirname:
        dirname = args.dirname

    # set up a pipe, read and parse the particle trace
    reader = McDisplayReader(args, n=100, dir=dirname)
    instrument = reader.read_instrument()
    raybundle = reader.read_particles()

    if args.invcanvas:
        ## Switch to using white background and black foreground
        pg.setConfigOption('background', 'w')
        pg.setConfigOption('foreground', 'k')

    gui = McDisplay2DGui(title=dirname+" - Press 'h' for comp list")
    try:
      if not args.tof and not args.TOF and not args.ToF:
        sys.exit(gui.run_ui(instrument, raybundle.rays))
      else:
        sys.exit(gui.run_ui_tof(instrument, raybundle.rays))
    except:
      sys.exit(gui.run_ui(instrument, raybundle.rays))


if __name__ == '__main__':
    scriptname=pathlib.Path(__file__).stem
    parser = argparse.ArgumentParser(description=__doc__.replace('mcdisplay',scriptname))
    parser.add_argument('instr', help='display this instrument file (.instr or .out)')
    parser.add_argument('--default', action='store_true', help='automatically use instrument defaults for simulation run')
    #enable tof for mcdisplay (McStas) only
    if( scriptname.startswith('mc') ):
      parser.add_argument('--tof', action='store_true', help='enable time-of-flight mode')
      parser.add_argument('--TOF', action='store_true', help='alternative to --tof')
      parser.add_argument('--ToF', action='store_true', help='another alternative to --tof')
    parser.add_argument('--dirname', help='output directory name override')
    parser.add_argument('--inspect', help='display only particle rays reaching this component')
    parser.add_argument('--invcanvas', action='store_true', help='invert canvas background from black to white')
    parser.add_argument('instr_options', nargs='*', help='simulation options and instrument params')

    args, unknown = parser.parse_known_args()
    # if --inspect --first or --last are given after instr, the remaining args become "unknown",
    # but we assume that they are instr_options
    if len(unknown)>0:
        args.instr_options = unknown

    # enable ^C termination
    import signal
    signal.signal(signal.SIGINT, signal.SIG_DFL)

    try:
        main(args)
    except KeyboardInterrupt:
        print('')
    except Exception as e:
        print(e)

