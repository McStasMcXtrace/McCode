#!/usr/bin/env python3
# -*- coding: utf-8 -*-
'''
mcdisplay webgl script.
'''
import sys
import os
import logging
import argparse
import json
import subprocess
from datetime import datetime
import numpy as np
from enum import Enum

import PyQt4
import pyqtgraph as pg
from pyqtgraph.Qt import QtGui, QtCore

sys.path.append(os.path.join(os.path.dirname(__file__), '..', '..'))

from mccodelib import mccode_config
from mccodelib.mcdisplayutils import McDisplayReader
from mccodelib.instrgeom import Vector3d, DrawLine, DrawMultiline
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
        transform = [c.transform for c in instr.components if c.name == group.compname][0]
        for e in group.events:
            p = transform.apply(Vector3d(e.args[0], e.args[1], e.args[2]))
            coords.append((p[k1], p[k2]))
    
    return coords

def plot_2d_ray(coords, plt):
    ''' see get_2d_ray to understand the data structure '''
    
    x = np.array([p[0] for p in coords])
    y = np.array([p[1] for p in coords])
    return plt.plot(x, y, pen=pg.mkPen(color=(255, 255, 255)))

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
        coords_sets.append((c.name, comp_coord_sets))
    
    return coords_sets

def plot_2d_instr(coords_sets, plt):
    ''' see get_2d_instrument impl to understand the data structure '''
    idx = 0
    def get_next_colour(idx):
        colours = [(100, 0, 255), (30, 255, 100), (250, 150, 255), (150, 150, 115), (255, 255, 0), (0, 150, 0)]
        colour = colours[idx % len(colours)]
        idx += 1
        return colour
    
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
            
        plt.plot(x_comp, y_comp, connect=connect_comp, pen=pg.mkPen(color=colour))

    # Fix the axes to have a common x and y zooom factor
    AxisBounds=plt.getViewBox().childrenBounds()
    x0=(AxisBounds[0][1]+AxisBounds[0][0])
    y0=(AxisBounds[1][1]+AxisBounds[1][0])
    dx=AxisBounds[0][1]-AxisBounds[0][0]
    dy=AxisBounds[0][1]-AxisBounds[0][0]
    if dy>=dx:
        dx=dy
    else:
        dy=dx
    plt.getViewBox().setRange(xRange=[x0-dx/2,x0+dx/2],yRange=[y0-dy/2,y0+dy/2])
    
class McDisplay2DGui(object):
    class ZoomState(Enum):
        ZOOM = 0
        UNZOOM = 1
    
    def __init__(self):
        #
        # app, scene, plots 
        #
        
        self.app = QtGui.QApplication(sys.argv)
        
        window = pg.GraphicsWindow()
        layout = pg.GraphicsLayout()
        
        window.resize(1000, 600)
        window.setWindowTitle('2d-mcdisplay')
        window.setCentralItem(layout)
        
        layout.window = window # keep window to avoid garbage collection
        layout.setContentsMargins(2, 2, 2, 2) # outermost margin
        
        self.layout = layout
        self.plt_zy = pg.PlotItem(enableMenu=False)
        self.plt_xy = pg.PlotItem(enableMenu=False)
        self.plt_zx = pg.PlotItem(enableMenu=False)
        
        self.ray_idx = 0
        self.rayplots = []
        
        #
        # zoom stuff
        #
        self.unzoom()
        
        def unzoom_handler(event):
            if event.button() != 2:
                return
            if self.zoomstate == self.ZoomState.ZOOM:
                self.unzoom()
        
        def zoom_handler(event, item=None, idx=None):
            if event.button() != 1:
                return
            if self.zoomstate == self.ZoomState.UNZOOM and event.currentItem == item:
                self.zoom(idx)
        
        self.zoomstate = self.ZoomState.UNZOOM
        
        self.plt_zy.scene().sigMouseClicked.connect(lambda event: zoom_handler(event=event, item=self.plt_zy.getViewBox(), idx=0))
        self.plt_xy.scene().sigMouseClicked.connect(lambda event: zoom_handler(event=event, item=self.plt_xy.getViewBox(), idx=1))
        self.plt_zx.scene().sigMouseClicked.connect(lambda event: zoom_handler(event=event, item=self.plt_zx.getViewBox(), idx=2))
        
        self.layout.scene().sigMouseClicked.connect(unzoom_handler)
        
        #
        # keypress events stuff
        #
        def key_handler(event):
            ''' global keypress handler '''
            if False:
                print(event.key())
            
            if event.key() == 81:                # q
                quit()
            elif event.key() in [32, 16777268]:  # space, F5
                self._display_nextray()
            elif event.key() in [72, 16777264]:  # h, F1
                print_help()
        
        def print_help():
            ''' print help lines to the console '''
            helplines = []
            helplines.append('q            - quit')
            helplines.append('space        - next ray')
            helplines.append('click        - zoom')
            helplines.append('right-click  - zoom out')
            helplines.append('h/F1         - help')
            print('\n'.join(helplines))
        
        # add generic handlers
        self.layout.scene().keyPressEvent = key_handler
        print_help()
    
    def run_ui(self):
        '''  '''
        
        self.unzoom()
        self._display_nextray()
        return self.app.exec_()
    
    def set_instr_and_plot(self, instr):
        ''' set internal references to the full instrument and three 2d instrument set of coordinate pairs '''
        self.instr = instr
        self.instr_zy = get_2d_instrument(instr, 'zy')
        self.instr_xy = get_2d_instrument(instr, 'xy')
        self.instr_zx = get_2d_instrument(instr, 'zx')
        
        plot_2d_instr(self.instr_zy, self.plt_zy)
        plot_2d_instr(self.instr_xy, self.plt_xy)
        plot_2d_instr(self.instr_zx, self.plt_zx)
        
    def set_rays(self, rays):
        ''' just set a reference to rays '''
        self.rays = rays
    
    def _display_nextray(self):
        '''  '''
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
        self.layout.clear()
        try:
            self.layout.scene.sigMouseClicked.disconnect()
        except:
            pass
        try:
            self.plt_zy.scene().sigMouseClicked.disconnect()
        except:
            pass
        try:
            self.plt_xy.scene().sigMouseClicked.disconnect()
        except:
            pass
        try:
            self.plt_zx.scene().sigMouseClicked.disconnect()
        except:
            pass
    
    def zoom(self, idx_subwin):
        ''' zoom action, plot a single view full-window '''
        self._clear()
        
        if idx_subwin == 0: self.layout.addItem(self.plt_zy, 0, 0)
        if idx_subwin == 1: self.layout.addItem(self.plt_xy, 0, 0)
        if idx_subwin == 2: self.layout.addItem(self.plt_zx, 0, 0)
        
        self.zoomstate = self.ZoomState.ZOOM
    
    def unzoom(self):
        ''' unzoom action, plot the overview window with the three side views along each axis '''
        self._clear()
        
        self.layout.addItem(self.plt_zy, 0, 0)
        self.layout.addItem(self.plt_xy, 0, 1)
        self.layout.addItem(self.plt_zx, 1, 0)
        
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
    
    gui = McDisplay2DGui()
    gui.set_instr_and_plot(instrument)
    gui.set_rays(raybundle.rays)

    sys.exit(gui.run_ui())


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('instr', help='display this instrument file (.instr or .out)')
    parser.add_argument('--default', action='store_true', help='automatically use instrument defaults for simulation run')
    parser.add_argument('--dirname', help='output directory name override')
    parser.add_argument('--inspect', help='display only particle rays reaching this component')
    parser.add_argument('--first', help='zoom range first component name')
    parser.add_argument('--last', help='zoom range last component name')
    parser.add_argument('instr_options', nargs='*', help='simulation options and instrument params')
    
    args, unknown = parser.parse_known_args()
    # if --inspect --first or --last are given after instr, the remaining args become "unknown",
    # but we assume that they are instr_options
    if len(unknown)>0:
        args.instr_options = unknown
    
    try:
        main(args)
    except KeyboardInterrupt:
        print('')
    except Exception as e:
        print(e)
        raise e

