'''
Atomic plot functions for mcplot-pyqtgraph
'''
import numpy as np
import pyqtgraph as pg

def plot_Data1D(data, plt):
    ''' populate plt with data, 1D version '''
    # data
    x = np.array(data.xvals).astype(np.float)
    y = np.array(data.yvals).astype(np.float)
    
    # labels
    plt.setLabels(title=data.title, bottom=data.xlabel, left=data.ylabel)
    
    # error bars
    beam = 0
    if len(x) > 1:
        beam = (x[1]-x[0])*0.8
    height = np.array(data.y_err_vals).astype(np.float)
    err = pg.ErrorBarItem(x=x, y=y, height=height, beam=beam)
    plt.addItem(err)
    
    # commit
    plt.plot(x, y)

def plot_Data2D(data, plt):
    ''' populate plt with data, 2D version '''
    # data
    img = pg.ImageItem()
    img.setImage(np.array(data.zvals))
    
    # color map (by lookup table)
    pos_min = np.min(data.zvals)
    pos_max = np.max(data.zvals)
    pos = [pos_min, pos_min + 1/2*(pos_max-pos_min), pos_max]
    
    color = np.array([[0, 0, 0, 255], [255, 128, 0, 255], [255, 255, 0, 255]], dtype=np.ubyte)
    
    cm = pg.ColorMap(pos, color)
    lut = cm.getLookupTable(pos_min, pos_max, 256)
    img.setLookupTable(lut=lut)
    
    # labels
    plt.setLabels(title=data.title, bottom=data.xlabel, left=data.ylabel)
    
    # commit
    plt.addItem(img)
