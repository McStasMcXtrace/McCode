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
    
    # title
    plt.setLabels(title=data.title, bottom=data.xlabel, left=data.ylabel)
    # error bars
    beam = 0
    if len(x) > 1:
        beam = x[1]-x[0]/0.8
    height = np.array(data.y_err_vals)
    err = pg.ErrorBarItem(x=x, y=y, height=height, beam=beam)
    plt.addItem(err)
    
    plt.plot(x, y)

def plot_Data2D(data, plt):
    ''' populate plt with data, 2D version '''
    img = pg.ImageItem()
    plt.addItem(img)
    plt.setLabels(title=data.title, bottom=data.xlabel, left=data.ylabel)
    img.setImage(np.array(data.zvals))

