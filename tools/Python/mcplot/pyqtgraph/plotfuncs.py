'''
Atomic plot functions for mcplot-pyqtgraph
'''
import numpy as np
import pyqtgraph as pg

def plot_Data1D(data):
    ''' create a plotItem and populate it with data, Data1D '''
    # plot item
    plt = pg.PlotItem()
    
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
    plt.setMenuEnabled(False)
    vb = plt.getViewBox()
    # prevent garbage collection
    #vb.plt = plt
    return plt, vb

def plot_Data2D(data):
    ''' creat a layout and populate a plotItem with data Data2D, adding a color bar '''
    # data
    img = pg.ImageItem()
    img.setImage(np.array(data.zvals))
    
    # color map (by lookup table)
    pos_min = np.min(data.zvals)
    pos_max = np.max(data.zvals)
    pos = [pos_min, pos_min + 1/2*(pos_max-pos_min), pos_max]
    
    color = np.array([[0, 0, 0, 255], [255, 128, 0, 255], [255, 255, 0, 255]], dtype=np.ubyte)
    
    colormap = pg.ColorMap(pos, color)
    lut = colormap.getLookupTable(pos_min, pos_max, 256)
    img.setLookupTable(lut)
    
    # graphics layout with a plotitem and a gradienteditoritem
    layout = pg.GraphicsLayout()

    plt = layout.addPlot(0, 0)
    plt.setLabels(title=data.title, bottom=data.xlabel, left=data.ylabel)
    plt.setMenuEnabled(False)
    
    plt.addItem(img)
    
    colorbar = pg.GradientEditorItem(orientation='right', allowAdd=False)
    colorbar.showMenu = lambda ev: None
    colorbar.removeTick(colorbar.getTick(0), finish=False)
    colorbar.removeTick(colorbar.getTick(0), finish=False)
    layout.addItem(colorbar, 0, 1)
    
    return layout, plt.getViewBox()

