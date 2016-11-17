'''
Atomic plot functions for mcplot-pyqtgraph
'''
import numpy as np
import pyqtgraph as pg

def plot_Data1D(data, log=False):
    ''' create a plotItem and populate it with data, Data1D '''
    plt = pg.PlotItem()
    
    # data
    if log:
        x = np.log(np.array(data.xvals).astype(np.float))
        y = np.log(np.array(data.yvals).astype(np.float))
    else:
        x = np.array(data.xvals).astype(np.float)
        y = np.array(data.yvals).astype(np.float)
    
    # labels
    plt.setLabels(title=data.title, bottom=data.xlabel, left=data.ylabel)
    
    # error bars
    beam = 0
    if len(x) > 1:
        beam = (x[1]-x[0])*0.8
    if log:
        height = np.log(np.array(data.y_err_vals).astype(np.float))
    else:
        height = np.array(data.y_err_vals).astype(np.float)
    err = pg.ErrorBarItem(x=x, y=y, height=height, beam=beam)
    
    plt.addItem(err)
    
    # commit
    plt.plot(x, y)
    plt.setMenuEnabled(False)
    vb = plt.getViewBox()
    
    return plt, vb

def plot_Data2D(data, log=False):
    ''' create a layout and populate a plotItem with data Data2D, adding a color bar '''
    
    # data
    img = pg.ImageItem()
    img.setImage(np.array(data.zvals))    
    
    if log:
        print("2D log scale not yet implemented")
    
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
    
    # title label
    layout.addLabel(data.title, 0, 0, colspan=2)
    
    # plot area
    plt = layout.addPlot(1, 0)
    plt.setLabels(bottom=data.xlabel, left=data.ylabel)
    plt.setMenuEnabled(False)
    plt.addItem(img)
    #ticks = plt.axes['bottom']['item'].tickValues(0, 128, 16)
    #plt.axes['bottom']['item'].setTicks(ticks)
    
    # color bar
    cbimg = pg.ImageItem()
    numsteps = 100
    arr_1 = (pos_max - pos_min) / pos_max * range(numsteps)
    arr_2 = np.zeros(numsteps)
    cbimg.setImage(np.array([arr_1, arr_2]))

    cbimg.setLookupTable(lut)
    colorbar = layout.addPlot(1, 1)
    colorbar.addItem(cbimg)
    colorbar.setFixedWidth(80)
    
    colorbar.axes['bottom']['item'].hide()
    colorbar.axes['left']['item'].hide()
    colorbar.axes['right']['item'].show()
    colorbar.axes['right']['item'].setScale(pos_max-pos_min)
    
    # return layout so it doesn't get garbage collected, but the proper plot viewBox pointer for click events
    return layout, plt.getViewBox()

