'''
Atomic plot functions for mcplot-pyqtgraph
'''
import numpy as np
import pyqtgraph as pg

def plot_Data1D(data, log=False):
    ''' create a plotItem and populate it with data, Data1D '''
    plt = pg.PlotItem()
    
    # data
    x = np.array(data.xvals).astype(np.float)
    y = np.array(data.yvals).astype(np.float)
    e = np.array(data.y_err_vals).astype(np.float)

    if log:
        nonzeros=[]
        zeros=[]
        for i in range(len(y)):
            if y[i]>0:
                nonzeros.append(i)
            else:
                zeros.append(i)
        y[zeros] = np.min(y[nonzeros])/10
        plt.setLogMode(y=True)
    else:
        plt.setLogMode(y=False)

    plt.setXRange(np.min(x), np.max(x), padding=0)
        
    # labels
    title = '%s' % (data.title)
    plt.setLabels(title=title, bottom=data.xlabel, left=data.ylabel)
    
    # error bars
    beam = 0
    if len(x) > 1:
        beam = (x[1]-x[0])*0.5
    
    # TODO: Find solution for adding errorbars in the log case
    if not log:
        err = pg.ErrorBarItem(x=x, y=y, height=e, beam=beam)
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
    dataset = np.array(data.zvals)
    datashape = dataset.shape
    
    ymin = 1e-19
    if log:
        dataset = np.reshape(dataset, (1, datashape[0]*datashape[1]))
        ymin = np.min(dataset[dataset>0])/10
        dataset[dataset<=0] = ymin
        dataset = np.reshape(dataset, datashape)
        dataset = np.log10(dataset)

    img.setImage(dataset)
    
    # scale(x,y) is in %, translate(x,y) is in the original units
    img.scale((data.xlimits[1] - data.xlimits[0])/datashape[0], (data.xlimits[3] - data.xlimits[2])/datashape[1])
    img.translate(-datashape[0]/2, -datashape[1]/2)
    
    # color map (by lookup table)
    pos_min = np.min(dataset)
    pos_max = np.max(dataset)
    pos = [pos_min, pos_min + 1/2*(pos_max-pos_min), pos_max]
    colormaps={
        'thermal'  : np.array([[185, 0, 0, 255], [255, 220, 0, 255], [255, 255, 255, 255], [0, 0, 0, 255]], dtype=np.ubyte),
        'flame'    : np.array([[7, 0, 220, 255], [236, 0, 134, 255], [246, 246, 0, 255], [255, 255, 255, 255], [0, 0, 0, 255]], dtype=np.ubyte),
        'yellowy'  : np.array([[0, 0, 0, 255], [32, 0, 129, 255], [255, 255, 0, 255], [115, 15, 255, 255], [255, 255, 255, 255]], dtype=np.ubyte),
        'bipolar'  : np.array([[0, 255, 255, 255], [255, 255, 0, 255], [0, 0, 0, 255], [0, 0, 255, 255], [255, 0, 0, 255]], dtype=np.ubyte),
        'greyclip' : np.array([[0, 0, 0, 255], [255, 255, 255, 255], [255, 0, 0, 255]], dtype=np.ubyte),
        'nice'     : np.array([[0, 0, 0, 255], [255, 128, 0, 255], [255, 255, 0, 255]], dtype=np.ubyte),
        }
    colormap = pg.ColorMap(pos, colormaps['nice'])
    lut = colormap.getLookupTable(pos_min, pos_max, 256)
    img.setLookupTable(lut)
    
    # graphics layout with a plotitem and a gradienteditoritem
    layout = pg.GraphicsLayout()
    layout.setContentsMargins(0, 0, 20, 5)
    
    # title label
    layout.addLabel(data.title, 0, 0, colspan=2)
    
    # plot area
    plt = layout.addPlot(1, 0)
    plt.setLabels(bottom=data.xlabel, left=data.ylabel)
    plt.setMenuEnabled(False)
    plt.addItem(img)
    plt.getViewBox().autoRange(padding=0)
    
    # color bar
    cbimg = pg.ImageItem()
    numsteps = 100
    arr_1 = pos_min + (pos_max - pos_min) * np.arange(numsteps)/(numsteps)
    cbimg.setImage(np.array([arr_1]))
    # calculate scaling and translation for the y-axis
    dy = (pos_max - pos_min) / numsteps
    ty = (pos_min) / dy
    cbimg.scale(1, dy)
    cbimg.translate(0,ty)
    
    cbimg.setLookupTable(lut)
    
    colorbar = layout.addPlot(1, 1)
    colorbar.addItem(cbimg)
    colorbar.setFixedWidth(40)
    
    pg.GradientEditorItem
    
    colorbar.axes['top']['item'].show()
    colorbar.axes['top']['item'].setStyle(showValues=False)
    colorbar.axes['bottom']['item'].show()
    colorbar.axes['bottom']['item'].setStyle(showValues=False)
    colorbar.axes['left']['item'].show()
    colorbar.axes['left']['item'].setStyle(showValues=False)
    colorbar.axes['right']['item'].show()
    
    colorbar.getViewBox().autoRange(padding=0)
    
    # return layout so it doesn't get garbage collected, but the proper plot viewBox pointer for click events
    return layout, plt.getViewBox()

