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

    plt.setXRange(np.min(data.xvals), np.max(data.xvals), padding=0)
        
    # labels
    plt.setLabels(title=data.title, bottom=data.xlabel, left=data.ylabel)
    
    # error bars
    beam = 0
    if len(x) > 1:
        beam = (x[1]-x[0])*0.8

    height = np.array(data.y_err_vals).astype(np.float)

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
       
    if log:
        datashape = dataset.shape
        datset=np.reshape(dataset, (1, datashape[0]*datashape[1]))
        ymin = np.min(dataset[dataset>0])/10
        dataset[dataset<=0] = ymin
        dataset=np.reshape(dataset, datashape)
        dataset=np.log(dataset)
        #        dataset[np.where(np.array(data.zvals) <= 0)]=ymin

    img.setImage(dataset)
    
    # color map (by lookup table)
    pos_min = np.min(dataset)
    pos_max = np.max(dataset)
    
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
    plt.getViewBox().autoRange(padding=0)
    
    # color bar
    cbimg = pg.ImageItem()
    numsteps = 100

    arr_1 = (pos_max - pos_min) / pos_max * range(numsteps) + pos_min
    print(pos_max)
    print(pos_min)
    print(arr_1)
    cbimg.setImage(np.array([arr_1]))

    cbimg.setLookupTable(lut)
    colorbar = layout.addPlot(1, 1)
    colorbar.addItem(cbimg)
    colorbar.setFixedWidth(65)

    colorbar.axes['top']['item'].show()
    colorbar.axes['top']['item'].setStyle(showValues=False)
    colorbar.axes['bottom']['item'].show()
    colorbar.axes['bottom']['item'].setStyle(showValues=False)
    colorbar.axes['left']['item'].show()
    colorbar.axes['left']['item'].setStyle(showValues=False)
    colorbar.axes['right']['item'].show()
    colorbar.axes['right']['item'].setScale(pos_max-pos_min)

    colorbar.getViewBox().autoRange(padding=0)
    
    # return layout so it doesn't get garbage collected, but the proper plot viewBox pointer for click events
    return layout, plt.getViewBox()

