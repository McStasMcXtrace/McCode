'''
Atomic plot functions for mcplot-pyqtgraph
'''
import numpy as np

def plot_Data1D(data, plt):
    ''' populate plt with data, 1D version '''
    x = np.array(data.xvals).astype(np.float)
    y = np.array(data.yvals).astype(np.float)
    plt.setWindowTitle(data.title)
    #height = np.array(data1.y_err_vals)
    #err = pg.ErrorBarItem(x=x, y=y, height=height, beam=0.02)
    #p1.addItem(err)
    plt.plot(x, y)

def plot_Data2D(data, plt):
    ''' populate plt with data, 2D version '''
    print("Data2D plots have not been implemented.")    
