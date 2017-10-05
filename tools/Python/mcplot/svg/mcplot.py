#!/usr/bin/env python
import argparse
import logging
import os
import sys
import numpy as np
import scipy.misc
import io
import base64

sys.path.append(os.path.join(os.path.dirname(__file__), '..', '..'))

from mccodelib.mcplotloader import McCodeDataLoader, Data1D, Data2D
from mccodelib.plotgraph import PNSingle

def fillout_template_1d(template, x, y, yerr, xlabel, ylabel, title):
    '''  '''
    template = template.replace('@X_DATA_ARRAY@', x.__str__())
    template = template.replace('@Y_DATA_ARRAY@', y.__str__())
    template = template.replace('@YERR_DATA_ARRAY@', yerr.__str__())
    template = template.replace('@WIDTH@', str(500))
    template = template.replace('@HEIGHT@', str(300))
    
    template = template.replace('@XLABEL@', xlabel)
    template = template.replace('@YLABEL@', ylabel)
    template = template.replace('@TITLE@', title)
    
    return template

def fillout_template_2d(template, xmin, xmax, ymin, ymax, image_str, xlabel, ylabel, title):
    '''  '''
    template = template.replace('@XMIN@', str(xmin))
    template = template.replace('@XMAX@', str(xmax))
    template = template.replace('@YMIN@', str(ymin))
    template = template.replace('@YMAX@', str(ymax))

    template = template.replace('@IMAGE@', image_str)
    
    template = template.replace('@WIDTH@', str(500))
    template = template.replace('@HEIGHT@', str(300))
    
    template = template.replace('@XLABEL@', xlabel)
    template = template.replace('@YLABEL@', ylabel)
    template = template.replace('@TITLE@', title)
    
    return template

def get_cm():
    return np.array([[  0,   0, 143, 255], [  0,   0, 159, 255], [  0,   0, 175, 255], [  0,   0, 191, 255], [  0,   0, 207, 255], [  0,   0, 223, 255], [  0,   0, 239, 255], [  0,   0, 255, 255], [  0,  16, 255, 255], [  0,  32, 255, 255], [  0,  48, 255, 255], [  0,  64, 255, 255], [  0,  80, 255, 255], [  0,  96, 255, 255], [  0, 112, 255, 255], [  0, 128, 255, 255], [  0, 143, 255, 255], [  0, 159, 255, 255], [  0, 175, 255, 255], [  0, 191, 255, 255], [  0, 207, 255, 255], [  0, 223, 255, 255], [  0, 239, 255, 255], [  0, 255, 255, 255], [ 16, 255, 239, 255], [ 32, 255, 223, 255], [ 48, 255, 207, 255], [ 64, 255, 191, 255], [ 80, 255, 175, 255], [ 96, 255, 159, 255], [112, 255, 143, 255], [128, 255, 128, 255], [143, 255, 112, 255], [159, 255,  96, 255], [175, 255,  80, 255], [191, 255,  64, 255], [207, 255,  48, 255], [223, 255,  32, 255], [239, 255,  16, 255], [255, 255,   0, 255], [255, 239,   0, 255], [255, 223,   0, 255], [255, 207,   0, 255], [255, 191,   0, 255], [255, 175,   0, 255], [255, 159,   0, 255], [255, 143,   0, 255], [255, 128,   0, 255], [255, 112,   0, 255], [255,  96,   0, 255], [255,  80,   0, 255], [255,  64,   0, 255], [255,  48,   0, 255], [255,  32,   0, 255], [255,  16,   0, 255], [255,   0,   0, 255], [239,   0,   0, 255], [223,   0,   0, 255], [207,   0,   0, 255], [191,   0,   0, 255], [175,   0,   0, 255], [159,   0,   0, 255], [143,   0,   0, 255], [128,   0,   0, 255]], dtype=np.ubyte)

def lookup(cm, x):
    xp = (len(cm)-1) * x
    f = np.floor(xp)
    c = np.ceil(xp)
    a1 = xp - f
    a2 = c - xp
    # this should be better, but there are still some strange artefacts in the generated image
    #return np.add(cm[f], (xp-f)*(np.subtract(cm[c], cm[f])) ).astype(np.ubyte)
    return cm[round(xp)]

def main(args):
    logging.basicConfig(level=logging.INFO)
    
    if len(args.simulation) == 0:
        simfile = ''
    else:
        simfile = args.simulation[0]
    
    # load data
    loader = McCodeDataLoader(simfile=simfile)
    try:
        loader.load()
    except Exception as e:
        print('mcplot loader: ' + e.__str__())
        quit()
    node = loader.plot_graph
    
    # write data to an html file
    if type(node) is PNSingle:
        print("\n\n")
        data = node.getdata_idx(0)
        if type(data) is Data1D:
            
            x = data.xvals
            y = data.yvals
            yerr = data.y_err_vals
            text = fillout_template_1d(open('template_1d.html').read(), x, y, yerr, data.xlabel, data.ylabel, data.title)
            for l in text.splitlines():
                print(l)
            print("")
            
        elif type(data) is Data2D:
            
            vals = np.array(data.zvals)
            dims = np.shape(vals)
            img = np.zeros((dims[0], dims[1], 4))
            maxval = np.max(vals)
            cm = get_cm()
            for i in range(dims[0]):
                for j in range(dims[1]):
                    color = lookup(cm, vals[i,j]/maxval)
                    img[i,j,:] = color
            
            image = scipy.misc.toimage(img)
            output = io.BytesIO()
            image.save(output, format="png")
            contents = output.getvalue()
            output.close()
            
            encoded = str(base64.b64encode(contents)).lstrip('b')
            #print(encoded)

            xmin = data.xlimits[0]
            xmax = data.xlimits[1]
            ymin = data.xlimits[2]
            ymax = data.xlimits[3]

            text = fillout_template_2d(open('template_2d.html').read(), xmin, xmax, ymin, ymax, encoded, data.xlabel, data.ylabel, data.title)
            for l in text.splitlines():
                print(l)
            print("")
            
            #scipy.misc.imsave('outimage.png', img)
            
        else:
            print("can only plot 1D data right now")
    else:
        print("can only plot single data files")

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('simulation', nargs='*', help='file or directory to plot')
    
    args = parser.parse_args()
    
    main(args)

