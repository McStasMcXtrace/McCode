#!/usr/bin/env python
import argparse
import logging
import os
import sys
import numpy as np
import scipy.misc
import io
import base64
import json

sys.path.append(os.path.join(os.path.dirname(__file__), '..', '..'))

from mccodelib.mcplotloader import McCodeDataLoader, Data1D, Data2D
from mccodelib.plotgraph import PNSingle

def get_html(template_name, params):
    '''  '''
    text = open(template_name).read()
    text = text.replace("@PARAMS@", params)
    return text

def get_json_1d(x, y, yerr, xlabel, ylabel, title):
    '''  '''
    params = {}
    p = params
    p['w'] = 500
    p['h'] = 300
    p['x'] = x
    p['y'] = y
    p['yerr'] = yerr
    p['xlabel'] = xlabel
    p['ylabel'] = ylabel
    p['title'] = title
    
    return json.dumps(params, indent=4)

def get_json_2d(xmin, xmax, ymin, ymax, image_str, colorbar_img_str, xlabel, ylabel, title):
    '''  '''
    params = {}
    p = params
    
    p['w'] = 500
    p['h'] = 300
    
    p['xmin'] = xmin
    p['xmax'] = xmax
    p['ymin'] = ymin
    p['ymax'] = ymax
    
    p['img2dData'] = image_str
    p['imgColorbar'] = colorbar_img_str
    
    p['xlabel'] = xlabel
    p['ylabel'] = ylabel
    p['title'] = title
    
    return json.dumps(params, indent=4)

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
    return cm[np.int(np.round(xp))]

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
            params_str = get_json_1d(x, y, yerr, data.xlabel, data.ylabel, data.title)
            text = get_html('template_1d.html', params_str)
            for l in text.splitlines():
                print(l)
            print("")
            
        elif type(data) is Data2D:
            
            vals = np.array(data.zvals)
            dims = np.shape(vals)
            
            # create the 2d data as a png given our colormap
            img = np.zeros((dims[0], dims[1], 4))
            maxval = np.max(vals)
            cm = get_cm()
            for i in range(dims[0]):
                for j in range(dims[1]):
                    color = lookup(cm, vals[i,j]/maxval)
                    img[i,j,:] = color
            
            # encode png as base64 string
            image = scipy.misc.toimage(img)
            output = io.BytesIO()
            image.save(output, format="png")
            contents = output.getvalue()
            output.close()
            encoded_2d_data = str(base64.b64encode(contents)).lstrip('b').strip("\'")
            
            # crate colormap 1 x 256 image
            img = np.zeros((256, 1, 4))
            for i in range(256):
                color = lookup(cm, i/255)
                img[255-i, 0] = color
            
            # e3ncode cm image
            cb_img = scipy.misc.toimage(img)
            output = io.BytesIO()
            
            cb_img.save(output, format='png')
            contents = output.getvalue()
            output.close()
            encoded_cb = str(base64.b64encode(contents)).lstrip('b').strip("\'")

            xmin = data.xlimits[0]
            xmax = data.xlimits[1]
            ymin = data.xlimits[2]
            ymax = data.xlimits[3]

            json_str = get_json_2d(xmin, xmax, ymin, ymax, encoded_2d_data, encoded_cb, data.xlabel, data.ylabel, data.title)
            text = get_html('template_2d.html', json_str)
            
            for l in text.splitlines():
                print(l)
            print("")
            
        else:
            print("can only plot 1D data right now")
    else:
        print("can only plot single data files")

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('simulation', nargs='*', help='file or directory to plot')
    
    args = parser.parse_args()
    
    main(args)

