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
import subprocess

sys.path.append(os.path.join(os.path.dirname(__file__), '..', '..'))

from mccodelib.mcplotloader import McCodeDataLoader, Data1D, Data2D
from mccodelib.plotgraph import PNSingle, PNMultiple
from mccodelib import mccode_config
from shutil import copyfile

WIDTH = 700
HEIGHT = 480

def file_base_name(file_name):
    if '.' in file_name:
        separator_index = file_name.index('.')
        base_name = file_name[:separator_index]
        return base_name
    else:
        return file_name

def path_base_name(path):
    file_name = os.path.basename(path)
    return file_base_name(file_name)

def get_html_single(template_name, params, simfile):
    '''  '''
    text = open(os.path.join(os.path.dirname(__file__),template_name)).read()
    text = text.replace("@PARAMS@", params)
    text = text.replace("@DATAFILE@", simfile)
    return text

def get_json_1d(x, y, yerr, xlabel, ylabel, title):
    '''  '''
    params = {}
    p = params
    p['w'] = WIDTH
    p['h'] = HEIGHT
    p['x'] = x
    p['y'] = y
    p['yerr'] = yerr
    p['xlabel'] = xlabel
    p['ylabel'] = ylabel
    p['title'] = title

    return json.dumps(params, indent=4)

def get_json_2d(xmin, xmax, ymin, ymax, image_str, colorbar_img_str, cb_min, cb_max, xlabel, ylabel, title):
    '''  '''
    params = {}
    p = params

    p['w'] = WIDTH
    p['h'] = HEIGHT

    p['xmin'] = xmin
    p['xmax'] = xmax
    p['ymin'] = ymin
    p['ymax'] = ymax

    p['img2dData'] = image_str
    p['imgColorbar'] = colorbar_img_str

    p['cbMin'] = cb_min
    p['cbMax'] = cb_max

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

def browse(html_filepath):
    # open a web-browser in a cross-platform way
    try:
        subprocess.Popen('%s %s' % (mccode_config.configuration['BROWSER'], html_filepath), shell=True)
    except Exception as e:
        raise Exception('Os-specific open browser: %s' % e.__str__())

def get_params_str_1D(data):
    x = data.xvals
    y = data.yvals
    yerr = data.y_err_vals
    try:
        title = '%s [%s] %s\nI = %s Err = %s N = %s\n %s' % (data.component, data.filename, data.title, data.values[0], data.values[1], data.values[2], data.statistics)
    except:
        title = '%s\n[%s]' % (data.component, data.filename)
    return get_json_1d(x, y, yerr, data.xlabel, data.ylabel, title)

def get_params_str_2D(data):
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

    cb_min = np.min(vals)
    cb_max = np.max(vals)

    # generate the title
    verbose = True
    try:
        title = '%s\nI = %s' % (data.component, data.values[0])
        if verbose:
            title = '%s [%s] %s\nI = %s Err = %s N = %s\n %s' % (data.component, data.filename, data.title, data.values[0], data.values[1], data.values[2], data.statistics)
    except:
        title = '%s\n[%s]' % (data.component, data.filename)

    return get_json_2d(xmin, xmax, ymin, ymax, encoded_2d_data, encoded_cb, cb_min, cb_max, data.xlabel, data.ylabel, title)


def main(args):
    logging.basicConfig(level=logging.INFO)

    if len(args.simulation) == 0:
        simfile = ''
    else:
        simfile = args.simulation[0]

    # Define directory and basename
    simdir = os.path.dirname(simfile)
    simbase = path_base_name(simfile)

    # Store system stdout for later restoring
    f = sys.stdout
    html_filepath=''

    # If simfile includes a directory name, dump our js code there
    if not(args.nohtml):
        if os.path.isdir(simdir):
            copyfile(os.path.join(os.path.dirname(__file__),'d3.v4.min.js'), os.path.join(simdir,'d3.v4.min.js'))
            copyfile(os.path.join(os.path.dirname(__file__),'plotfuncs.js'), os.path.join(simdir,'plotfuncs.js'))
            if not args.index is None:
                html_filepath=os.path.join(simdir,simbase + '_' + str(args.index) + '.html')
            else:
                html_filepath=os.path.join(simdir,simbase + '.html')
            f = open(html_filepath, 'w')

    # load data
    loader = McCodeDataLoader(simfile=simfile)
    try:
        loader.load()
    except Exception as e:
        print('mcplot loader: ' + e.__str__())
        quit()
    node = loader.plot_graph

    data = None

    # write data to an html file
    if type(node) is PNMultiple:
        if args.index is None:
            data = node.getdata_lst()
        else:
            data = node.getdata_idx(args.index)

    elif type(node) is PNSingle:
        f.write("\n\n")
        data = node.getdata_idx(0)

    # handle multiplot
    if type(data) is list:
        data_lst = data
        params_str_lst = []
        for d in data_lst:
            if type(d) is Data1D:
                params_str_lst.append(get_params_str_1D(d))
            elif type(d) is Data2D:
                params_str_lst.append(get_params_str_2D(d))

        # create dynamic lines
        divs_txt = ""
        create_txt = ""
        params = ""
        id = ""
        i = 0
        for d in data_lst:
            i = i + 1
            id = "plt_%d" % i
            if type(d) is Data1D:
                params = get_params_str_1D(d)
            elif type(d) is Data2D:
                params = get_params_str_2D(d)

            divs_txt = divs_txt + "\n" + '<div style="display:inline" id="%s"></div>' % id
            create_txt = create_txt + "\n" + '  new Plot1D(%s, d3.select("#%s").append("svg"));' % (params, id)

        # assemble html
        text = '''<!DOCTYPE html>
<head>
  <script src="d3.v4.min.js"></script>
  <script src="plotfuncs.js"></script>
</head>
<body>
        ''' + divs_txt + "\n\n<script>\n" + create_txt + "\n</script>\n</body>\n</html>\n"

    # handle single 1D data
    elif type(data) is Data1D:
        text = get_html_single('template_1d.html', get_params_str_1D(data), os.path.basename(data.filename))

    # handle single 2D data
    elif type(data) is Data2D:
        text = get_html_single('template_2d.html', get_params_str_2D(data), os.path.basename(data.filename))

    else:
        print("can not plot file " + simfile + " of data type " + type(data))

    # write file then browse
    #for l in text.splitlines():
    #    f.write(l + "\n")
    f.write(text)
    f.write("")
    if not(args.nohtml):
        f.close()
    # exit if nobrowse flag has been set
    if args.nobrowse:
        return
    else:
        browse(html_filepath)


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('simulation', nargs='*', help='file or directory to plot')
    parser.add_argument('--index', dest='index', type=int, help='Pick nth column from scan file')
    parser.add_argument('--nobrowse', action='store_true', help='do not open a webbrowser viewer')
    parser.add_argument('--nohtml', action='store_true', help='do not save html file')

    args = parser.parse_args()

    main(args)
