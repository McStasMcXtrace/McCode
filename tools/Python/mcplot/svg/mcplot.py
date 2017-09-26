#!/usr/bin/env python
import argparse
import logging
import os
import sys

sys.path.append(os.path.join(os.path.dirname(__file__), '..', '..'))

from mccodelib.mcplotloader import McCodeDataLoader, Data1D
from mccodelib.plotgraph import PNSingle

def fillout_template(template, x, y, yerr, xlabel, ylabel, title):
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
            text = fillout_template(open('template.html').read(), x, y, yerr, data.xlabel, data.ylabel, data.title)
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

