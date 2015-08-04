#!/usr/bin/env python
#
# Implements a Python interface for plotting McStas data output with GnuPlot.
#
import Gnuplot
import argparse
import logging
import string
import numpy
import re
import os
import string

def plot_array_1d_struct(data_struct, gp=None):
    """
    plots a single 1d array with errorbars (compatible with multiplot if a GnuPlot is passed as gp)
    """
    if gp == None: 
        gp = Gnuplot.Gnuplot(persist=1)
    data = Gnuplot.Data(data_struct['data'],
                        using='1:2:3',
                        with_='errorbars')
    gp.plot(data,
            title=data_struct['title'], 
            xlabel=data_struct['xlabel'],
            ylabel=data_struct['ylabel'])

def plot_array_2d(data_file, title, xlabel, ylabel, gp=None):
    """
    plots a single 2d array (compatible with multiplot if a GnuPlot is passed as gp)
    """
    if gp == None:
        gp = Gnuplot.Gnuplot(persist=1)
    gp("set view map")
    gp.title(title)
    gp.xlabel(xlabel)
    gp.ylabel(ylabel)
    gp("splot '%s' matrix using 1:2:3 index 0 w image notitle" % data_file)

def calc_panel_size(num):
    """
    given the number of monitors to display as multiplot, return rows/cols
    """
    from pylab import sqrt
    Panels = ( [1,1], [2,1], [2,2], [3,2], [3,3], [4,3], [5,3], [4,4],
               [5,4], [6,4], [5,5], [6,5], [7,5], [6,6], [8,5], [7,6],
               [9,5], [8,6], [7,7], [9,6], [8,7], [9,7], [8,8], [10,7],
               [9,8], [11,7], [9,9], [11,8], [10,9], [12,8], [11,9],
               [10,10] )
    # default size about sqrt($num) x sqrt($num).
    ny = int(sqrt(num))
    nx = int(num/ny)
    if nx*ny < num:
        nx = nx+1;
    
    fit = nx*ny - num

    for j in range(0, 31):
        panel = Panels[j]
        d = panel[0]*panel[1] - num
        if d > 0:
            if d < fit:
                fit = d; nx = panel[0]; ny = panel[1]
    
    return nx, ny

def get_overview(sim_file):
    """
    returns a list of data files associated with the "mccode.sim" file (full paths)
    """
    sim_file = os.path.abspath(sim_file)
    if os.path.dirname(sim_file) != '':
        os.chdir(os.path.dirname(sim_file))
        sim_file = os.path.basename(sim_file)
    
    monitor_files = filter(lambda line: (line.strip()).startswith('filename:'), open(sim_file).readlines())
    monitor_files = map(lambda f: os.path.abspath(f.rstrip('\n').split(':')[1].strip()), monitor_files)
    
    return monitor_files
    
def get_monitor(mon_file):
    """
    returns monitor .dat file info structured as a python dict
    """
    is_header = lambda line: line.startswith('#')
    header_lines = filter(is_header, open(mon_file).readlines())
    
    str ="{"
    for j in range(0, len(header_lines)):
        # Field name and data
        Line = header_lines[j]; Line = Line[2:len(Line)].strip()
        Line = Line.split(':')
        Field = Line[0]
        Value = ""
        Value = string.join(string.join(Line[1:len(Line)], ':').split("'"), '')
        str = str + "'" + Field + "':'" + Value + "'"
        if j<len(header_lines)-1:
            str = str + ","
    str = str + "}"
    file_struct = eval(str)
    
    # Add the data block:
    file_struct['data'] = numpy.loadtxt(mon_file)
    file_struct['fullpath'] = mon_file
    file_struct['File'] = mon_file
    print("Loading " + mon_file)
    return file_struct

def plot_single(data_file):
    """
    plots a single file to a new window (array_1d or array_2d)
        data_file: single .dat file (absolute path)
    """
    data_struct = get_monitor(data_file)
    type = data_struct['type']
    array_2d = re.search('array_2d.*', type)
    if array_2d:
        plot_array_2d(data_file, data_struct['title'], data_struct['xlabel'], data_struct['ylabel'])
    else:
        plot_array_1d_struct(data_struct)

def plot_multiple(data_files, nx, ny):
    """
    plots all files to a singe window as multiplot (individually as array_1d or array_2d)
    """
    gp = Gnuplot.Gnuplot(persist=1)
    cmd1 = 'set multiplot layout %d,%d columnsfirst' % (nx, ny)
    logging.debug(cmd1)
    gp('set multiplot layout %d,%d rowsfirst' % (ny, nx))
    
    for f in data_files:
        data_struct = get_monitor(f)
        type = data_struct['type']
        array_2d = re.search('array_2d.*', type)
        if array_2d:
            plot_array_2d(f, data_struct['title'], data_struct['xlabel'], data_struct['ylabel'], gp)
        else:
            plot_array_1d_struct(data_struct, gp)
    
def main(args):
    logging.basicConfig(level=logging.INFO)
     
    # 0 - handle sim file
    sim_file = "mccode.sim"
    if args.simulation:
        simulation = args.simulation[0]
        logging.debug('simulation file/dir: %s', simulation)
        
        if os.path.isdir(simulation):
            simulation = os.path.join(simulation, 'mccode.sim')
            
        if os.path.splitext(simulation)[1] == '.sim':
            sim_file = simulation
        else:    
            # 1 - plot single monitor data
            print('Plot single monitor')
            plot_single(simulation)
            exit()
            
    # check sim file
    if not os.path.isfile(sim_file):
        print('Sim file not found')
        exit()
    else:
        print('Using sim file: %s' % os.path.abspath(sim_file))
        
    # 2 - sim file: get overview
    data_files = get_overview(sim_file)    
    
    # 3 - multiplot
    (nx, ny) = calc_panel_size(len(data_files))
    plot_multiple(data_files, nx, ny)
    
    
if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('simulation', nargs='*', help='simulation (.sim) or monitor (.dat) file, or directory')
    args = parser.parse_args()

    main(args)
