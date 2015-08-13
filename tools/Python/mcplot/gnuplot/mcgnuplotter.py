#!/usr/bin/env python
#
#  Implements functionality for mcplot-gnuplot.
#
import Gnuplot
import string
import numpy
import re
import os

class McGnuplotter():
    # static members
    __gp_persist = 0
    
    def __init__(self, input_file, noqt = False):
        """
        constructor - takes a .sim file or a .dat file name (for single vs. multiplot usage)
        """
        if noqt:
            McGnuplotter.__gp_persist = 1

        # dynamic members
        self.__sim_file = ''
        self.__data_file_dict = {}
        self.__gp_lst = []
        
        # check input file type & setup if sim file
        file_ext = os.path.splitext(input_file)[1]
        if file_ext == '.sim':
            self.__sim_file = input_file
            data_file_lst = McGnuplotter.__get_overview_files(self.__sim_file)
            for data_file in data_file_lst:
                data_struct = McGnuplotter.__get_monitor(data_file)
                self.__data_file_dict[data_struct['file']] = data_struct
        elif file_ext == '.dat':
            data_struct = McGnuplotter.__get_monitor(input_file)
            self.__data_file_dict[data_struct['file']] = data_struct
        else:
            raise Exception('McGnuPlotter: input file must be .sim or .dat')
    
    def closeAllGnuplots(self):
        for gp in self.__gp_lst:
            print('closing gnuplot window...')
            gp.close()
    
    def plot(self):
        """
        plots the file that was handed to the constructor -  .sim or .dat. The only way to plot overview data.
        """
        if os.path.splitext(os.path.basename(self.__sim_file))[1] == '.sim':
            file_struct_list = []
            for key in self.__data_file_dict:
                file_struct_list.append(self.__data_file_dict[key])
            gp = self.__plot_multiple(file_struct_list)
            self.__gp_lst.append(gp)
        else:
            for key in self.__data_file_dict:
                data_struct = self.__data_file_dict[key]
                gp = self.__plot_single(data_struct)
                self.__gp_lst.append(gp)
    
    def plot_single(self, data_key):
        """
        plots .dat file corresponding to data_key
        """
        data_struct = self.__data_file_dict[data_key]
        gp = self.__plot_single(data_struct)
        self.__gp_lst.append(gp)
    
    def get_data_keys(self):
        key_lst = []
        for key in self.__data_file_dict:
            key_lst.append(key)
        return key_lst

    @staticmethod
    def __plot_single(data_struct):
        """
        plots single file (array_1d or array_2d)
            data_file: single .dat file (absolute path)
        """
        array_2d = re.search('array_2d.*', data_struct['type'])
        if array_2d:
            gp = McGnuplotter.__plot_array_2d(data_struct['fullpath'], data_struct['title'], data_struct['xlabel'], data_struct['ylabel'])
        else:
            gp = McGnuplotter.__plot_array_1d_struct(data_struct)
        
        return gp
    
    @staticmethod
    def __plot_multiple(file_struct_lst):
        """
        plots all files to a singe window as multiplot (individually as array_1d or array_2d)
        """
        (nx, ny) = McGnuplotter.__calc_panel_size(len(file_struct_lst))
        
        gp = Gnuplot.Gnuplot(persist=McGnuplotter.__gp_persist)
        
        gp('set multiplot layout %d,%d rowsfirst' % (ny, nx))        
        for data_struct in file_struct_lst:
            array_2d = re.search('array_2d.*', data_struct['type'])
            if array_2d:
                McGnuplotter.__plot_array_2d(data_struct['fullpath'], data_struct['title'], data_struct['xlabel'], data_struct['ylabel'], gp)
            else:
                McGnuplotter.__plot_array_1d_struct(data_struct, gp)
        
        return gp

    @staticmethod
    def __plot_array_1d_struct(data_struct, gp=None):
        """
        plots a single 1d array with errorbars (compatible with multiplot if a GnuPlot is passed as gp)
        """
        if gp == None: 
            gp = Gnuplot.Gnuplot(persist=McGnuplotter.__gp_persist)
        
        data = Gnuplot.Data(data_struct['data'],
                            using='1:2:3',
                            with_='errorbars')
        gp.plot(data,
                title=data_struct['title'], 
                xlabel=data_struct['xlabel'],
                ylabel=data_struct['ylabel'])
        
        return gp
    
    @staticmethod
    def __plot_array_2d(data_file, title, xlabel, ylabel, gp=None):
        """
        plots a single 2d array (compatible with multiplot if a GnuPlot is passed as gp)
        """
        if gp == None:
            gp = Gnuplot.Gnuplot(persist=McGnuplotter.__gp_persist)
        
        gp("set view map")
        gp.title(title)
        gp.xlabel(xlabel)
        gp.ylabel(ylabel)
        gp("splot '%s' matrix using 1:2:3 index 0 w image notitle" % data_file)
        
        return gp
    
    @staticmethod
    def __calc_panel_size(num):
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

    @staticmethod
    def __get_overview_files(sim_file):
        """
        returns a list of data files associated with the "mccode.sim" file (full paths)
        """
        org_dir = os.getcwd()
        try:
            sim_file = os.path.abspath(sim_file)
            if os.path.dirname(sim_file) != '':
                os.chdir(os.path.dirname(sim_file))
                sim_file = os.path.basename(sim_file)
            
            monitor_files = filter(lambda line: (line.strip()).startswith('filename:'), open(sim_file).readlines())
            monitor_files = map(lambda f: os.path.abspath(f.rstrip('\n').split(':')[1].strip()), monitor_files)
        finally:
            os.chdir(org_dir)
        
        return monitor_files
    
    @staticmethod
    def __get_monitor(mon_file):
        """
        returns monitor .dat file info structured as a python dict
        """
        is_header = lambda line: line.startswith('#')
        header_lines = filter(is_header, open(mon_file).readlines())
        
        file_struct_str ="{"
        for j in range(0, len(header_lines)):
            # Field name and data
            Line = header_lines[j]; Line = Line[2:len(Line)].strip()
            Line = Line.split(':')
            Field = Line[0]
            Value = ""
            Value = string.join(string.join(Line[1:len(Line)], ':').split("'"), '')
            file_struct_str = file_struct_str + "'" + Field + "':'" + Value + "'"
            if j<len(header_lines)-1:
                file_struct_str = file_struct_str + ","
        file_struct_str = file_struct_str + "}"
        file_struct = eval(file_struct_str)
        
        # Add the data block:
        file_struct['data'] = numpy.loadtxt(mon_file)
        file_struct['fullpath'] = mon_file
        file_struct['file'] = os.path.basename(mon_file)
        print("Loading " + mon_file)
        return file_struct
