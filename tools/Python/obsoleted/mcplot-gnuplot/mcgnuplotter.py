#!/usr/bin/env python3
#
#  Implements functionality for mcplot-gnuplot. Independent of ui.
#
import Gnuplot
# This is how we would control the default terminal:
# Gnuplot.GnuplotOpts.default_term = 'x11'
import string
import numpy
import re
import os

# base class for mcstas gnuplot objects
class McGnuplotObject():
    def __init__(self, key, data_struct, gp):
        """ set key, data for this instance """
        self.gp = gp
        self.key = key
        self.data = data_struct
        self.log_scale = False
        return
    
    def save(self, term):
        """ saves to file depending on term: [<gnuplot terminal>, <file extension>] """
        self.gp('set terminal %s' % term[0])
        out_file_noext = '%s' % os.path.splitext(self.data['fullpath'])[0]
        if self.log_scale: 
            out_file_noext = out_file_noext + '_log'
        self.gp('set output "%s.%s"' % (out_file_noext, term[1]))
        self.plot()
        self.gp('set term pop')
    
    def set_term(self, term):
        """ set gnuplot term on this instance """
        self.gp('set term %s' % term)

    def plot(self):
        """ give data member to plot_impl """
        self.plot_impl(self.gp, self.data)
    
    @staticmethod
    def plot_impl(gp, data):
        """ override and implement gnuplot commands """
        print('McGnuPlotObject: plot_impl not implemented')
        return
    
    def setLog(self, log_scale=True):
        self.setLog_impl(self.gp, log_scale)
        self.log_scale = log_scale
 
    @staticmethod
    def setLog_impl(gp, log_scale):
        """ override to implement set/unset log scale """
        return

# implements overview plotting 
# NOTE: overrides plot() and setLog() rather than the standard plot_impl() and setLog_impl()
class McGnuplotOverview(McGnuplotObject):
    def __init__(self, key, data_struct, gp, siblings):
        self.__setLogOnce = True
        self.__siblings = siblings
        return McGnuplotObject.__init__(self, key, data_struct, gp)

    def plot(self):
        """ plots all files to a singe window as multiplot (individually as array_1d or array_2d) """
        (nx, ny) = McGnuplotOverview.__calc_panel_size(len(self.__siblings))
        
        self.gp('set multiplot layout %d,%d rowsfirst' % (ny, nx))
        
        font_size = 6
        if max(nx,ny)>4:
            font_size = 2
        
        self.gp('set xtics font ",%s"' % font_size)
        self.gp('set ytics font ",%s"' % font_size)
        self.gp('set cbtics font ",%s"' % font_size)
        self.gp('set title font ",%s"' % font_size)
        self.gp('set xlabel font ",%s"' % font_size)
        self.gp('set ylabel font ",%s"' % font_size)
        
        for sib in self.__siblings:
            sib.setLog_impl(self.gp, self.log_scale)
            sib.plot_impl(self.gp, sib.data)
        
        self.gp('unset multiplot')

    @staticmethod
    def __calc_panel_size(num):
        """given the number of monitors to display as multiplot, return rows/cols"""
        from math import sqrt
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

# implements 2d plotting 
class McGnuplotPSD(McGnuplotObject):
    def __init__(self, key, data_struct, gp):
        return McGnuplotObject.__init__(self, key, data_struct, gp)
    
    @staticmethod
    def plot_impl(gp, data):
        gp("set view map")
        gp.title(data['title'])
        gp.xlabel(data['xlabel'])
        gp.ylabel(data['ylabel'])
        
        gp('set xtics format "%.1e"')
        gp('set ytics format "%.1e"')
        gp('set cbtics format "%.1e"')
            
        gp("splot '%s' matrix using 1:2:3 index 0 w image notitle" % data['fullpath'])

    @staticmethod
    def setLog_impl(gp, log_scale):
        if log_scale:
            gp('unset logscale y')
            gp('set logscale cb')
        else:
            gp('unset logscale y')
            gp('unset logscale cb')

# implements 1D plotting
class McGnuplot1D(McGnuplotObject):
    def __init__(self, key, data_struct, gp):
        return McGnuplotObject.__init__(self, key, data_struct, gp)

    @staticmethod
    def plot_impl(gp, data):
        plot_data = Gnuplot.Data(data['data'],
                    using='1:2:3',
                    with_='errorlines')
        
        gp('set linetype 1 lw 1 lc rgb "dark-spring-green" pointtype 2')
        
        gp('set xtics format "%.1e"')
        gp('set ytics format "%.1e"')
        
        gp.plot(plot_data,
                title=data['title'], 
                xlabel=data['xlabel'],
                ylabel=data['ylabel'])

    @staticmethod
    def setLog_impl(gp, log_scale):
        if log_scale:
            gp('set logscale y')
        else:
            gp('unset logscale y')

# mediator for all gnuplot objects associated to a .sim file (or of a single .dat file) - see mcplot.py, gnuplot version
class McGnuplotter():
    __overview_key = '< overview >'

    def __init__(self, input_file, noqt=False, log_scale=False):
        """ constructor - takes a .sim file or a .dat file name (for single vs. multiplot usage) NOTE: must be absolute file """
        
        # potentially set the gnuplot command that is supported by gnuplot 5+ on all platforms
        #Gnuplot._Gnuplot.gp.GnuplotOpts.gnuplot_command = r'gnuplot'
        
        # remember construction args
        self.arg_input_file = input_file
        self.arg_noqt = noqt
        self.arg_log_scale = log_scale
        
        gp_persist = 0
        if noqt:
            gp_persist = 1
        
        self.__gnuplot_objs = {}
        
        # check input file type & setup
        file_ext = os.path.splitext(input_file)[1]
        if file_ext == '.sim':
            data_file_lst = get_overview_files(input_file)
            siblings = []
            
            
            if 'mccode.dat' in map( lambda f: os.path.basename(f), data_file_lst):
                # mccode.dat / scan sweep mode
                print("mccode.dat found")
                print("Plotting mode not supported, exiting")
                exit()
                #dict = get_monitor(os.path.join(os.path.dirname(data_file_lst[0]), 'mccode.dat'))
                #for key in dict:
                #    print('key: %s, value: %s' % (key, dict[key]))
            else:
                # single scan step mode - load multiple monitor files
                for data_file in data_file_lst:
                    data_struct = get_monitor(data_file)
                    if re.search('array_2d.*', data_struct['type']):
                        gpo = McGnuplotPSD(data_struct['file'], data_struct, Gnuplot.Gnuplot(persist=gp_persist))
                        siblings.append(gpo)
                    else:
                        gpo = McGnuplot1D(data_struct['file'], data_struct, Gnuplot.Gnuplot(persist=gp_persist))
                        siblings.append(gpo)
                overview_data_struct = {}
                overview_data_struct['fullpath'] = input_file
                overview = McGnuplotOverview(McGnuplotter.__overview_key, overview_data_struct, Gnuplot.Gnuplot(persist=gp_persist), siblings)
                self.__gnuplot_objs[overview.key] = overview
                for gpo in siblings:
                    self.__gnuplot_objs[gpo.key] = gpo
            
        else:
            data_struct = get_monitor(input_file)
            if re.search('array_2d.*', data_struct['type']):
                gpo = McGnuplotPSD(data_struct['file'], data_struct, Gnuplot.Gnuplot(persist=gp_persist))
            else:
                gpo = McGnuplot1D(data_struct['file'], data_struct, Gnuplot.Gnuplot(persist=gp_persist))
            self.__gnuplot_objs[gpo.key] = gpo
        
        # execute set/unset logscale
        self.setLogscale(log_scale)

    def plot(self, key):
        """ plots .dat file corresponding to key """
        if key in self.__gnuplot_objs:
            self.__gnuplot_objs[key].plot()
        else:
            raise Exception('McGnuplotter.plot: no such key')
    
    def save(self, key, term=None, ext=None):
        """ like plot, but saves to file (default: png). 'term' and 'ext' are gnuplot term_lst and file extension strings """
        if key in self.__gnuplot_objs:
            term_lst = ['png', 'png']
            if term:
                term_lst[0] = term
            if ext:
                term_lst[1] = ext
            self.__gnuplot_objs[key].save(term_lst)
        else:
            raise Exception('McGnuplotter.save: no such key: %s' % key)
    
    def setTerm(self, key, term):
        """ sets the gnuplot terminal 'term' on the instance associated with 'key', if it exists """
        if key in self.__gnuplot_objs:
            self.__gnuplot_objs[key].set_term(term)
        else:
            raise Exception('McGnuplotter.save: no such key: %s' % key)
    
    def getDataKeys(self):
        """ returns an alpha-num sorted list of all McGnuplotObject instances installed at construction time by key """
        return sorted(self.__gnuplot_objs.keys(), key=lambda item: (int(item.partition(' ')[0][0]) if item[0].isdigit() else float('inf'), item))
    
    def setLogscale(self, log_scale):
        """ set log scale on all McGnuplotObject instances """
        for key in self.__gnuplot_objs:
            self.__gnuplot_objs[key].setLog(log_scale)
    
    def getSimilarInstance(self):
        """ returns a plotter instance with the same construction args """
        return McGnuplotter(self.arg_input_file, self.arg_noqt, self.arg_log_scale)
    
    def closeAll(self):
        """ elliminates all gp instances WARNING: you can not plot after calling this, new instances must be created first """
        for key in self.__gnuplot_objs:
            self.__gnuplot_objs[key].gp.close()

def get_overview_files(sim_file):
    """ returns a list of data files associated with the "mccode.sim" file (full paths) """
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

def get_monitor(mon_file):
    """ returns monitor .dat file info structured as a python dict """
    is_header = lambda line: line.startswith('#')
    header_lines = list(filter(is_header, open(mon_file).readlines()))
    
    file_struct_str ="{"
    for j in range(0, len(header_lines)):
        # Field name and data
        Line = header_lines[j]; Line = Line[2:len(Line)].strip()
        Line = Line.split(':')
        Field = Line[0]
        Value = ""
        Value = "".join(":".join(Line[1:len(Line)]).split("'"))
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
