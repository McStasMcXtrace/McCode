#!/usr/bin/env python
#
# Implements Python interface for plotting McStas data output, with Matplotlib
#
#   This file is part of the McStas neutron ray-trace simulation package
#   Copyright (C) 1997-2004, All rights reserved
#   Risoe National Laborartory, Roskilde, Denmark
#   Institut Laue Langevin, Grenoble, France
#
#   This program is free software; you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation; version 2 of the License.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
#
#   You should have received a copy of the GNU General Public License
#   along with this program; if not, write to the Free Software
#   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

# requirement: 
#   python >= 2.4, python-matplotlib >= 0.91, numpy >= 1.1, 
#   freetype >= 1.4, libpng >= 1.1

# Usage: mcplot.py [options] <simfile | detectorfile | scanfile>
#
#  McCode plotting tool for simulation data.
#
#  Plots all monitor data from a simulation, scan or a single data file.
#  When using e.g. -f ps or -f pdf, the program writes a hardcopy file
#  (named as inputfile plus extension) and then exits.
#
#  Key shortcuts when plots are active:
#    x or q closes current figure
#    g      toggles grid on plots
#    o      toggles linear/log intensity on plots
#    c      toggles contour plots (2D)
#    mouse  opens plot in separate window (from overview)
#    p      dumps a Postscript graphic
#    d      dumps a PDF graphic
#    n      dumps a png graphic
#    j      dumps a jpg graphic
#  (some formats might not work on your platform)
#
# Options:
#  -h, --help            show this help message and exit
#  -f Format, --format=Format
#                        Export plot in format (e.g. ps) and exit
#  -l, --log             Plot results in log intensity scale
#  -c, --contour         Plot matrixes using contours instead of images


import sys
import os
import string
import matplotlib

FSlist      = list()
options     = 0
scan_window = None  # hold single scan window when circulating scan steps
scan_index  = 0     # hold current scan index (in scan_window)
scan_flag   = 0     # indicates if data set is a scan one. 
                    #   Set to 1 when showing overview. Set to 2 when starting to show separate scan steps
scan_length = 0     # length of scan data set
File        = 0     # hold initial filename/path to data set
exp_counter = 0     # Incremented on each hardcopy

"""string used for Usage (--help) and 'h' key stroke on plot"""
usage =         "usage: %s [options] <simfile | detectorfile | scanfile>\n\n" \
                % os.path.basename(sys.argv[0])
usage = usage + "  McCode plotting tool for simulation data (McCode format).\n\n"
usage = usage + "  Plots all monitor data from a simulation, scan or a single data file.\n"
usage = usage + "  When using e.g. -f ps or -f pdf, the program writes a hardcopy file\n"
usage = usage + "    (named as inputfile plus extension) and then exits.\n\n"
usage = usage + "  Key shortcuts when plots are active:\n"
usage = usage + "    x or q closes application\n"
usage = usage + "    g      toggles grid on plots\n"
usage = usage + "    o      toggles linear/log intensity on plots\n"
usage = usage + "    c      toggles contour plots (2D)\n"
usage = usage + "    mouse  opens plot in separate window (from overview)\n"
usage = usage + "    p      dumps a Postscript graphic\n"
usage = usage + "    d      dumps a PDF graphic\n"
usage = usage + "    n      dumps a PNG graphic\n"
usage = usage + "    j      dumps a JPEG graphic\n"
usage = usage + "  (some formats might not work on your platform)\n\n"

def display_single(FileStruct):
    """
    Plot a single 1D/2D axis with data, using matplotlib and optionally mplot3d.
      called from: click, display
      input:  FileStruct as obtained from read_monitor()
      output: FileStruct data structure
    """
    from pylab import errorbar,gca,xlim,ylim,ylabel,xlabel,title,linspace,pcolor,colorbar,log,contour
    from numpy import where
    type = FileStruct['type'].split('(')[0].strip()

    if type == 'array_1d':
        # 1D data set
        Xmin = eval(FileStruct['xlimits'].split()[0])
        Xmax = eval(FileStruct['xlimits'].split()[1])
        x=FileStruct['data'][:,0]
        y=FileStruct['data'][:,1]
        dy=FileStruct['data'][:,2]
        if options.log == True:
            # handle Log intensity
            invalid    = where(y <= 0)
            valid      = where(y > 0)
            min_valid  = min(y[valid])
            y[invalid] = min_valid/10
            dy=dy/y
            y=log(y)
            FileStruct['ylabel'] = "log(" + FileStruct['ylabel'] +")"
        h=errorbar(x,y,dy)
        FileStruct['axes']=gca()
        xlim(Xmin,Xmax)
        Title = FileStruct['component'] + ' [' + FileStruct['File'] + '], ' \
              + FileStruct['title']
        if len(FileStruct['values'])>0:
            Title = Title + '\n'
            Title = Title + "I=" + FileStruct['values'].split()[0]
            Title = Title + " E=" + FileStruct['values'].split()[1]
            Title = Title + " N=" + FileStruct['values'].split()[2]
    elif type == 'array_2d':
        # 2D data set
        mysize=FileStruct['data'].shape
        I=FileStruct['data'][0:mysize[0]/3,...]
        if options.log == True:
            # handle Log intensity
            invalid    = where(I <= 0)
            valid      = where(I > 0)
            min_valid  = min(I[valid])
            I[invalid] = min_valid/10
            I=log(I)
        mysize=I.shape
        Xmin = eval(FileStruct['xylimits'].split()[0])
        Xmax = eval(FileStruct['xylimits'].split()[1])
        Ymin = eval(FileStruct['xylimits'].split()[2])
        Ymax = eval(FileStruct['xylimits'].split()[3])
        x = linspace(Xmin,Xmax,mysize[1])
        y = linspace(Ymin,Ymax,mysize[0])
        
#        try:
#          # use mplot3d toolkit
#          from mpl_toolkits.mplot3d import Axes3D
#          from pylab import gcf
#          
#          ax = Axes3D(gcf())
#          if options.contour==True:
#            ax.contour(x,y,I)
#          else:
#            ax.plot_surface(x,y,I)
#        
#        except ImportError:
        # use default flat rendering
        ax = gca()
        if options.contour==True:
            h=contour(x,y,I)
        else:
            h=pcolor(x,y,I)
        
        FileStruct['axes']=gca()
        xlim(Xmin,Xmax)
        ylim(Ymin,Ymax)
        
        Title = FileStruct['component'] + ' [' + FileStruct['File'] + '], ' \
              + FileStruct['title']
        if options.log == True:
            Title = Title + " (log plot)"        
        Title = Title + "\nI=" + FileStruct['values'].split()[0]
        Title = Title + " E=" + FileStruct['values'].split()[1]
        Title = Title + " N=" + FileStruct['values'].split()[2]
        colorbar()
        
    xlabel(FileStruct['xlabel'],fontsize=FileStruct['FontSize']*1.5,fontweight='bold')
    ylabel(FileStruct['ylabel'],fontsize=FileStruct['FontSize']*1.5,fontweight='bold')
    title(Title, fontsize=FileStruct['FontSize']*1.5,fontweight='bold')
    return FileStruct
    # end display_single

def calc_panel_size(num):
    """
    Compute size of subplot to use
      called from: display
      input:  number of slots to prepare
      output: dimension of subplot
    """
    from pylab import sqrt
    Panels = ( [1,1], [2,1], [2,2], [3,2], [3,3], [4,3], [5,3], [4,4],
               [5,4], [6,4], [5,5], [6,5], [7,5], [6,6], [8,5], [7,6],
               [9,5], [8,6], [7,7], [9,6], [8,7], [9,7], [8,8], [10,7],
               [9,8], [11,7], [9,9], [11,8], [10,9], [12,8], [11,9],
               [10,10] )
    # Default size about sqrt($num) x sqrt($num).
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

    return nx,ny
    # end calc_panel_size

def read_monitor(this_File):
    """
    Read a monitor file (McCode format) using loadtxt module
      called from: display
      input: this_File file name as a string
      output: FileStruct data structure
    """
    from numpy import loadtxt

    # Read header
    isHeader = lambda line: line.startswith('#')
    Header = filter(isHeader, open(this_File).readlines())
    
    # Traverse header and define corresponding 'struct'
    str ="{"
    for j in range(0, len(Header)):
        # Field name and data
        Line = Header[j]; Line = Line[2:len(Line)].strip()
        Line = Line.split(':')
        Field = Line[0]
        Value = ""
        Value = string.join(string.join(Line[1:len(Line)], ':').split("'"), '')
        str = str + "'" + Field + "':'" + Value + "'"
        if j<len(Header)-1:
            str = str + ","
    str = str + "}"
    Filestruct = eval(str)
    # Add the data block:
    Filestruct['data']=loadtxt(this_File)
    Filestruct['fullpath'] = this_File
    print "Loading " + this_File
    
    return Filestruct
    # end read_monitor

def get_monitor(FS,j):
    """
    Extract one of the monitor in scan steps
      called from: display
      input:  FileStruct obtained from read_monitor(scan data file) 
              j          index of monitor column to extract
      output: FileStruct with selected monitor 'j'
    """
    # Ugly, hard-coded...
    data=FS['data'][:,(0,2*j+1,2*j+2)]
    vars=FS['variables'].split()   
    FSsingle={'xlimits':FS['xlimits'],'data':data,'component':vars[j+1],'values':'',
              'type':'array_1d(100)',
              'xlabel':FS['xlabel'],'ylabel':FS['ylabel'],'File':'Scan','title':'','FontSize':6}
    return FSsingle
    # end get_monitor

def click(event):
    """
    Handle mouse click in the main matplotlib overview window, opens single monitor window
      called from: display
      input:  event structure
      output: None
    """
    from pylab import get_current_fig_manager,get,gcf,figure,clf,connect,show
    tb = get_current_fig_manager().toolbar
    if event.button==1 and event.inaxes and tb.mode == '':
        g = event.inaxes
        # Determine number of clicked axis
        ax = get(gcf(),'axes')
        jused = 0
        for j in range(0, len(FSlist)):
            FS = FSlist[j]
            if g==FS['axes']:
                h=figure()
                clf()
                FS=display_single(FS)
                connect('key_press_event', keypress)
                jused = j
        FSlist[jused]['axes']=g
        show()
    # end click

def keypress(event):
    """
    Handle key shortcut in a window
      called from: click, display
      input:  event structure
      output: None
    """
    from pylab import close,gcf,figure,gca,text,title,show
    event.key = event.key.lower()
    if event.key == 'q':
        close('all')
    elif event.key == 'x':
        close('all')
    elif event.key == 'p':
        dumpfile('ps')
    elif event.key == 'd':
        dumpfile('pdf')
    elif event.key == 'n':
        dumpfile('png')
    elif event.key == 'j':
        dumpfile('jpg')
    elif event.key == 'o' or event.key == 'l':
        'toggle lin/log'
        from pylab import get_current_fig_manager
        tb = get_current_fig_manager().toolbar
        options.log = not options.log;
        if options.log == True:
          tb.set_message('Log intensity scale selected for next plots');
        else:
          tb.set_message('Linear intensity scale selected for next plots');
    elif event.key == 'c':
        'toggle contour/normal'
        from pylab import get_current_fig_manager
        tb = get_current_fig_manager().toolbar
        options.contour = not options.contour;
        if options.contour == True:
          tb.set_message('Contour mode selected for next 2D plots');
        else:
          tb.set_message('Normal mode selected for next 2D plots');
    elif event.key == 'h':
        'usage/help'
        h=figure()
        text(0.05,0.05,usage)
        title(os.path.basename(sys.argv[0]) + ": plotting tool for McCode data set",fontweight='bold')
        show()
    elif event.key == 'right' or event.key == 'pagedown':
        'show next scan step/monitor'
        display_scanstep(File, +1)
    elif event.key == 'left' or event.key == 'backspace' or event.key == 'pageup':
        'show previous scan step/monitor'
        display_scanstep(File, -1)
    # end keypress
    
def display_scanstep(this_File, relative_index):
    """
    When displaying a scan data set, displays next/previous scan step in a separate window
      called from: keypress
      input:  this_File file name of scan data set/directory
              relative_index +1 or -1 to indicate forward/backward circulate in scan steps
      output: None
    """
    global scan_index,scan_window,scan_flag

    # are we managing a scan step overview ?
    if scan_flag == 0 or scan_length <= 1:
      return
    
    from pylab import gcf,close,figure
    
    # open or re-open scan_window
    if scan_window is not None:
        close(scan_window)
        
    scan_window=figure()
    
    if scan_flag == 1:
      index = 0
      scan_flag = 2
    else:
      index = scan_index + relative_index
    
    # check scan boundaries
    if index >= scan_length:
       index = 0
    elif index < 0:
      index = scan_length-1

    scan_index = index
    print "Loading " + os.path.join(this_File, "%i" % index)
    display(os.path.join(this_File, "%i" % index), "/%i" % index)
    # end display_scanstep

def dumpfile(format):
    """
    Save current fig to hardcopy. 
      called from: keypress
      input:  format of hardcopy to generate, e.g. pdf, ps, png, ...
      output: None
    """
    global exp_counter
    from pylab import savefig
    Filename = "mcplot_" + str(exp_counter) + "." + format
    exp_counter = exp_counter + 1
    savefig(Filename)
    print "Saved " + Filename
    # end dumpfile

def display(this_File, comment):
    """
    Display data set from File: overview or scan data set. Installs button and keyboard events.
    Only returns after show().
      called from: main
      input:  this_File file name to show as a string
      output: None
    """
    from pylab import connect,subplot,gca,savefig,show,gcf
    global scan_flag,scan_length
    
    if this_File =="":
        # No filename given, assume mcstas.sim in current dir
        this_File = "mcstas.sim"

    # FIX: make the path to this_File absolute to always get a non-empty dirname.
    this_File = os.path.abspath(this_File)
    
    if os.path.isdir(this_File)==1:
        # dirname given, assume mcstas.sim in that dir.
        this_File = os.path.join(this_File,'mcstas.sim')

    if os.path.dirname(this_File) != '':
        pwd = os.getcwd()
        os.chdir(os.path.dirname(this_File))
        this_File = os.path.basename(this_File)

    if os.path.isfile(this_File)==0:
        print os.path.basename(sys.argv[0]) +": Dataset " + File + " does not exist!"
        exit()
    
    isBegin = lambda line: line.startswith('begin')
    isCompFilename = lambda line: line.startswith('    filename:')
    # First, determine if this is single or overview plot...
    SimFile = filter(isBegin, open(this_File).readlines())
    Datfile = 0;
    if SimFile == []:
        FS = read_monitor(this_File)
        type = FS['type'].split('(')[0].strip()
        if type!='multiarray_1d':
            FS['FontSize']=8
            FS=display_single(FS)
            if options.Format!=None: 
                savefig(this_File + "." + options.Format)
                print "Saved " + File + "." + options.Format
            show()
            exit()
            Datfile = 1

    # Get filenames from the sim file
    MonFiles = filter(isCompFilename, open(this_File).readlines())
    L = len(MonFiles)
    
    # create main window
    gcf()
    # Scan or overview?
    if L==0:
        """Scan view"""
        if Datfile==0:
            isFilename = lambda line: line.startswith('filename')
            Scanfile = filter(isFilename, open(this_File).readlines()); 
            Scanfile = Scanfile[0].split(': ')
            Scanfile = os.path.join(os.path.dirname(this_File),Scanfile[1].strip())
            # Proceed to load scan datafile
            FS = read_monitor(Scanfile)
            L=(len(FS['variables'].split())-1)/2
            dims = calc_panel_size(L)
            scan_flag = 1
            for j in range(0, L):
                FSsingle = get_monitor(FS,j)
                subplot(dims[1],dims[0],j+1)
#                try:
#                    # use mplot3d toolkit
#                    from mpl_toolkits.mplot3d import Axes3D
#                    from pylab import gcf
#                    ax = Axes3D(gcf())
#                except ImportError:
                ax=gca()
                 
                FSlist[len(FSlist):] = [FSsingle]
                FSlist[j]=display_single(FSsingle)
                scan_length = FSsingle['data'].shape[0]
            
    else:
        """Overview or single monitor"""
        dims = calc_panel_size(L)
        for j in range(0, L):
            subplot(dims[1],dims[0],j+1)
#            try:
#                # use mplot3d toolkit
#                from mpl_toolkits.mplot3d import Axes3D
#                from pylab import gcf
#                ax = Axes3D(gcf())
#            except ImportError:
            ax=gca()
            MonFile = MonFiles[j].split(':'); MonFile = MonFile[1].strip()
            FS=read_monitor(MonFile)
            FS['FontSize']=6
            FSlist[len(FSlist):] = [FS]
            FSlist[j]=display_single(FS)
    for xlabel_i in ax.get_xticklabels():
        xlabel_i.set_fontsize(6)
    for ylabel_i in ax.get_yticklabels():
        ylabel_i.set_fontsize(6)   
            
    if options.Format!=None:
        savefig(this_File + "." + options.Format)
        print "Saved " + File + "." + options.Format
    else:
        from pylab import get_current_fig_manager
        tb = get_current_fig_manager().toolbar
        tb.set_message('File: ' + File + comment + ' mcplot: Keys: h=help [goc|x|pdnj] ' + File + comment);
        
    # install handlers for keyboard and mouse events

    connect('button_press_event',click)
    connect('key_press_event', keypress)
    
    os.chdir(pwd)
    show()
    # end display

if __name__ == "__main__":
    """
    Main mcplot entry point. 
    Parse options, Read monitors, select overview/scan/single monitor
    """
    from optparse import OptionParser
    parser = OptionParser(usage=usage)
    parser.add_option("-f", "--format", dest="Format",
                      help="Export plot in format (e.g. ps) and exit", 
                      metavar="Format")
    parser.add_option("-l", "--log",
                      help="Plot results in log intensity scale", 
                      action="store_true",dest="log")
    parser.add_option("-c", "--contour",
                      help="Plot matrixes using contours instead of images", 
                      action="store_true",dest="contour")
    
    (options, args) = parser.parse_args()
    
    File=string.join(args)

    if options.Format!=None:
        matplotlib.use(options.Format)
        
    display(File, "")
    # End of __main__
    
