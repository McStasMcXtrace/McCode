#!/usr/bin/env python
import sys;
import os;

import matplotlib
if sys.platform == 'darwin':
    matplotlib.use('MacOSX');

from numpy import loadtxt
from pylab import *

def mcplot_single(File):
    # Pattern matches
    isHeader = lambda line: line.startswith('#');
    isYlabel = lambda line: line.startswith('# ylabel');
    isXlabel = lambda line: line.startswith('# xlabel');
    isComp = lambda line: line.startswith('# component');
    isFilename = lambda line: line.startswith('# filename');
    isTitle = lambda line: line.startswith('# title');
    isValues = lambda line: line.startswith('# values');
    isVariables = lambda line: line.startswith('# variables');
    isStatistics = lambda line: line.startswith('# statistics');
    isXlimits = lambda line: line.startswith('# xlimits');
    isXYlimits = lambda line: line.startswith('# xylimits');
    isType = lambda line: line.startswith('# type');
    # Read the header and determine type of dataset
    Header = filter(isHeader, open(File).readlines())
    Type = filter(isType,Header); Type = Type[0].split(': '); Type = Type[1].strip(); Type = Type.split('('); Type = Type[0];

    Xlabel = filter(isXlabel,Header); Xlabel = Xlabel[0].split(': '); Xlabel = Xlabel[1].strip();

    if Type == 'multiarray_1d':
        Variables = filter(isVariables,Header); Variables = Variables[0].split(': '); Variables = Variables[1].split();
        L=len(Variables);
        dims = calc_panel_size((L-1)/2);    
        data=loadtxt(File);
        x=data[:,0];
        for j in range(1, 1+(L-1)/2):
            subplot(dims[0],dims[1],j);
            I=data[:,2*j-1];
            E=data[:,2*j];
            errorbar(x,I,E);
            xlabel(Variables[0], fontsize=4);
            ylabel("Intensity " + Variables[2*j-1], fontsize=4);
            title(Variables[2*j-1] + " " + File + "\n Scan of " + Variables[0], fontsize=4)
    else:
        # Common stuff for 1 and 2 d:
        Ylabel = filter(isYlabel,Header); Ylabel = Ylabel[0].split(': '); Ylabel = Ylabel[1].strip();
        Component = filter(isComp,Header); Component = Component[0].split(': '); Component = Component[1].strip();
        Filename = filter(isFilename,Header); Filename = Filename[0].split(': '); Filename = Filename[1].strip();
        Title = filter(isTitle,Header); Title = Title[0].split(': '); Title = Title[1].strip();
        Values = filter(isValues,Header); Values = Values[0].split(': '); Values = Values[1].split();
        Statistics = filter(isStatistics,Header); Statistics = Statistics[0].split(': '); Statistics = Statistics[1].strip();
        
        data=loadtxt(File);
    
    

        if Type == 'array_1d':
            Xlimits = filter(isXlimits,Header); Xlimits = Xlimits[0].split(': '); Xlimits = Xlimits[1].split(); 
            Xmin = eval(Xlimits[0]); Xmax = eval(Xlimits[1]);
        
            x=data[:,0];
            y=data[:,1];
            dy=data[:,2];
            errorbar(x,y,dy);
            xlim(Xmin,Xmax);
        elif Type == 'array_2d':
            mysize=data.shape;
            I=data[0:mysize[0]/3,...];
            mysize=I.shape;
            XYlimits = filter(isXYlimits,Header); XYlimits = XYlimits[0].split(': '); XYlimits = XYlimits[1].split(); 
            Xmin = eval(XYlimits[0]); Xmax = eval(XYlimits[1]);
            Ymin = eval(XYlimits[2]); Ymax = eval(XYlimits[3]);
            x = linspace(Xmin,Xmax,mysize[1]);
            y = linspace(Ymin,Ymax,mysize[0]);       
            pcolor(x,y,I);
            ylim(Ymin,Ymax);
            
        xlim(Xmin,Xmax);
        xlabel(Xlabel);
        ylabel(Ylabel);
    
        Title = Component + ' [' + Filename + '], ' + Title + '\n' + 'I=' + Values[0] + ' E=' + Values[1] + ' N=' + Values[2] + '; ' + Statistics;
    
        title(Title, fontsize=8)
    return 0;

def calc_panel_size(num):
    Panels = ( [1,1], [2,1], [2,2], [3,2], [3,3], [4,3], [5,3], [4,4],
               [5,4], [6,4], [5,5], [6,5], [7,5], [6,6], [8,5], [7,6],
               [9,5], [8,6], [7,7], [9,6], [8,7], [9,7], [8,8], [10,7],
               [9,8], [11,7], [9,9], [11,8], [10,9], [12,8], [11,9],
               [10,10] );
    # Default size about sqrt($num) x sqrt($num).
    ny = int(sqrt(num));
    nx = int(num/ny);
    
    if nx*ny < num:
        nx = nx+1;
    
    fit = nx*ny - num;
    
    for j in range(0, 31):
        panel = Panels[j];
        d = panel[0]*panel[1] - num;
        if d > 0:
            if d < fit:
                fit = d; nx = panel[0]; ny = panel[1];

    return nx,ny;


File = sys.argv[1];

isBegin = lambda line: line.startswith('begin');
isCompFilename = lambda line: line.startswith('    filename:');
# First, determine if this is single or overview plot...
SimFile = filter(isBegin, open(File).readlines());

if SimFile == []:
    mcplot_single(File);
    show();
else:
    # Get filenames from the sim file
    MonFiles = filter(isCompFilename, open(File).readlines());
    L = len(MonFiles);
    
    # Scan or oveview?
    if L==0:
        isFilename = lambda line: line.startswith('filename');
        Scanfile = filter(isFilename, open(File).readlines()); Scanfile = Scanfile[0].split(': '); 
        Scanfile = os.path.join(os.path.dirname(File),Scanfile[1].strip()); 
        # Proceed to scan datafile
        mcplot_single(Scanfile);

    else:
        dims = calc_panel_size(L);

        for j in range(0, L):
            subplot(dims[0],dims[1],j+1);
            MonFile = MonFiles[j].split(':'); MonFile = MonFile[1].strip();
            mcplot_single(MonFile);
    show();
    


