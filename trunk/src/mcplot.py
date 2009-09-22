import sys;

import matplotlib
if sys.platform == 'darwin':
    matplotlib.use('MacOSX');

from numpy import loadtxt
from pylab import *



File = sys.argv[1];

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

# Common stuff for 1 and 2 d:
Xlabel = filter(isXlabel,Header); Xlabel = Xlabel[0].split(': '); Xlabel = Xlabel[1].strip();
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
show();
