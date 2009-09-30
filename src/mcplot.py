#!/usr/bin/env python
import sys;
import os;
import string;

def mcplot_single(FileStruct):
    type = FileStruct['type'].split('(')[0].strip();

    if type == 'multiarray_1d':
        # Only a single panel is plotted here, Struct has field member 'selected' with integer value from outer function
        Variables = FileStruct['variables'].split();
        j = FileStruct['selected'];
        print str(j) + " is selected"
        x=FileStruct['data'][:,0];
        I=FileStruct['data'][:,2*j+1];
        E=FileStruct['data'][:,2*j+2];
        h=errorbar(x,I,E);
        FileStruct['axes']=gca()
        #h.RawData=FileStruct;
        Xmin = eval(FileStruct['xlimits'].split()[0]);
        Xmax = eval(FileStruct['xlimits'].split()[1]);        
        xlim(Xmin,Xmax);
        xlabel(Variables[0], fontsize=FileStruct['FontSize']);
        ylabel("Intensity " + Variables[2*j-1], fontsize=FileStruct['FontSize']);
        Title = Variables[2*j-1] + " " + File + "\n Scan of " + Variables[0]
    elif type == 'array_1d':
        Xmin = eval(FileStruct['xlimits'].split()[0]);
        Xmax = eval(FileStruct['xlimits'].split()[1]);        
        x=FileStruct['data'][:,0];
        y=FileStruct['data'][:,1];
        dy=FileStruct['data'][:,2];
        h=errorbar(x,y,dy);
        FileStruct['axes']=gca()
        #h.RawData=FileStruct;
        xlim(Xmin,Xmax);
        xlabel(FileStruct['xlabel'],fontsize=FileStruct['FontSize']);
        ylabel(FileStruct['ylabel'],fontsize=FileStruct['FontSize']);
        Title = FileStruct['component'] + ' [' + FileStruct['File'] + '], ' + FileStruct['title'];
        if len(FileStruct['values'])>0:
            Title = Title + '\n';
            Title = Title + "I=" + FileStruct['values'].split()[0];
            Title = Title + " E=" + FileStruct['values'].split()[1];
            Title = Title + " N=" + FileStruct['values'].split()[2];
    elif type == 'array_2d':
        mysize=FileStruct['data'].shape;
        I=FileStruct['data'][0:mysize[0]/3,...];
        mysize=I.shape;
        Xmin = eval(FileStruct['xylimits'].split()[0]);
        Xmax = eval(FileStruct['xylimits'].split()[1]);
        Ymin = eval(FileStruct['xylimits'].split()[2]);
        Ymax = eval(FileStruct['xylimits'].split()[3]);
        x = linspace(Xmin,Xmax,mysize[1]);
        y = linspace(Ymin,Ymax,mysize[0]);       
        h=pcolor(x,y,I);
        FileStruct['axes']=gca()
        #h.RawData=FileStruct;
        xlim(Xmin,Xmax);
        ylim(Ymin,Ymax);
        xlabel(FileStruct['xlabel'],fontsize=FileStruct['FontSize']);
        ylabel(FileStruct['ylabel'],fontsize=FileStruct['FontSize']);
        Title = FileStruct['component'] + ' [' + FileStruct['File'] + '], ' + FileStruct['title'] + '\n';
        Title = Title + "I=" + FileStruct['values'].split()[0];
        Title = Title + " E=" + FileStruct['values'].split()[1];
        Title = Title + " N=" + FileStruct['values'].split()[2];
        colorbar()

    title(Title, fontsize=FileStruct['FontSize'])
    return FileStruct;

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

def read_monitor(File):
    # Read header
    isHeader = lambda line: line.startswith('#');
    Header = filter(isHeader, open(File).readlines())
    
    # Traverse header and define corresponding 'struct'
    str ="{";
    for j in range(0, len(Header)):
        # Field name and data
        Line = Header[j]; Line = Line[2:len(Line)].strip();
        Line = Line.split(':');
        Field = Line[0];
        Value = "";
        Value = string.join(string.join(Line[1:len(Line)], ':').split("'"), '');
        str = str + "'" + Field + "':'" + Value + "'";
        if j<len(Header)-1:
            str = str + ","
    str = str + "}";
    Filestruct = eval(str);
    # Add the data block:
    Filestruct['data']=loadtxt(File);
    
    return Filestruct;

def get_monitor(FS,j):
    # Ugly, hard-coded...
    data=FS['data'][:,(0,2*j+1,2*j+2)];
    vars=FS['variables'].split()   
    FSsingle={'xlimits':FS['xlimits'],'data':data,'component':vars[j+1],'values':'','type':'array_1d(100)',
              'xlabel':FS['xlabel'],'ylabel':FS['ylabel'],'File':'Scan','title':'','FontSize':6}
    return FSsingle


def click(event):
    """If the left mouse button is pressed: draw a little square. """
    tb = get_current_fig_manager().toolbar
    if event.button==1 and event.inaxes and tb.mode == '':
        g = event.inaxes
        # Determine number of clicked axis
        ax = get(gcf(),'axes')
        jused = 0
        for j in range(0, len(FSlist)):
            FS = FSlist[j]
            if g==FS['axes']:
                #                print "Match at " + str(juse) + " in array of length " + str(len(FSlist))
                h=figure(2)
                clf()
                mcplot_single(FS)
                jused = j
        FSlist[jused]['axes']=g

import matplotlib
#if sys.platform == 'darwin':
#    matplotlib.use('MacOSX');
File = sys.argv[1];
Format = 0;
if len(sys.argv)>2:
    Format = sys.argv[2];
    matplotlib.use(Format);

from numpy import loadtxt
from pylab import *
    


isBegin = lambda line: line.startswith('begin');
isCompFilename = lambda line: line.startswith('    filename:');
# First, determine if this is single or overview plot...
SimFile = filter(isBegin, open(File).readlines());
Datfile = 0;
if SimFile == []:
    FS = read_monitor(File);
    type = FS['type'].split('(')[0].strip();
    if type!='multiarray_1d':
        FS['FontSize']=8;
        mcplot_single(FS);
        if Format!=0: 
            savefig(File + "." + Format);
            print "Saved " + File + "." + Format
        show()
        exit();
    Datfile = 1;

# Get filenames from the sim file
MonFiles = filter(isCompFilename, open(File).readlines());
L = len(MonFiles);
# Scan or oveview?
FSlist = list();
if L==0:
    if Datfile==0:
        isFilename = lambda line: line.startswith('filename');
        Scanfile = filter(isFilename, open(File).readlines()); Scanfile = Scanfile[0].split(': '); 
        Scanfile = os.path.join(os.path.dirname(File),Scanfile[1].strip()); 
        # Proceed to load scan datafile
        FS = read_monitor(Scanfile);
    L=(len(FS['variables'].split())-1)/2;
    dims = calc_panel_size(L);
    for j in range(0, L):
        FSsingle = get_monitor(FS,j);
        
        #FSlist[len(FSlist):] = [copy(FS)]
        subplot(dims[1],dims[0],j+1);
        ax=gca()
        for xlabel_i in gca().get_xticklabels():
            xlabel_i.set_fontsize(6)
        for ylabel_i in gca().get_yticklabels():
            ylabel_i.set_fontsize(6)    
        FSlist[len(FSlist):] = [FSsingle]
        FSlist[j]=mcplot_single(FSsingle);

        #FS['selected'] = j;
        #
        #FS=mcplot_single(FS);
#        connect('button_press_event',click)
else:
    dims = calc_panel_size(L);
    for j in range(0, L):
        subplot(dims[1],dims[0],j+1);
        ax=gca()
        for xlabel_i in gca().get_xticklabels():
            xlabel_i.set_fontsize(6)
        for ylabel_i in gca().get_yticklabels():
            ylabel_i.set_fontsize(6)    
        MonFile = MonFiles[j].split(':'); MonFile = MonFile[1].strip();
        FS=read_monitor(MonFile);
        FS['FontSize']=6;
        FSlist[len(FSlist):] = [FS]
        FSlist[j]=mcplot_single(FS);
connect('button_press_event',click)
if Format!=0: 
    savefig(File + "." + Format);
    print "Saved " + File + "." + Format
show();
        
    


