'''
functionality for loading mccode data into suitable data types,
and assembling it in a plot-friendly way.
'''
import glob
import re
from os.path import isfile, isdir, join, dirname, basename, splitext
from os import walk

from .flowchart import *

'''
Plot graph node types have parent, primaries and secondaries, corresponding to whether 
"back", "click" or "ctr-click" is used to navigate.
Descendents also have a data pointer, which is an instance or a list.
'''
class PlotNode(object):
    ''' 
    Base class for plot graph nodes. 
    Parent is set implicitly on "primary" and "secondary" child node lists.
    '''
    def __init__(self):
        self.parent = None
        self.primaries = []
        self.secondaries = []
    def set_primaries(self, node_lst):
        self.primaries = node_lst
        for node in node_lst:
            node.parent = self
    def get_primaries(self):
        return self.primaries
    def set_secondaries(self, node_lst):
        self.secondaries = node_lst
        for node in node_lst: 
            node.parent = self
    def get_secondaries(self):
        return self.secondaries
    def get_parent(self):
        return self.parent
    def getdata_lst(self):
        ''' getdata always returns a list '''

class PNMultiple(PlotNode):
    def __init__(self, data_handle_lst):
        self._data_handle_lst = data_handle_lst
        super().__init__()
    def getdata_lst(self):
        return [d.getdata() for d in self._data_handle_lst]
    def __str__(self):
        return 'PNMultiple'

class PNSingle(PlotNode):
    def __init__(self, data_handle):
        self._data_handle = data_handle
        super().__init__()
    def getdata_lst(self):
        return [self._data_handle.getdata()]
    def __str__(self):
        return 'PNSingle'

class DataHandle():
    ''' 
    proxy object for singular deferred loading
    
    Do not set "data=..." unless you have a good reason.
    '''
    def __init__(self, load_fct, data=None):
        self._load_fct = load_fct
        self._data = data
    def getdata(self):
        if not self._data:
            self._data = self._load_fct()
        return self._data

class PlotGraphPrint(object):
    ''' NOTE: iteration logics not yet implemented '''
    def __init__(self, rootnode, indent_str='    '):
        if indent_str == '' or type(indent_str) != str:
            raise Exception('PlotGraphPrint: indent_str must be a non-empty string.')
        self.indent_str = indent_str
        self.root = rootnode
        self.printed_ids = []
        # execute
        self.print_recurse(self.root, level=0)
        
    def print_recurse(self, node, level):
        ''' node print recursion '''
        self.printnode(node, level)
        children = node.primaries + node.secondaries
        for c in children:
            self.print_recurse( c, level+1)                

    def printnode(self, node, level=0):
        ''' 
        Prints the node id, its children id's and data reference, respecting indent and
        using self.indent_str.
        '''
        
        # only print nodes once
        if id(node) in self.printed_ids:
            return
        
        indent = self.indent_str
        
        # print the node
        print()
        print(indent*(level+0) + '%s (%d):' % (node, id(node)))
        
        if node.parent:
            print(indent*(level+1) + 'parent:')
            print(indent*(level+2) + '%s (%d)' % (node.parent, id(node.parent)))
        
        print(indent*(level+1) + 'data objects:')
        for d in node.getdata_lst():
            print(indent*(level+2) + '%s (%d)' % (d, id(d)))
        
        if not len(node.primaries) == 0:
            print(indent*(level+1) + 'primary children:')
            for p in node.primaries:
                print(indent*(level+2) + '%s (%d)' % (p, id(p)))
        
        if not len(node.secondaries) == 0:
            print(indent*(level+1) + 'secondary children:')
            for s in node.secondaries:
                print(indent*(level+2) + '%s (%d)' % (s, id(s)))
        
        self.printed_ids.append(id(node))


'''
McCode simulation output data types.
'''
class DataMcCode(object):
    ''' base type holding only the data object's title '''
    def __init__(self, *args, **kwargs):
        self.title = ''

    def __str__(self, *args, **kwargs):
        return self.title

class Data1D(DataMcCode):
    ''' 1d plots use this data type '''
    def __init__(self):
        super(Data1D, self).__init__()
        
        self.component = ''
        self.filename = ''
        self.title = ''
        self.xlabel = ''
        self.ylabel = ''
        
        self.xvar = ''
        self.xlimits = () # pair
        
        self.variables = []
        
        self.yvar = () # pair
        self.values = () # triplet
        self.statistics = ''
        
        # data references
        self.xvals = []
        self.yvals = []
        self.y_err_vals = []
        self.Nvals = []
    
    def clone(self):
        data = Data1D()
        
        data.component = self.component
        data.filename = self.filename
        data.title = self.title
        data.xlabel = self.xlabel
        data.ylabel = self.ylabel
        
        data.xvar = self.xvar
        data.xlimits = self.xlimits
        
        data.variables = self.variables
        
        data.yvar = self.yvar
        data.values = self.values
        data.statistics = self.statistics
        
        # data references
        data.xvals = self.xvals
        data.yvals = self.yvals
        data.y_err_vals = self.y_err_vals
        data.Nvals = self.Nvals
        
        return data
        
    def get_stats_title(self):
        '''I=.... Err=... N=...; X0=...; dX=...;'''
        try:
            stitle = '%s=%e Err=%e N=%d; %s' % (self.yvar[0], self.values[0], self.values[1], self.values[2], self.statistics)
        except:
            stitle = '%s of %s' % (self.yvar[0], self.xvar)
        return stitle
    
    def __str__(self):
        return 'Data1D, ' + self.get_stats_title()

class Data2D(DataMcCode):
    ''' PSD data type '''
    def __init__(self):
        super(Data2D, self).__init__()
        
        self.component = ''
        self.filename = ''
        self.title = ''
        
        self.xlabel = ''
        self.ylabel = ''
        
        self.xvar = ''
        self.yvar = ''
        self.zvar = ''
        self.xylimits = () # quadruple
        
        self.values = () # triplet
        self.statistics = '' # quadruple
        self.signal = ''
        
        # data references
        self.zvals = []
        self.counts = []
    
    def get_stats_title(self):
        '''I=.... Err=... N=...; X0=...; dX=...;'''
        stitle = '%s=%e Err=%e N=%d' % (self.zvar, self.values[0], self.values[1], self.values[2])
        return stitle
    
    def __str__(self):
        return 'Data2D, ' + self.get_stats_title()


''' 
Utility funcitons for loading and parsing mccode output files
'''
freetext_pat = '[\w \[\]\{\}\(\)\.\+\-\\\/\^\~]+'

def _parse_1D_monitor(text):
    ''' populates data fields of new Data1D object using the text from a mccode data file '''
    data = Data1D()

    try:
        # load essential header data
        '''# component: Ldetector'''
        m = re.search('\# component: ([\w]+)\n', text)
        data.component = m.group(1)
        '''# filename: Edet.dat'''
        m = re.search('\# filename: ([\w\.]+)\n', text)
        data.filename = m.group(1)
        '''# title: Wavelength monitor'''
        m = re.search('\# title: (%s)\n' % freetext_pat, text)
        data.title = m.group(1)
        '''# xlabel: Wavelength [AA]'''
        m = re.search('\# xlabel: (%s)\n' % freetext_pat, text)
        data.xlabel = m.group(1)
        '''# ylabel: Intensity'''
        m = re.search('\# ylabel: (%s)\n' % freetext_pat, text)
        data.ylabel = m.group(1)

        '''# xvar: L'''
        m = re.search('\# xvar: ([\w]+)\n', text)
        data.xvar = m.group(1)
        '''# xlimits: 5.5 6.5'''
        m = re.search('\# xlimits: ([\d\.\-e]+) ([\d\.\-e]+)\n', text)
        data.xlimits = (float(m.group(1)), float(m.group(2)))

        '''# yvar: (I,I_err)'''
        m = re.search('\# yvar: \(([\w]+),([\w]+)\)\n', text)
        data.yvar = (m.group(1), m.group(2))
        
        '''# values: 6.72365e-17 4.07766e-18 4750'''
        m = re.search('\# values: ([\d\-\+\.e]+) ([\d\-\+\.e]+) ([\d\-\+\.e]+)\n', text)
        data.values = (float(m.group(1)), float(m.group(2)), float(m.group(3)))
        '''# statistics: X0=5.99569; dX=0.0266368;'''
        m = re.search('\# statistics: X0=([\d\.\-e]+); dX=([\d\.\-e]+);\n', text)
        data.statistics = 'X0=%f; dX=%f;' % (float(m.group(1)), float(m.group(2)))

        # load the actual data
        lines = text.splitlines()
        xvals = []
        yvals = []
        y_err_vals = []
        Nvals = []
        for l in lines:
            if '#' in l:
                continue

            vals = l.split()
            xvals.append(float(vals[0]))
            yvals.append(float(vals[1]))
            y_err_vals.append(float(vals[2]))
            Nvals.append(float(vals[3]))

        data.xvals = xvals
        data.yvals = yvals
        data.y_err_vals = y_err_vals
        data.Nvals = Nvals

    except Exception as e:
        print('Data1D load error.')
        raise e

    return data

def _parse_2D_monitor(text):
    data = Data2D()

    ''' populates data fields using the text from a mccode data file '''
    try:
        # load essential header data
        '''# component: detector'''
        m = re.search('\# component: ([\w]+)\n', text)
        data.component = m.group(1)
        '''# filename: PSD.dat'''
        m = re.search('\# filename: ([\w\.]+)\n', text)
        data.filename = m.group(1)
        '''# title: PSD monitor'''
        m = re.search('\# title: (%s)\n' % freetext_pat, text)
        data.title = m.group(1)

        '''# xlabel: X position [cm]'''
        m = re.search('\# xlabel: (%s)\n' % freetext_pat, text)
        data.xlabel = m.group(1)
        '''# ylabel: Y position [cm]'''
        m = re.search('\# ylabel: (%s)\n' % freetext_pat, text)
        data.ylabel = m.group(1)

        '''# xvar: X'''
        m = re.search('\# yvar: (%s)\n' % freetext_pat, text)
        data.xvar = m.group(1)
        '''# yvar: Y '''
        m = re.search('\# yvar: (%s)\n' % freetext_pat, text)
        data.yvar = m.group(1)
        
        '''# zvar: I '''
        m = re.search('\# zvar: (%s)\n' % freetext_pat, text)
        data.zvar = m.group(1)
        '''# xylimits: -30 30 -30 30'''
        m = re.search('\# xylimits: ([\d\.\-e]+) ([\d\.\-e]+) ([\d\.\-e]+) ([\d\.\-e]+)\n', text)
        data.xlimits = (float(m.group(1)), float(m.group(2)), float(m.group(3)), float(m.group(4)))

        '''# values: 6.72365e-17 4.07766e-18 4750'''
        m = re.search('\# values: ([\d\+\-\.e]+) ([\d\+\-\.e]+) ([\d\+\-\.e]+)\n', text)
        data.values = (float(m.group(1)), float(m.group(2)), float(m.group(3)))
        '''# statistics: X0=5.99569; dX=0.0266368;'''
        m = re.search('\# statistics: X0=([\d\.\+\-e]+); dX=([\d\.\+\-e]+); Y0=([\d\.\+\-e]+); dY=([\d\.\+\-e]+);\n', text)
        data.statistics = 'X0=%f; dX=%f; Y0=%f; dY=%f;' % (float(m.group(1)), float(m.group(2)), float(m.group(3)), float(m.group(4)))
        '''# signal: Min=0; Max=1.20439e-18; Mean=4.10394e-21;'''
        m = re.search('\# signal: Min=([\d\.\+\-e]+); Max=([\d\.\+\-e]+); Mean=([\d\.\+\-e]+);\n', text)
        data.signal = 'Min=%f; Max=%f; Mean=%f;' % (float(m.group(1)), float(m.group(2)), float(m.group(3)))

        '''# Data [detector/PSD.dat] I:'''
        '''# Events [detector/PSD.dat] N:'''
        lines = text.splitlines()
        dat = False
        events = False
        for l in lines:
            if '# Data ' in l:
                dat = True
                continue

            if '# Events ' in l:
                dat = False
                events = True
                continue

            if '# Errors ' in l:
                # NOTE: error values are not loaded
                dat = False
                events = False
                continue

            if dat:
                try:
                    vals = [float(item) for item in l.strip().split()]
                    data.zvals.append(vals)
                except:
                    pass
            if events:
                try:
                    vals = [float(item) for item in l.strip().split()]
                    data.counts.append(vals)
                except:
                    pass

    except Exception as e:
        print('Data2D load error.')
        raise e

    return data

def _load_monitor(monitorfile):
    ''' deferred loading: returns a data handle, which the user must call getdata() on to load the actual data '''
    def load(monfile):
        f = monfile
        text = open(f).read()
        # determine 1D / 2D data
        
        m = re.search('\# type: (array_\wd)', text)
        typ = m.group(1)
        if typ == 'array_1d':
            data = _parse_1D_monitor(text)
        elif typ == 'array_2d':
            data = _parse_2D_monitor(text)
        else:
            raise Exception('load_monitor: unknown data format.')
        return data
    
    data = DataHandle(load_fct=lambda m=monitorfile: load(monfile=m))
    
    return data

def _get_filenames_from_mccodesim(mccodesim):
    dir = dirname(mccodesim)
    
    text = open(mccodesim).read()
    data_idx = text.find('begin data')
    filenames = []
    for line in text[data_idx:].splitlines():
        m = re.search(r'filename: ([\w\._\-+]+)\s*', line)
        if m: 
            filenames.append(join(dir, m.group(1)))
    return filenames

def _load_data_from_mcfiles(filenames):
    data_lst = []
    for f in filenames:
        data = _load_monitor(f)
        data_lst.append(data)
    return data_lst

def _load_multiplot_1D_lst(f_dat):
    '''
    loads the one-dimensional 'multiplot' data sets from a mccode.dat scan sweep file,
    corresponding to each monitor as a function of the sweep parameter.
    '''
    text = open(f_dat).read()
    data_handle_lst = []

    try:
        header = Data1D()
        header.component = ''
        header.filename = 'mcstas.dat'
        
        # NOTE: title this is overwritten below to be equal to yvar
        '''# title: Scan of lambda'''
        m = re.search('\# title: ([\w, ]+)\n', text)
        header.title = m.group(1)
        
        '''# xlabel: 'lambda\''''
        m = re.search('\# xlabel: ([\w \[\]\/\^\',]+)\n', text)
        header.xlabel = m.group(1).strip("\'")
        '''# ylabel: 'Intensity\''''
        m = re.search('\# ylabel: ([\w \[\]\/\^\',]+)\n', text)
        header.ylabel = m.group(1).strip("\'")

        # NOTE: this only supports a single xvar
        '''# xvars: lambda'''
        m = re.search('\# xvars: ([\w, ]+)\n', text)
        header.xvar = m.group(1).replace(',', '')
        num_xvars = len(header.xvar.split())
        
        '''# xlimits: 6 7'''
        m = re.search('\# xlimits: ([\d\.\-e]+) ([\d\.\-e]+)\n', text)
        header.xlimits = (float(m.group(1)), float(m.group(2)))
        
        '''# variables: lambda Ldetector_I Ldetector_ERR PSDrad_I PSDrad_ERR PSDrad_I PSDrad_ERR detector_I detector_ERR'''
        m = re.search('\# variables: ([\w ]+)\n', text)
        variables = m.group(1).split()
        
        '''# yvars: (AutoTOFL0_I,AutoTOFL0_ERR) (AutoTOF0_I,AutoTOF0_ERR) (AutoL0_I,AutoL0_ERR) ...'''
        m = re.search('\# yvars: ([\w \(\)\,]+)\n', text)
        unsplit = m.group(1)
        unsplit = unsplit.replace('(', ' ')
        unsplit = unsplit.replace(')', ' ')
        unsplit = unsplit.replace(',', ' ')
        yvars = unsplit.split()
        
        # get x and y values (the latter is a list of a list, the infamous yvals_lst which contains yvals values, which are lists)
        lines = text.splitlines()
        xvals = []
        yvals_lst = []
        yvals_err_lst = []
        
        for l in lines:
            if '#' in l:
                continue
            xvals.append(l.split()[0])

            for i in range(len(yvars)//2):
                yvals_lst.append([])
                yvals_err_lst.append([])
                yvals_lst[i].append(l.split()[2*i+num_xvars])
                yvals_err_lst[i].append(l.split()[2*i+num_xvars+1])
        header.xvals = xvals
        
        # create a new instance for each y variable
        for i in range(len(yvars)//2):
            data = header.clone()
            data.yvals = yvals_lst[i]
            data.yvar = yvars[2*i]
            data.title = '%s' % (data.yvar)
            data.y_err_vals = yvals_err_lst[i]
            data_handle_lst.append(DataHandle(load_fct=None, data=data))

    except Exception as e:
        print(e.__str__())
        raise e

    return data_handle_lst

def _load_sweep_monitors(rootdir):
    '''
    loads the files of a scan sweep into plotable datastructures
    '''
    def walkfunc(arg, dirname, fnames):
        mnames = []
        dirsignature = (dirname, mnames)
        for f in fnames:
            # NOTE: this will attempt to load all files except for mccode.sim
            if f not in mnames and f != 'mccode.sim':
                mnames.append(f)
        arg.append(dirsignature)
    
    # get the subdirs somehow
    subdirtuple = []
    for root, dirs, files in walk(top=rootdir):
        walkfunc(subdirtuple, root, files)
    del subdirtuple[0] # remove root dir
    subdirs = [t[0] for t in subdirtuple]
    # get the right order of subdirs by recreating them a little bit
    subdirs = [join(dirname(subdirs[i]), str(i)) for i in range(len(subdirs))] # sortalpha(subdirs)
    
    # get the monitor ordering right by snooping the '  filename:' labels out of the scan point file 0/mccode.sim
    def get_subdir_monitors(subdir):
        mons = []
        f = open(join(subdir, 'mccode.sim'), 'rb')
        line = f.readline().decode()
        while line:
            line = f.readline().decode()
            m = re.match('  filename:\s+([\w\.]+)\s*\n', line)
            if m:
                mons.append(join(subdir, m.group(1)))
        return mons
   
    monitors_by_subdir = []
    for s in subdirs:
        monitors_by_subdir.append(get_subdir_monitors(s))
    
    # notice that columns and rows are swapped, so we get to use a list-of-lists data structure, with rows the same monitor
    sweep_monitors = [None]*len(monitors_by_subdir[0])
    for i in range(len(monitors_by_subdir[0])):          # N
        mon_lst = [None]*len(monitors_by_subdir)
        for j in range(len(monitors_by_subdir)):
            mon_lst[j] = _load_monitor(monitors_by_subdir[j][i])
        sweep_monitors[i] = mon_lst
    
    return sweep_monitors


'''
Flowchart functions for decision and terminal nodes (there are no process nodes in the implemented chart, 
although a cleaner implementation might include some).

Decision (bool) functions only probe disk contents. Terminal functions will load and interprit data files.
Therefore, any load errors are encountered at the terminal nodes' load data step.

NOTE: Functions can be context-dependent, according to the corresponding node position.
The function has_filename must be called first (see test_decfuncs).
'''
def has_filename(args):
    f = args['simfile']
    if not isfile(f):
        if not isdir(f):
            if not f == '':
                raise Exception('Invalid input file.')
            else:
                f = '.'
        args['directory'] = f
        args['simfile'] = ''
        return False
    else:
        args['directory'] = dirname(f)
        return True

def is_mccodesim_or_mccodedat(args):
    f = args['simfile']
    f_name = basename(f)
    return (f_name == 'mccode.sim' or f_name == 'mccode.dat') and isfile(f)

def is_monitorfile(args):
    f = args['simfile']
    ext = splitext(f)[1]
    if isfile(f):
        args['monitorfile'] = f
        return True
    else:
        return False

def is_sweepfolder(args):
    d = args['directory']

    if not isdir(d):
        return False
    dotsim = join(d, 'mccode.sim')
    dotdat = join(d, 'mccode.dat')
    return isfile(dotsim) and isfile(dotdat)

def is_broken_sweepfolder(args):
    ''' not implemented (returns trivial answer) '''
    return False

def is_sweep_data_present(args):
    ''' not implemented '''
    raise Exception('is_sweep_data_present has not been implemented.')

def is_mccodesim_w_monitors(args):
    f = args['simfile']
    d = args['directory']
    # checks mccode.sim existence
    if not isfile(join(d, 'mccode.sim')):
        return False
    else:
        f = join(d, 'mccode.sim')
        args['simfile'] = f
    
    # look for any "unkonwn" files, could be data files
    datfiles = glob.glob(join(d, '*'))
    if 'mccode.sim' in datfiles: 
        datfiles.remove('mccode.sim')
    if 'mccode.dat' in datfiles: 
        datfiles.remove('mccode.dat')
    return len(datfiles) > 0

def has_datfile(args):
    d = args['directory']
    
    # look for any "unkonwn" files, could be data files
    datfiles = glob.glob(join(d, '*'))
    if 'mccode.sim' in datfiles: 
        datfiles.remove('mccode.sim')
    if 'mccode.dat' in datfiles: 
        datfiles.remove('mccode.dat')
    if len(datfiles) > 0:
        args['monitorfile'] = datfiles[0]
        return True
    else:
        return False

def has_multiple_datfiles(args):
    d = args['directory']
    
    # look for any "unkonwn" files, could be data files
    datfiles = glob.glob(join(d, '*'))
    if 'mccode.sim' in datfiles: 
        datfiles.remove('mccode.sim')
    if 'mccode.dat' in datfiles: 
        datfiles.remove('mccode.dat')
    return len(datfiles) > 1

def test_decfuncs(simfile):
    ''' calls all decision functions in the node tree '''
    args = {}
    args['simfile'] = simfile

    print('has_filename:              %s' % str(has_filename(args)))
    print('is_mccodesim_or_mccodedat: %s' % str(is_mccodesim_or_mccodedat(args)))
    print('is_monitorfile:            %s' % str(is_monitorfile(args)))
    print('is_sweepfolder:            %s' % str(is_sweepfolder(args)))
    print('is_broken_sweepfolder:     %s' % str(is_broken_sweepfolder(args)))
    #print('is_sweep_data_present:     %s' % str(is_sweep_data_present(args))) # should not be called until implemented
    print('is_mccodesim_w_monitors:   %s' % str(is_mccodesim_w_monitors(args)))
    print('has_datfile:               %s' % str(has_datfile(args)))
    print('has_multiple_datfiles:     %s' % str(has_multiple_datfiles(args)))

'''
Terminal load functions - calls data load utilities, assembles and returns data graph.
'''
def load_monitor(args):
    # assume monitorfile is present and exists
    data = _load_monitor(args['monitorfile'])

    # plot graph only has one node in this case
    root = PNSingle(data)

    return root

def load_simulation(args):
    # assume simfile is mccode.sim
    f = args['simfile']
    d = args['directory']

    # load monitor data handles
    data_lst = _load_data_from_mcfiles(_get_filenames_from_mccodesim(join(d, 'mccode.sim')))

    # construct two-level plot graph
    root = PNMultiple(data_lst)
    primnodes = []
    for data in data_lst:
        node = PNSingle(data)
        primnodes.append(node)
    root.set_primaries(primnodes)
    root.set_secondaries(primnodes) # there is only one way to click here...could also be None

    return root

def load_sweep(args):
    d = args['directory']
    f_sim = join(d, 'mccode.sim')
    f_dat = join(d, 'mccode.dat')

    # load primary data_handle, 1D sweep values
    data_handle_lst_sweep1D = _load_multiplot_1D_lst(f_dat)
    root = PNMultiple(data_handle_lst_sweep1D)

    # primary nodes (zoom on 1D sweep values)
    primnodes_lst = []
    for data_handle in data_handle_lst_sweep1D:
        primnode = PNSingle(data_handle)
        primnodes_lst.append(primnode)
    root.set_primaries(primnodes_lst)

    # load secondary data_handle, sweep data_handle points organized by monitor
    monitors = _load_sweep_monitors(d)

    # secondary modes (individual sweep monitors)
    secnodes_lst = []
    for i in range(len(monitors)):
        m_lst = monitors[i]
        secnode = PNMultiple(m_lst)
        children = []
        for m in m_lst:
            child = PNSingle(m)
            children.append(child)
        secnode.set_primaries(children)
        secnode.set_secondaries(children)
        secnodes_lst.append(secnode)
    root.set_secondaries(secnodes_lst)

    return root

def load_sweep_b(args):
    raise Exception('load_sweep_b is not implemented.')

def load_sweep_c(args):
    raise Exception('load_sweep_c is not implemented.')

def load_monitor_folder(args):
    # assume simfile is folder with multiple dat files
    d = args['directory']

    # load monitor files into a list
    datfiles = glob.glob(join(d, '*'))
    data_lst = _load_data_from_mcfiles(datfiles)

    # construct two-level plot graph
    root = PNMultiple(data_lst)
    primnodes = []
    for data in data_lst:
        node = PNSingle()
        node.setdata(data)
        primnodes.append(node)
    root.set_primaries(primnodes)
    root.set_secondaries(primnodes)

    return root

def throw_error(args):
    raise Exception('Could not load file "%s".' % args['simfile'])

class McPlotDataLoader():
    ''' assembly and execution of mccode data loader flowchart '''
    def __init__(self, simfile):
        '''  '''
        self.simfile = simfile
        self.plot_graph = None

    def load(self):
        ''' loads mccode data and assembles the plotable data graph '''

        # exit terminals
        exit_error                  = FCNTerminal(key="error",  fct=throw_error)
        exit_case1                  = FCNTerminal(key="case1",  fct=load_monitor)
        exit_case2                  = FCNTerminal(key="case2",  fct=load_simulation)
        exit_case3                  = FCNTerminal(key="case3",  fct=load_sweep)
        exit_case3b                 = FCNTerminal(key="case3b", fct=throw_error)
        exit_case3c                 = FCNTerminal(key="case3c", fct=throw_error)
        exit_case4                  = FCNTerminal(key="case4",  fct=load_monitor_folder)
        # decision nodes (assembled in backwards order)
        dec_multiplefiles           = FCNDecisionBool(fct=has_multiple_datfiles,    node_T=exit_case4,              node_F=exit_case1)
        dec_hasdatfile              = FCNDecisionBool(fct=has_datfile,              node_T=dec_multiplefiles,       node_F=exit_error)
        dec_ismccodesimwmonitors    = FCNDecisionBool(fct=is_mccodesim_w_monitors,  node_T=exit_case2,              node_F=dec_hasdatfile)
        dec_datafolderspresent      = FCNDecisionBool(fct=is_sweep_data_present,    node_T=exit_case3b,             node_F=exit_case3c)
        dec_isbrokensweep           = FCNDecisionBool(fct=is_broken_sweepfolder,    node_T=dec_datafolderspresent,  node_F=dec_ismccodesimwmonitors)
        dec_issweepfolder           = FCNDecisionBool(fct=is_sweepfolder,           node_T=exit_case3,              node_F=dec_isbrokensweep)
        dec_ismonitor               = FCNDecisionBool(fct=is_monitorfile,           node_T=exit_case1,              node_F=exit_error)
        dec_ismccodesimordat        = FCNDecisionBool(fct=is_mccodesim_or_mccodedat, node_T=dec_issweepfolder,      node_F=dec_ismonitor)
        dec_hasfilename             = FCNDecisionBool(fct=has_filename,             node_T=dec_ismccodesimordat,    node_F=dec_issweepfolder)
        # enter terminal node
        enter_simfile               = FCNTerminal(key='enter', node_next=dec_hasfilename)

        # get the "args" which is just the simfile, a string, which may correspond to a file or a folder
        args = {}
        args['simfile'] = self.simfile

        # traverse the flow chart
        control = FlowChartControl(terminal_enter=enter_simfile)
        exit_node = control.process(args=args)

        self.plot_graph = exit_node.result
        
        self.directory = args['directory']
