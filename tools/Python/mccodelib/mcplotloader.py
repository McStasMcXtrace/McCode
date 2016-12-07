'''
functionality for loading mccode data into suitable data types,
and assembling it in a plot-friendly way.
'''
import glob
import re
from os.path import isfile, isdir, join, dirname, basename, splitext
from os import walk

from .flowchart import *
from .mcplotgraph import *

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

class DataMultiHeader(DataMcCode):
    ''' "header" place holder type used in the plot graph '''
    def __str__(self):
        return 'DataMultiHeader'


''' 
Utility funcitons for loading and parsing mccode output files
'''
freetext_pat = '[\w \[\]\{\}\(\)\.\+\-\\\/\^]+'

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
        m = re.search('\# xvar: ([\w ]+)\n', text)
        data.xvar = m.group(1)
        '''# yvar: Y '''
        m = re.search('\# yvar: ([\w ]+)\n', text)
        data.yvar = m.group(1)
        
        '''# zvar: I '''
        m = re.search('\# zvar: ([\w ]+)\n', text)
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

def _parse_header(text):
    # TODO: implement
    return DataMultiHeader()

def _load_monitor(monitorfile):
    f = monitorfile
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

    if False:
        print(data.get_stats_title())

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

def _load_header(simfile):
    f = simfile
    text = open(f).read()
    return _parse_header(text)

def _load_sweep_header(simfile):
    f = simfile
    text = open(f).read()
    return _parse_header(text)

def _load_multiplot_1D_lst(f_dat):
    '''
    loads the one-dimensional 'multiplot' data sets from a mccode.dat scan sweep file,
    corresponding to each monitor as a function of the sweep parameter.
    '''
    text = open(f_dat).read()
    data_lst = []

    try:
        header = Data1D()
        header.component = ''
        header.filename = 'mcstas.dat'
        
        # NOTE: title this is overwritten below to be equal to yvar
        '''# title: Scan of lambda'''
        m = re.search('\# title: ([\w ]+)\n', text)
        header.title = m.group(1)
        
        '''# xlabel: 'lambda\''''
        m = re.search('\# xlabel: ([\w \[\]\/\^\']+)\n', text)
        header.xlabel = m.group(1).strip("\'")
        '''# ylabel: 'Intensity\''''
        m = re.search('\# ylabel: ([\w \[\]\/\^\']+)\n', text)
        header.ylabel = m.group(1).strip("\'")

        # NOTE: this only supports a single xvar
        '''# xvars: lambda'''
        m = re.search('\# xvars: ([\w]+)\n', text)
        header.xvar = m.group(1)
        '''# xlimits: 6 7'''
        m = re.search('\# xlimits: ([\d\.\-e]+) ([\d\.\-e]+)\n', text)
        header.xlimits = (float(m.group(1)), float(m.group(2)))
        
        '''# variables: lambda Ldetector_I Ldetector_ERR PSDrad_I PSDrad_ERR PSDrad_I PSDrad_ERR detector_I detector_ERR'''
        m = re.search('\# variables: ([\w ]+)\n', text)
        variables = m.group(1).split()

        # get x and y values (the latter is a list of a list, the infamous yvals_lst which contains yvals values, which are lists)
        lines = text.splitlines()
        xvals = []
        yvals_lst = []
        yvals_err_lst = []

        yvariables = variables # remember, every second "variable" is an error bar
        yvariables.pop(0) # remove xval, which for some reason is the first value
        for l in lines:
            if '#' in l:
                continue
            xvals.append(l.split()[0])

            for i in range(len(yvariables)//2):
                yvals_lst.append([])
                yvals_err_lst.append([])
                yvals_lst[i].append(l.split()[2*i+1])
                yvals_err_lst[i].append(l.split()[2*i+2])
        header.xvals = xvals

        # create a new instance for each y variable
        for i in range(len(yvariables)//2):
            data = header.clone()
            data.yvals = yvals_lst[i]
            data.yvar = yvariables[2*i]
            data.title = '%s' % (data.yvar)
            data.y_err_vals = yvals_err_lst[i]
            data_lst.append(data)

    except Exception as e:
        print(e.__str__())
        raise e

    return data_lst

def _load_sweep_monitors(rootdir):
    '''
    loads the files of a scan sweep into plotable datastructures
    '''
    def sortalpha(data):
        return sorted(data, key=lambda item: (
                                       int(item.partition(' ')[0])
                                       if item[0].isdigit() else float('inf'), item)
               )

    def walkfunc(arg, dirname, fnames):
        mnames = []
        dirsignature = (dirname, mnames)
        for f in fnames:
            # NOTE: this will attempt to load all files except for mccode.sim
            if f not in mnames and f != 'mccode.sim':
                mnames.append(f)
        arg.append(dirsignature)

    d = rootdir
    subdirtuple = []
    for root, dirs, files in walk(top=d):
        walkfunc(subdirtuple, root, files)

    del subdirtuple[0] # remove root dir

    subdirs = map(lambda t: t[0], subdirtuple)
    subdirs = sortalpha(subdirs)
    monitors = list(map(lambda t: t[1], subdirtuple))[0]

    # assemble monitor data
    sweep_monitors = []
    for m in monitors:
        mon_lst = []
        for s in subdirs:
            mon_lst.append(_load_monitor(join(s, m)))
        sweep_monitors.append(mon_lst)

    sweep_headers = []
    for m_lst in sweep_monitors:
        header = DataMultiHeader()
        header.title = m_lst[0].title
        sweep_headers.append(header)

    return sweep_monitors, sweep_headers


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
            raise Exception('Invalid input file.')
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
    if ext == '.dat' and isfile(f):
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

    # load header and monitor data
    header = _load_header(f)
    #data_lst = _load_datfiles(d)
    data_lst = _load_data_from_mcfiles(_get_filenames_from_mccodesim(join(d, 'mccode.sim')))

    # construct two-level plot graph
    root = PNMultiple(header, data_lst)
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

    # load primary data, 1D sweep values
    datalst_sweep1D = _load_multiplot_1D_lst(f_dat)
    header = _load_sweep_header(f_sim)
    root = PNMultiple(header, datalst_sweep1D)

    # primary nodes (zoom on 1D sweep values)
    primnodes_lst = []
    for data in datalst_sweep1D:
        primnode = PNSingle(data)
        primnodes_lst.append(primnode)
    root.set_primaries(primnodes_lst)

    # load secondary data, sweep data points organized by monitor
    monitors, headers = _load_sweep_monitors(d)

    # secondary modes (individual sweep monitors)
    secnodes_lst = []
    for i in range(len(headers)):
        m_lst = monitors[i]
        header = headers[i]
        secnode = PNMultiple(header, m_lst)
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
    root = PNMultiple(DataMultiHeader(), data_lst)
    primnodes = []
    for data in data_lst:
        node = PNSingle(data)
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
        
        self.simfile = join(args['directory'],args['simfile'])
