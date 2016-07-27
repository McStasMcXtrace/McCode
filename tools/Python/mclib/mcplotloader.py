'''
functionality for loading mccode data into suitable data types, 
and assembling it in a plot-friendly way.
'''
import glob
import re
from os.path import isfile, isdir, join, dirname, basename, walk, splitext, walk

from flowchart import *
from mcplotgraph import *

''' 
Flowchart decision functions.

NOTE: These functions probe disk contents, but they do not open any files, so 
all decisions are based on file exists, folder exists and filename.
Any errors are therefore encountered only at the actual load data step.

They assume that args['simfilee'] is a string that was input by the user (possibly empty).

They are implemented in a context-dependent way! Each one
can have assumptions in its implementation depending on its position in the flow chart.

Also note the use of implicit data keys, 'monitorfile' etc. which are assumed to exist by the
data loader terminal functions.
'''
def has_filename(args):
    f = args['simfile']
    if not isfile(f):
        if not isdir(f):
            raise Exception('mcplot loader: Invalid input file.')
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
    raise Exception('is_sweep_data_present has not been not implemented.')

def is_mccodesim_w_monitors(args):
    f = args['simfile']
    d = args['directory']
    # checks mccode.sim existence
    if not isfile(join(d, 'mccode.sim')):
        return False
    else:
        f = join(d, 'mccode.sim')
        args['simfile'] = f
    # look for .dat files
    datfiles = glob.glob(join(d, '*.dat'))
    return len(datfiles) > 0

def has_datfile(args):
    d = args['directory']
    datfiles = glob.glob(join(d, '*.dat'))
    if len(datfiles) > 0:
        args['monitorfile'] = datfiles[0]
        return True
    else:
        return False

def has_multiple_datfiles(args):
    d = args['directory']
    datfiles = glob.glob(join(d, '*.dat'))
    return len(datfiles) > 1

def test_decfuncs(simfile):
    args = {}
    args['simfile'] = simfile
    
    print 'has_filename:              %s' % str(has_filename(args))
    print 'is_mccodesim_or_mccodedat: %s' % str(is_mccodesim_or_mccodedat(args))
    print 'is_monitorfile:            %s' % str(is_monitorfile(args))
    print 'is_sweepfolder:            %s' % str(is_sweepfolder(args))
    print 'is_broken_sweepfolder:     %s' % str(is_broken_sweepfolder(args))
    #print 'is_sweep_data_present:     %s' % str(is_sweep_data_present(args))
    print 'is_mccodesim_w_monitors:   %s' % str(is_mccodesim_w_monitors(args))
    print 'has_datfile:               %s' % str(has_datfile(args))
    print 'has_multiple_datfiles:     %s' % str(has_multiple_datfiles(args))

'''
Utility funcitons for loading and parsing mccode output data files 
'''
def _parse_1D_monitor(text):
    data = Data1D()
    data.load(text)
    return data

def _parse_2D_monitor(text):
    # TODO: implement
    return Data2D()

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
    
    return data

def _load_datfiles(directory):
    d = directory
    datfiles = glob.glob(join(d, '*.dat'))
    data_lst = []
    for f in datfiles:
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

def _load_multiplot_1D_lst(f_sim):
    '''
    loads the one-dimensional 'multiplot' data sets from a mccode.sim scan sweep file, 
    corresponding to each monitor as a function of the sweep parameter.
    '''
    # TODO: implement
    lst = []
    lst.append(Data1D())
    lst.append(Data1D())
    lst.append(Data1D())
    
    return lst

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
            if splitext(f)[1] == '.dat':
                if f not in mnames: 
                    mnames.append(f)
        arg.append(dirsignature)
    
    d = rootdir
    subdirtuple = []
    walk(top=d, func=walkfunc, arg=subdirtuple)
    del subdirtuple[0] # remove root dir
    
    subdirs = map(lambda t: t[0], subdirtuple)
    subdirs = sortalpha(subdirs)
    monitors = map(lambda t: t[1], subdirtuple)[0]
    
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
Terminal load data functions. 

Loads data files, then assembles and returns a data graph, which can be plotted.
'''
def load_monitor(args):
    # assume monitorfile is present and exists
    data = _load_monitor(args['monitorfile'])

    # plot graph only has one node in this case
    root = PNSingle(data=data)
    
    return root

def load_simulation(args):
    # assume simfile is mccode.sim
    f = args['simfile']
    d = args['directory']

    # load header and monitor data
    header = _load_header(f)
    data_lst = _load_datfiles(d)
    
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
    datalst_sweep1D = _load_multiplot_1D_lst(f_sim)
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
    d = args['simfile']
    
    # load monitor files into a list 
    data_lst = _load_datfiles(d)
    
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
    raise Exception('error terminal reached: %s' % args['simfile'])

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

