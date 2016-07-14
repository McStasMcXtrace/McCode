'''
functionality for loading mccode data into suitable data types, 
and assembling it in a plot-friendly way.
'''
import glob
import re
from os.path import isfile, isdir, join, dirname, basename, walk, splitext

from flowchart import *
from mcplotgraph import *

'''
McCode sim output data types, pythonified.
'''
def load1DMonitor(text):
    '''  '''
    return Data1D()

def load2DMonitor(text):
    '''  '''
    return Data2D()

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
    return isfile(f)

def is_mccodesim_or_mccodedat(args):
    f = basename(args['simfile'])
    return (f == 'mccode.sim' or f == 'mccode.dat') and isfile(f)

def is_monitorfile(args):
    f = args['simfile']
    ext = splitext(f)[1]
    if ext == '.dat' and isfile(f):
        args['monitorfile'] = f
        return True
    else:
        return False

def is_sweepfolder(args):
    folder = args['simfile']
    if not isdir(folder):
        return False
    dotsim = join(folder, 'mccode.sim')
    dotdat = join(folder, 'mccode.dat')
    return isfile(dotsim) and isfile(dotdat)

def is_broken_sweepfolder(args):
    ''' not implemented (returns trivial answer) '''
    return False

def is_sweep_data_present(args):
    ''' not implemented '''
    raise Exception('is_sweep_data_present has not been not implemented.')

def is_mccodesim_w_monitors(args):
    f = args['simfile']
    # cover cases where f is a folder without forward slash, in which case dirname returns an empty string
    if not isfile(f):
        if not f == '':
            f = f + '/'
    # checks mccode.sim existence
    if not isfile(join(f, 'mccode.sim')):
        return False
    # assume f is mccode.sim
    d = dirname(f)
    datfiles = glob.glob(join(d, '*.dat'))
    return len(datfiles) > 0

def has_datfile(args):
    # assume folder
    d = args['simfile']
    datfiles = glob.glob(join(d, '*.dat'))
    args['monitorfile'] = datfiles[0]
    return len(datfiles) > 0

def has_multiple_datfiles(args):
    # assume folder
    d = args['simfile']
    datfiles = glob.glob(join(d, '*.dat'))
    return len(datfiles) > 1

def test_decfuncs(simfile):
    args = {}
    args['simfile'] = simfile
    
    print 'has_filename:              %s' % str(has_filename(args))
    print 'is_mccodesim_or_mccodedat: %s' % str(is_mccodesim_or_mccodedat(args))
    print 'is_monitorfile:            %s' % str(is_monitorfile(args))
    print 'is_sweepfolder:            %s' % str(is_sweepfolder(args))
    #print 'is_broken_sweepfolder:     %s' % str(is_broken_sweepfolder(args))
    #print 'is_sweep_data_present:     %s' % str(is_sweep_data_present(args))
    print 'is_mccodesim_w_monitors:   %s' % str(is_mccodesim_w_monitors(args))
    print 'has_datfile:               %s' % str(has_datfile(args))
    print 'has_multiple_datfiles:     %s' % str(has_multiple_datfiles(args))

''' 
Terminal load data functions. 

Loads data files, then assembles and returns a data graph, which can be plotted.
'''
def load_monitor(args):
    # assume monitorfile is present and exists
    f = args['monitorfile']
    text = f.open().read()
    # determine 1D / 2D data
    
    m = re.search('\# type: (\w)', text)
    typ = m.group(1)
    if typ == 'array_1d':
        data = load1DMonitor(text)
    elif typ == 'array_2d':
        data = load2DMonitor(text)
    else:
        raise Exception('load_monitor: unknown data format.')
    
    # TODO: assemble and return the plot-graph

def load_simulation(args):
    # assume simfile is mccode.sim
    f = args['simfile']
    pass

def load_sweep(args):
    # assume simfile is mccode.sim with mccode.dat
    f = args['simfile']
    f_dat = join(splitext(f)[0], 'dat')
    
    # TODO: implement

def load_sweep_b(args):
    raise Exception('load_sweep_b is not implemented.')

def load_sweep_c(args):
    raise Exception('load_sweep_c is not implemented.')

def load_monitor_folder(args):
    # assume simfile is folder with multiple dat files
    d = args['simfile']
    
    # TODO: implement

def throw_error(args):
    raise Exception('error terminal reached: %s' % args['simfile'])

class McPlotDataLoader():
    ''' assembly and execution of mccode data loader flowchart '''
    def __init__(self, simfile):
        '''  '''
        self.simfile = simfile
    
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
        # decision nodes assembled in backwards order
        dec_multiplefiles           = FCNDecisionBool(fct=has_multiple_datfiles,    node_T=exit_case4,              node_F=exit_case1)
        dec_hasdatfile              = FCNDecisionBool(fct=has_datfile,              node_T=dec_multiplefiles,       node_F=exit_error)
        dec_ismccodesimwmonitors    = FCNDecisionBool(fct=is_mccodesim_w_monitors,  node_T=exit_case2,              node_F=dec_hasdatfile)
        dec_datafolderspresent      = FCNDecisionBool(fct=is_sweep_data_present,    node_T=exit_case3b,             node_F=exit_case3c)
        dec_isbrokensweep           = FCNDecisionBool(fct=is_broken_sweepfolder,    node_T=dec_datafolderspresent,  node_F=dec_ismccodesimwmonitors)
        dec_issweepfolder           = FCNDecisionBool(fct=is_sweepfolder,           node_T=exit_case3,              node_F=dec_isbrokensweep)
        dec_ismonitor               = FCNDecisionBool(fct=is_monitorfile,           node_T=exit_case1,              node_F=exit_error)
        dec_ismccodesimordat        = FCNDecisionBool(fct=is_mccodesim_or_mccodedat, node_T=dec_issweepfolder,      node_F=dec_ismonitor)
        dec_hasfilename             = FCNDecisionBool(fct=has_filename,             node_T=dec_ismccodesimordat,    node_F=dec_issweepfolder)
        # enter node
        enter_simfile               = FCNTerminal(key='enter', node_next=dec_hasfilename)
        
        # get the "args" which is just the simfile, a string, which may correspond to a file or a folder
        args = {}
        args['simfile'] = self.simfile
        
        # traverse the flow chart
        control = FlowChartControl(terminal_enter=enter_simfile)
        exit_node = control.process(args=args)
        
        print
        print 'plot case flowchart exit-terminal key: %s' % exit_node.key

        # TODO: assemble data graph
        self.data_graph = None



